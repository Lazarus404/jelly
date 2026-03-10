/*
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

use super::*;
use crate::ast::Span;
use crate::ir::{BlockId, IrBlock, IrFunction, IrInsn, IrModule, IrTerminator, VRegId};
use crate::typectx::{T_F64, T_I32};

#[test]
fn bytes_concat2_is_not_commutative() {
    // Ensure CSE does not treat concat(a,b) and concat(b,a) as identical.
    let mut m = IrModule {
        types: vec![],
        sigs: vec![],
        const_i64: vec![],
        const_f64: vec![],
        const_bytes: vec![],
        atoms: vec![],
        funcs: vec![IrFunction {
            name: Some("f".to_string()),
            param_count: 0,
            cap_vregs: vec![],
            entry: BlockId(0),
            blocks: vec![IrBlock {
                label: None,
                insns: vec![
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::BytesConcat2 {
                            dst: VRegId(2),
                            a: VRegId(0),
                            b: VRegId(1),
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::BytesConcat2 {
                            dst: VRegId(3),
                            a: VRegId(1),
                            b: VRegId(0),
                        },
                    },
                ],
                term: IrTerminator::Ret { value: VRegId(2) },
            }],
            vreg_types: vec![],
        }],
        entry: 0,
    };

    let changed = common_subexpression_elimination(&mut m);
    assert!(!changed, "CSE must not fold swapped BytesConcat2");

    let f = &m.funcs[0];
    let b0 = &f.blocks[0];
    assert!(matches!(b0.insns[1].op, IrOp::BytesConcat2 { .. }));
}

#[test]
fn does_not_cse_obj_get_across_call() {
    // Without invalidation, we might fold the second ObjGetAtom into a Mov from the first,
    // which is unsound because the call may mutate the object.
    let mut m = IrModule {
        types: vec![],
        sigs: vec![],
        const_i64: vec![],
        const_f64: vec![],
        const_bytes: vec![],
        atoms: vec![],
        funcs: vec![IrFunction {
            name: Some("f".to_string()),
            param_count: 0,
            cap_vregs: vec![],
            entry: BlockId(0),
            blocks: vec![IrBlock {
                label: None,
                insns: vec![
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::ObjGetAtom {
                            dst: VRegId(1),
                            obj: VRegId(0),
                            atom_id: 7,
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::Call {
                            dst: VRegId(3),
                            callee: VRegId(2),
                            arg_base: VRegId(10),
                            nargs: 0,
                            sig_id: 0,
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::ObjGetAtom {
                            dst: VRegId(4),
                            obj: VRegId(0),
                            atom_id: 7,
                        },
                    },
                ],
                term: IrTerminator::Ret { value: VRegId(4) },
            }],
            vreg_types: vec![],
        }],
        entry: 0,
    };

    let _changed = common_subexpression_elimination(&mut m);
    let b0 = &m.funcs[0].blocks[0];
    assert!(
        matches!(b0.insns[2].op, IrOp::ObjGetAtom { .. }),
        "expected second ObjGetAtom to remain (not be CSE'd across Call)"
    );
}

#[test]
fn does_not_cse_bytes_get_across_bytes_set() {
    let mut m = IrModule {
        types: vec![],
        sigs: vec![],
        const_i64: vec![],
        const_f64: vec![],
        const_bytes: vec![],
        atoms: vec![],
        funcs: vec![IrFunction {
            name: Some("f".to_string()),
            param_count: 0,
            cap_vregs: vec![],
            entry: BlockId(0),
            blocks: vec![IrBlock {
                label: None,
                insns: vec![
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::BytesGetU8 {
                            dst: VRegId(2),
                            bytes: VRegId(0),
                            index: VRegId(1),
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::BytesSetU8 {
                            bytes: VRegId(0),
                            index: VRegId(1),
                            value: VRegId(3),
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::BytesGetU8 {
                            dst: VRegId(4),
                            bytes: VRegId(0),
                            index: VRegId(1),
                        },
                    },
                ],
                term: IrTerminator::Ret { value: VRegId(4) },
            }],
            vreg_types: vec![],
        }],
        entry: 0,
    };

    let _changed = common_subexpression_elimination(&mut m);
    let b0 = &m.funcs[0].blocks[0];
    assert!(
        matches!(b0.insns[2].op, IrOp::BytesGetU8 { .. }),
        "expected second BytesGetU8 to remain (not be CSE'd across BytesSetU8)"
    );
}

#[test]
fn cse_eliminates_redundant_i32_from_f64_in_block() {
    // Two identical trapping conversions from the same src within a block can be CSE'd safely.
    let mut m = IrModule {
        types: vec![],
        sigs: vec![],
        const_i64: vec![],
        const_f64: vec![0.0],
        const_bytes: vec![],
        atoms: vec![],
        funcs: vec![IrFunction {
            name: Some("f".to_string()),
            param_count: 0,
            cap_vregs: vec![],
            entry: BlockId(0),
            blocks: vec![IrBlock {
                label: None,
                insns: vec![
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::ConstF64 {
                            dst: VRegId(0),
                            pool_index: 0,
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::I32FromF64 {
                            dst: VRegId(1),
                            src: VRegId(0),
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::I32FromF64 {
                            dst: VRegId(2),
                            src: VRegId(0),
                        },
                    },
                ],
                term: IrTerminator::Ret { value: VRegId(1) },
            }],
            vreg_types: vec![T_F64, T_I32, T_I32],
        }],
        entry: 0,
    };

    let changed = common_subexpression_elimination(&mut m);
    assert!(changed);
    let b0 = &m.funcs[0].blocks[0];
    assert!(
        matches!(
            b0.insns[2].op,
            IrOp::Mov {
                dst: VRegId(2),
                src: VRegId(1)
            }
        ),
        "expected second conversion to be replaced by Mov"
    );
}
