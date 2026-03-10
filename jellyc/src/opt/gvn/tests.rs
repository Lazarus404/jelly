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
use crate::ir::{BlockId, IrBlock, IrFunction, IrInsn, IrModule, IrOp, IrTerminator, VRegId};
use crate::typectx::{T_ARRAY_I32, T_DYNAMIC, T_I32};

#[test]
fn gvn_eliminates_redundant_pure_op_across_blocks() {
    // entry:
    //   v0 = const 1
    //   v1 = const 2
    //   v2 = add v0 v1
    //   jmp b1
    // b1:
    //   v3 = add v0 v1   ; redundant, dominated by v2
    //   ret v3
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
            blocks: vec![
                IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 {
                                dst: VRegId(0),
                                imm: 1,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 {
                                dst: VRegId(1),
                                imm: 2,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::AddI32 {
                                dst: VRegId(2),
                                a: VRegId(0),
                                b: VRegId(1),
                            },
                        },
                    ],
                    term: IrTerminator::Jmp { target: BlockId(1) },
                },
                IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::AddI32 {
                            dst: VRegId(3),
                            a: VRegId(0),
                            b: VRegId(1),
                        },
                    }],
                    term: IrTerminator::Ret { value: VRegId(3) },
                },
            ],
            vreg_types: vec![T_I32, T_I32, T_I32, T_I32],
        }],
        entry: 0,
    };

    let changed = global_value_numbering(&mut m);
    assert!(changed);
    let b1 = &m.funcs[0].blocks[1];
    assert!(
        matches!(
            b1.insns[0].op,
            IrOp::Mov {
                dst: VRegId(3),
                src: VRegId(2)
            }
        ),
        "expected redundant add to be replaced by Mov"
    );
}

#[test]
fn gvn_eliminates_redundant_from_dyn_across_blocks() {
    // entry:
    //   v0:I32 = const 123
    //   v1:Any = to_dyn v0
    //   v2:I32 = from_dyn_i32 v1
    //   jmp b1
    // b1:
    //   v3:I32 = from_dyn_i32 v1   ; redundant, dominated by v2
    //   ret v3
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
            blocks: vec![
                IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 {
                                dst: VRegId(0),
                                imm: 123,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ToDyn {
                                dst: VRegId(1),
                                src: VRegId(0),
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::FromDynI32 {
                                dst: VRegId(2),
                                src: VRegId(1),
                            },
                        },
                    ],
                    term: IrTerminator::Jmp { target: BlockId(1) },
                },
                IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::FromDynI32 {
                            dst: VRegId(3),
                            src: VRegId(1),
                        },
                    }],
                    term: IrTerminator::Ret { value: VRegId(3) },
                },
            ],
            vreg_types: vec![T_I32, T_DYNAMIC, T_I32, T_I32],
        }],
        entry: 0,
    };

    let changed = global_value_numbering(&mut m);
    assert!(changed);
    let b1 = &m.funcs[0].blocks[1];
    assert!(
        matches!(
            b1.insns[0].op,
            IrOp::Mov {
                dst: VRegId(3),
                src: VRegId(2)
            }
        ),
        "expected redundant FromDynI32 to be replaced by Mov"
    );
}

#[test]
fn gvn_eliminates_redundant_array_len_across_blocks() {
    // entry:
    //   v0:Array<I32> = ...
    //   v1:I32 = array_len v0
    //   jmp b1
    // b1:
    //   v2:I32 = array_len v0   ; redundant, dominated by v1
    //   ret v2
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
            blocks: vec![
                IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 {
                                dst: VRegId(10),
                                imm: 3,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ArrayNew {
                                dst: VRegId(0),
                                len: VRegId(10),
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ArrayLen {
                                dst: VRegId(1),
                                arr: VRegId(0),
                            },
                        },
                    ],
                    term: IrTerminator::Jmp { target: BlockId(1) },
                },
                IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::ArrayLen {
                            dst: VRegId(2),
                            arr: VRegId(0),
                        },
                    }],
                    term: IrTerminator::Ret { value: VRegId(2) },
                },
            ],
            // Types don't matter for GVN itself, but keep them consistent.
            vreg_types: {
                let mut v = vec![T_ARRAY_I32, T_I32, T_I32];
                // pad up to v10
                while v.len() < 11 {
                    v.push(T_DYNAMIC);
                }
                v[10] = T_I32;
                v
            },
        }],
        entry: 0,
    };

    let changed = global_value_numbering(&mut m);
    assert!(changed);
    let b1 = &m.funcs[0].blocks[1];
    assert!(
        matches!(
            b1.insns[0].op,
            IrOp::Mov {
                dst: VRegId(2),
                src: VRegId(1)
            }
        ),
        "expected redundant ArrayLen to be replaced by Mov"
    );
}
