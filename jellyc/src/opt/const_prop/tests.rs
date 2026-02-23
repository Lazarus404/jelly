/**
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

#[test]
fn const_prop_folds_jmpif_with_const_bool_cond() {
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
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::ConstBool {
                            dst: VRegId(0),
                            imm: true,
                        },
                    }],
                    term: IrTerminator::JmpIf {
                        cond: VRegId(0),
                        then_tgt: BlockId(1),
                        else_tgt: BlockId(2),
                    },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::Ret { value: VRegId(0) },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::Ret { value: VRegId(0) },
                },
            ],
            vreg_types: vec![crate::typectx::T_BOOL],
        }],
        entry: 0,
    };

    let changed = constant_propagation(&mut m);
    assert!(changed);
    match m.funcs[0].blocks[0].term {
        IrTerminator::Jmp { target } => assert_eq!(target, BlockId(1)),
        _ => panic!("expected Jmp"),
    }
}

#[test]
fn const_prop_folds_switch_kind_with_const_tag() {
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
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::ConstI32 {
                            dst: VRegId(0),
                            imm: 3,
                        },
                    }],
                    term: IrTerminator::SwitchKind {
                        kind: VRegId(0),
                        cases: vec![(3, BlockId(1)), (5, BlockId(2))],
                        default: BlockId(2),
                    },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::Ret { value: VRegId(0) },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::Ret { value: VRegId(0) },
                },
            ],
            vreg_types: vec![crate::typectx::T_I32],
        }],
        entry: 0,
    };

    let changed = constant_propagation(&mut m);
    assert!(changed);
    match m.funcs[0].blocks[0].term {
        IrTerminator::Jmp { target } => assert_eq!(target, BlockId(1)),
        _ => panic!("expected Jmp"),
    }
}

#[test]
fn const_prop_folds_jmpif_when_cond_constant_is_defined_in_pred_block() {
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
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::ConstBool {
                            dst: VRegId(0),
                            imm: false,
                        },
                    }],
                    term: IrTerminator::Jmp { target: BlockId(1) },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::JmpIf {
                        cond: VRegId(0),
                        then_tgt: BlockId(2),
                        else_tgt: BlockId(3),
                    },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::Ret { value: VRegId(0) },
                },
                IrBlock {
                    label: None,
                    insns: vec![],
                    term: IrTerminator::Ret { value: VRegId(0) },
                },
            ],
            vreg_types: vec![crate::typectx::T_BOOL],
        }],
        entry: 0,
    };

    let changed = constant_propagation(&mut m);
    assert!(changed);
    match m.funcs[0].blocks[1].term {
        IrTerminator::Jmp { target } => assert_eq!(target, BlockId(3)),
        _ => panic!("expected Jmp"),
    }
}
