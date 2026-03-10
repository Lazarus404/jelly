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
use crate::ir::{BlockId, IrBlock, IrBuilder, IrFunction, IrInsn, IrModule, IrTerminator};
use crate::typectx::{T_BOOL, T_I32};

#[test]
fn copy_prop_rewrites_operands_through_movs() {
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
                        op: IrOp::ConstI32 {
                            dst: VRegId(0),
                            imm: 1,
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::Mov {
                            dst: VRegId(1),
                            src: VRegId(0),
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::Mov {
                            dst: VRegId(2),
                            src: VRegId(1),
                        },
                    },
                    IrInsn {
                        span: Span::point(0),
                        op: IrOp::AddI32 {
                            dst: VRegId(3),
                            a: VRegId(2),
                            b: VRegId(2),
                        },
                    },
                ],
                term: IrTerminator::Ret { value: VRegId(3) },
            }],
            vreg_types: vec![crate::typectx::T_I32; 4],
        }],
        entry: 0,
    };

    let changed = copy_propagation(&mut m);
    assert!(
        changed,
        "expected copy propagation to rewrite some operands"
    );

    let b0 = &m.funcs[0].blocks[0];
    match &b0.insns[3].op {
        IrOp::AddI32 { a, b, .. } => {
            assert_eq!(*a, VRegId(0));
            assert_eq!(*b, VRegId(0));
        }
        _ => panic!("expected AddI32"),
    }
}

#[test]
fn copy_prop_can_propagate_across_basic_blocks() {
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
                            op: IrOp::Mov {
                                dst: VRegId(1),
                                src: VRegId(0),
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
                            dst: VRegId(2),
                            a: VRegId(1),
                            b: VRegId(1),
                        },
                    }],
                    term: IrTerminator::Ret { value: VRegId(2) },
                },
            ],
            vreg_types: vec![crate::typectx::T_I32; 3],
        }],
        entry: 0,
    };

    let changed = copy_propagation(&mut m);
    assert!(
        changed,
        "expected copy propagation to rewrite some operands"
    );
    match &m.funcs[0].blocks[1].insns[0].op {
        IrOp::AddI32 { a, b, .. } => {
            assert_eq!(*a, VRegId(0));
            assert_eq!(*b, VRegId(0));
        }
        _ => panic!("expected AddI32"),
    }
}

#[test]
fn copy_prop_is_conservative_when_predecessors_disagree() {
    // Diamond CFG where v1 aliases v0 only on one path; join must not rewrite v1 -> v0.
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
                            op: IrOp::ConstBool {
                                dst: VRegId(3),
                                imm: true,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 {
                                dst: VRegId(0),
                                imm: 1,
                            },
                        },
                    ],
                    term: IrTerminator::JmpIf {
                        cond: VRegId(3),
                        then_tgt: BlockId(1),
                        else_tgt: BlockId(2),
                    },
                },
                IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::Mov {
                            dst: VRegId(1),
                            src: VRegId(0),
                        },
                    }],
                    term: IrTerminator::Jmp { target: BlockId(3) },
                },
                IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::ConstI32 {
                            dst: VRegId(1),
                            imm: 2,
                        },
                    }],
                    term: IrTerminator::Jmp { target: BlockId(3) },
                },
                IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::AddI32 {
                            dst: VRegId(2),
                            a: VRegId(1),
                            b: VRegId(1),
                        },
                    }],
                    term: IrTerminator::Ret { value: VRegId(2) },
                },
            ],
            vreg_types: vec![
                crate::typectx::T_I32,  // v0
                crate::typectx::T_I32,  // v1
                crate::typectx::T_I32,  // v2
                crate::typectx::T_BOOL, // v3 cond
            ],
        }],
        entry: 0,
    };

    let _changed = copy_propagation(&mut m);
    match &m.funcs[0].blocks[3].insns[0].op {
        IrOp::AddI32 { a, b, .. } => {
            assert_eq!(*a, VRegId(1));
            assert_eq!(*b, VRegId(1));
        }
        _ => panic!("expected AddI32"),
    }
}

#[test]
fn copy_prop_can_alias_through_identity_phi() {
    let mut b = IrBuilder::new(Some("t".to_string()));
    let v0 = b.new_vreg(T_I32);
    let vcond = b.new_vreg(T_BOOL);
    let vphi = b.new_vreg(T_I32);
    let vout = b.new_vreg(T_I32);

    let bb_then = b.new_block(Some("then".to_string()));
    let bb_else = b.new_block(Some("else".to_string()));
    let bb_join = b.new_block(Some("join".to_string()));

    // entry:
    b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 7 });
    b.emit(
        Span::point(0),
        IrOp::ConstBool {
            dst: vcond,
            imm: true,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: vcond,
        then_tgt: bb_then,
        else_tgt: bb_else,
    });

    // then:
    b.set_block(bb_then);
    b.term(IrTerminator::Jmp { target: bb_join });

    // else:
    b.set_block(bb_else);
    b.term(IrTerminator::Jmp { target: bb_join });

    // join:
    b.set_block(bb_join);
    b.emit(
        Span::point(0),
        IrOp::Phi {
            dst: vphi,
            incomings: vec![(bb_then, v0), (bb_else, v0)],
        },
    );
    b.emit(
        Span::point(0),
        IrOp::Mov {
            dst: vout,
            src: vphi,
        },
    );
    b.term(IrTerminator::Ret { value: vout });

    let mut m = IrModule {
        types: vec![],
        sigs: vec![],
        const_i64: vec![],
        const_f64: vec![],
        const_bytes: vec![],
        atoms: vec![],
        funcs: vec![b.func],
        entry: 0,
    };
    let changed = copy_propagation(&mut m);
    assert!(
        changed,
        "expected copy propagation to rewrite some operands"
    );

    let f = &m.funcs[0];
    let join = &f.blocks[bb_join.0 as usize];
    match join.insns[1].op {
        IrOp::Mov { src, .. } => assert_eq!(src, v0),
        _ => panic!("expected Mov"),
    }
}
