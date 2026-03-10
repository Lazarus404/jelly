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
use crate::ir::{IrBuilder, IrModule, IrOp, IrTerminator};
use crate::typectx::{T_BOOL, T_DYNAMIC, T_I32};

#[test]
fn eliminates_from_dyn_across_basic_blocks_when_proven() {
    let mut b = IrBuilder::new(Some("t".to_string()));
    let v0 = b.new_vreg(T_I32);
    let v1 = b.new_vreg(T_DYNAMIC);
    let v2 = b.new_vreg(T_I32);

    let bb1 = b.new_block(Some("bb1".to_string()));

    // entry:
    b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 123 });
    b.emit(Span::point(0), IrOp::ToDyn { dst: v1, src: v0 });
    b.term(IrTerminator::Jmp { target: bb1 });

    // bb1:
    b.set_block(bb1);
    b.emit(Span::point(0), IrOp::FromDynI32 { dst: v2, src: v1 });
    b.term(IrTerminator::Ret { value: v2 });

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

    let changed = simplify_dynamic_boundaries(&mut m);
    assert!(changed, "expected boundary simplify to change IR");

    let ins = &m.funcs[0].blocks[bb1.0 as usize].insns[0].op;
    assert!(
        matches!(ins, IrOp::Mov { dst, src } if *dst == v2 && *src == v0),
        "expected FromDynI32 to become Mov from typed origin, got {ins:?}"
    );
}

#[test]
fn eliminates_from_dyn_across_phi_join_when_proven() {
    let mut b = IrBuilder::new(Some("t".to_string()));
    let v0 = b.new_vreg(T_I32);
    let vcond = b.new_vreg(T_BOOL);
    let v1 = b.new_vreg(T_DYNAMIC);
    let vphi = b.new_vreg(T_DYNAMIC);
    let v2 = b.new_vreg(T_I32);

    let bb_then = b.new_block(Some("then".to_string()));
    let bb_else = b.new_block(Some("else".to_string()));
    let bb_join = b.new_block(Some("join".to_string()));

    // entry:
    b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 123 });
    b.emit(Span::point(0), IrOp::ToDyn { dst: v1, src: v0 });
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
            incomings: vec![(bb_then, v1), (bb_else, v1)],
        },
    );
    b.emit(Span::point(0), IrOp::FromDynI32 { dst: v2, src: vphi });
    b.term(IrTerminator::Ret { value: v2 });

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

    let changed = simplify_dynamic_boundaries(&mut m);
    assert!(changed, "expected boundary simplify to change IR");

    let join = &m.funcs[0].blocks[bb_join.0 as usize];
    let ins = &join.insns[1].op;
    assert!(
        matches!(ins, IrOp::Mov { dst, src } if *dst == v2 && *src == v0),
        "expected FromDynI32 to become Mov from typed origin, got {ins:?}"
    );
}
