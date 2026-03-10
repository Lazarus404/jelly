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

use crate::ir::{IrBuilder, IrOp, IrTerminator, VRegId};

use crate::lower::T_I32;

#[derive(Clone)]
pub(super) struct DynScalarBlocks {
    pub bind_bs: Vec<crate::ir::BlockId>,
    pub guard_bs: Vec<Option<crate::ir::BlockId>>,
    pub body_bs: Vec<crate::ir::BlockId>,
    pub resume_bs: Vec<crate::ir::BlockId>,
    pub check_bool_bs: Vec<crate::ir::BlockId>,
    pub check_i32_bs: Vec<crate::ir::BlockId>,
    pub check_other_bs: Vec<crate::ir::BlockId>,
    pub join_b: crate::ir::BlockId,
}

pub(super) fn alloc_blocks(arms: &[crate::ast::MatchArm], b: &mut IrBuilder) -> DynScalarBlocks {
    let mut bind_bs = Vec::with_capacity(arms.len());
    let mut guard_bs: Vec<Option<crate::ir::BlockId>> = Vec::with_capacity(arms.len());
    let mut body_bs = Vec::with_capacity(arms.len());
    let mut resume_bs = Vec::with_capacity(arms.len());
    let mut check_bool_bs = Vec::with_capacity(arms.len());
    let mut check_i32_bs = Vec::with_capacity(arms.len());
    let mut check_other_bs = Vec::with_capacity(arms.len());

    for i in 0..arms.len() {
        bind_bs.push(b.new_block(Some(format!("dmatch_bind{}", i))));
        guard_bs.push(if arms[i].when.is_some() {
            Some(b.new_block(Some(format!("dmatch_when{}", i))))
        } else {
            None
        });
        body_bs.push(b.new_block(Some(format!("dmatch_body{}", i))));
        resume_bs.push(b.new_block(Some(format!("dmatch_resume{}", i))));
        check_bool_bs.push(b.new_block(Some(format!("dmatch_check_bool{}", i))));
        check_i32_bs.push(b.new_block(Some(format!("dmatch_check_i32{}", i))));
        check_other_bs.push(b.new_block(Some(format!("dmatch_check_other{}", i))));
    }

    let join_b = b.new_block(Some("dmatch_join".to_string()));

    DynScalarBlocks {
        bind_bs,
        guard_bs,
        body_bs,
        resume_bs,
        check_bool_bs,
        check_i32_bs,
        check_other_bs,
        join_b,
    }
}

fn chain_entries(
    arms: &[crate::ast::MatchArm],
    blocks: &DynScalarBlocks,
) -> (crate::ir::BlockId, crate::ir::BlockId, crate::ir::BlockId) {
    let mut first_bool = None::<crate::ir::BlockId>;
    let mut first_i32 = None::<crate::ir::BlockId>;
    let mut first_other = None::<crate::ir::BlockId>;

    for i in 0..arms.len() {
        match &arms[i].pat.node {
            crate::ast::PatternKind::BoolLit(_)
            | crate::ast::PatternKind::Wildcard
            | crate::ast::PatternKind::Bind(_)
            | crate::ast::PatternKind::Pin(_) => {
                if first_bool.is_none() {
                    first_bool = Some(blocks.check_bool_bs[i]);
                }
            }
            _ => {}
        }
        match &arms[i].pat.node {
            crate::ast::PatternKind::I32Lit(_)
            | crate::ast::PatternKind::I8Lit(_)
            | crate::ast::PatternKind::I16Lit(_)
            | crate::ast::PatternKind::Wildcard
            | crate::ast::PatternKind::Bind(_)
            | crate::ast::PatternKind::Pin(_) => {
                if first_i32.is_none() {
                    first_i32 = Some(blocks.check_i32_bs[i]);
                }
            }
            _ => {}
        }
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard
            | crate::ast::PatternKind::Bind(_)
            | crate::ast::PatternKind::Pin(_) => {
                if first_other.is_none() {
                    first_other = Some(blocks.check_other_bs[i]);
                }
            }
            _ => {}
        }
    }

    let entry_bool = first_bool.unwrap_or(blocks.check_other_bs[0]);
    let entry_i32 = first_i32.unwrap_or(blocks.check_other_bs[0]);
    let entry_other = first_other.unwrap_or(blocks.check_other_bs[0]);
    (entry_bool, entry_i32, entry_other)
}

pub(super) fn emit_kind_dispatch_and_resume(
    span: crate::ast::Span,
    v_subj: VRegId,
    arms: &[crate::ast::MatchArm],
    blocks: &DynScalarBlocks,
    b: &mut IrBuilder,
) -> VRegId {
    let (entry_bool, entry_i32, entry_other) = chain_entries(arms, blocks);

    let v_kind = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::Kindof {
            dst: v_kind,
            src: v_subj,
        },
    );
    b.term(IrTerminator::SwitchKind {
        kind: v_kind,
        cases: vec![(1, entry_bool), (2, entry_i32)],
        default: entry_other,
    });

    for i in 0..arms.len() {
        b.set_block(blocks.resume_bs[i]);
        let j = i + 1;
        if j >= arms.len() {
            b.term(IrTerminator::Jmp {
                target: blocks.join_b,
            });
            continue;
        }
        b.term(IrTerminator::SwitchKind {
            kind: v_kind,
            cases: vec![(1, blocks.check_bool_bs[j]), (2, blocks.check_i32_bs[j])],
            default: blocks.check_other_bs[j],
        });
    }

    v_kind
}
