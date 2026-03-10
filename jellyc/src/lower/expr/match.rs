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
use super::{lower_expr_expect, T_DYNAMIC, T_LIST_BYTES, T_LIST_I32};
use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{IrBuilder, TypeId, VRegId};
use crate::lower::{ensure_open_block, LowerCtx};

mod bind;
mod bind_seq;
mod check;
mod check_scalar;
mod check_seq;
mod driver;
mod dynamic_scalar;
mod util;
mod with;

use driver::lower_match_arms;
pub(crate) use with::lower_with_expr;
// Match expression lowering.
//
// The algorithm is as follows:
// 1. Lower the subject expression.
// 2. For each arm, check the pattern against the subject using the appropriate check function.
// 3. If the pattern matches, bind the value and jump to the body.
// 4. If the pattern does not match, jump to the next arm.
// 5. If all arms are checked and no match was found, jump to the join block.
// 6. The join block is responsible for producing the output value.

pub fn lower_match_expr(
    e: &Expr,
    subject: &Expr,
    arms: &[crate::ast::MatchArm],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let out_tid = ctx
        .sem_expr_types
        .get(&NodeId(e.span))
        .copied()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "missing semantic type for match",
            )
        })?;
    if arms.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "match must have at least one arm",
        ));
    }
    // For now, require the last arm to be `_ => { ...expr... }` so the match is exhaustive
    // and produces a value.
    if !matches!(
        arms.last().unwrap().pat.node,
        crate::ast::PatternKind::Wildcard
    ) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "non-exhaustive match (last arm must be '_')",
        ));
    }
    if arms.last().unwrap().when.is_some() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "last '_' match arm cannot have a when-guard",
        ));
    }
    if arms.last().unwrap().tail.is_none() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "match must produce a value (last arm must have a value)",
        ));
    }

    ensure_open_block(b);

    let (v_subj, t_subj) = lower_expr_expect(subject, ctx, b)?;

    // Special case: matching on a Dynamic subject with scalar-only patterns.
    // This is the first place where `KINDOF` + `SWITCH_KIND` is a clear win.
    if t_subj == T_DYNAMIC {
        let scalar_ok = arms.iter().all(|a| {
            matches!(
                a.pat.node,
                crate::ast::PatternKind::Wildcard
                    | crate::ast::PatternKind::Bind(_)
                    | crate::ast::PatternKind::Pin(_)
                    | crate::ast::PatternKind::BoolLit(_)
                    | crate::ast::PatternKind::I8Lit(_)
                    | crate::ast::PatternKind::I16Lit(_)
                    | crate::ast::PatternKind::I32Lit(_)
            )
        });
        if scalar_ok {
            return dynamic_scalar::lower_match_dynamic_scalar(e, v_subj, arms, out_tid, ctx, b);
        }
    }

    lower_match_arms(e, arms, v_subj, t_subj, out_tid, ctx, b)
}
