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

use super::super::{lower_expr_expect, T_BOOL, T_DYNAMIC};
use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{bind_local, ensure_open_block, lower_stmt, LowerCtx};

// Dynamic scalar match lowering.
//
// This is a special case for matching on a Dynamic subject with scalar-only patterns.
// This is the first place where `KINDOF` + `SWITCH_KIND` is a clear win.
//
// The algorithm is as follows:
// 1. Dispatch on the kind of the subject using `SWITCH_KIND`.
// 2. For each arm, check the pattern against the subject using the appropriate check function.
// 3. If the pattern matches, bind the value and jump to the body.
// 4. If the pattern does not match, jump to the next arm.

mod check;
mod dispatch;

pub(super) fn lower_match_dynamic_scalar(
    e: &Expr,
    v_subj: VRegId,
    arms: &[crate::ast::MatchArm],
    out_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Re-check exhaustiveness / value production (same rules as the general match lowering).
    if arms.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "match must have at least one arm",
        ));
    }
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

    // Ensure we are in an open block.
    ensure_open_block(b);

    let v_out: Option<VRegId> = Some(b.new_vreg(out_tid));

    // Shared blocks per arm (bind/guard/body), and per-kind check chains.
    let blocks = dispatch::alloc_blocks(arms, b);
    let join_b = blocks.join_b;
    let _v_kind = dispatch::emit_kind_dispatch_and_resume(e.span, v_subj, arms, &blocks, b);

    for i in 0..arms.len() {
        let bind_b = blocks.bind_bs[i];
        let body_b = blocks.body_bs[i];
        let guard_b = blocks.guard_bs[i];
        let resume_b = blocks.resume_bs[i];

        // --- check chain helpers
        let next_bool = if i + 1 < arms.len() {
            blocks.check_bool_bs[i + 1]
        } else {
            bind_b
        };
        let next_i32 = if i + 1 < arms.len() {
            blocks.check_i32_bs[i + 1]
        } else {
            bind_b
        };
        let next_other = if i + 1 < arms.len() {
            blocks.check_other_bs[i + 1]
        } else {
            bind_b
        };

        // --- check_bool
        b.set_block(blocks.check_bool_bs[i]);
        check::emit_check_bool_arm(ctx, b, v_subj, &arms[i], bind_b, next_bool)?;

        // --- check_i32
        b.set_block(blocks.check_i32_bs[i]);
        check::emit_check_i32_arm(ctx, b, v_subj, &arms[i], bind_b, next_i32)?;

        // --- check_other (only wildcard/bind/pin apply)
        b.set_block(blocks.check_other_bs[i]);
        check::emit_check_other_arm(ctx, b, v_subj, &arms[i], bind_b, next_other)?;

        // --- bind block
        b.set_block(bind_b);
        let arm_ended_control_flow = ctx.with_env_scope(|ctx| {
            if let crate::ast::PatternKind::Bind(name) = &arms[i].pat.node {
                bind_local(ctx, name.as_str(), v_subj, T_DYNAMIC);
            }

            // Route through optional when-guard.
            if let Some(gb) = guard_b {
                b.term(IrTerminator::Jmp { target: gb });
                b.set_block(gb);
                let w = arms[i].when.as_ref().expect("guard block implies when");
                let (v_w, t_w) = lower_expr_expect(w, ctx, b)?;
                if t_w != T_BOOL {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        w.span,
                        "match when guard must be bool",
                    ));
                }
                b.term(IrTerminator::JmpIf {
                    cond: v_w,
                    then_tgt: body_b,
                    else_tgt: resume_b,
                });
            } else {
                b.term(IrTerminator::Jmp { target: body_b });
            }

            // --- body
            b.set_block(body_b);
            for st in &arms[i].body {
                lower_stmt(st, ctx, b)?;
            }
            if !b.is_open() {
                return Ok(true);
            }

            if let Some(tail) = &arms[i].tail {
                let (v_tail, t_tail) = lower_expr_expect(tail, ctx, b)?;
                if t_tail != out_tid {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        tail.span,
                        "semantic type mismatch for match arm value",
                    ));
                }
                let dst = v_out.expect("match output");
                b.emit(e.span, IrOp::Mov { dst, src: v_tail });
                b.term(IrTerminator::Jmp { target: join_b });
            } else {
                // Fall through: execute next arm's body without re-checking (C-style fallthrough).
                let fallthrough_target = blocks.bind_bs[i + 1];
                b.term(IrTerminator::Jmp { target: fallthrough_target });
            }
            Ok(false)
        })?;
        if arm_ended_control_flow {
            continue;
        }
    }

    let dst = v_out.expect("allocated when type known");
    b.set_block(join_b);
    Ok((dst, out_tid))
}
