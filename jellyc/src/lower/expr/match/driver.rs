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

use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{bind_local, lower_stmt, LowerCtx};

use super::super::{lower_expr_expect, T_BOOL};
use super::bind::{bind_array_rest_slice, bind_obj_fields, bind_tuple_elems};
use super::bind_seq::bind_seq_pattern;
use super::check::{lower_check_obj, lower_check_tuple_exact};
use super::check_scalar::{
    lower_check_bind, lower_check_bool_lit, lower_check_i16_lit, lower_check_i32_lit,
    lower_check_i8_lit, lower_check_pin, lower_check_wildcard,
};
use super::check_seq::{
    lower_check_array_exact, lower_check_array_head_tail, lower_check_array_prefix_rest,
};

pub(crate) fn lower_match_arms(
    e: &Expr,
    arms: &[crate::ast::MatchArm],
    v_subj: VRegId,
    t_subj: TypeId,
    out_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let v_out: Option<VRegId> = Some(b.new_vreg(out_tid));

    // Pre-create check/bind/guard/body blocks.
    let mut check_bs = Vec::with_capacity(arms.len());
    let mut bind_bs = Vec::with_capacity(arms.len());
    let mut guard_bs: Vec<Option<crate::ir::BlockId>> = Vec::with_capacity(arms.len());
    let mut body_bs = Vec::with_capacity(arms.len());
    for i in 0..arms.len() {
        check_bs.push(b.new_block(Some(format!("match_check{}", i))));
        bind_bs.push(b.new_block(Some(format!("match_bind{}", i))));
        guard_bs.push(if arms[i].when.is_some() {
            Some(b.new_block(Some(format!("match_when{}", i))))
        } else {
            None
        });
        body_bs.push(b.new_block(Some(format!("match_body{}", i))));
    }

    // Create join last, so its uses can't precede defs.
    let join_b = b.new_block(Some("match_join".to_string()));

    // Jump into the first check.
    b.term(IrTerminator::Jmp {
        target: check_bs[0],
    });

    for i in 0..arms.len() {
        let next_check = if i + 1 < arms.len() {
            check_bs[i + 1]
        } else {
            bind_bs[i]
        };
        let body_b = body_bs[i];
        let guard_b = guard_bs[i];
        let bind_b = bind_bs[i];

        // --- check
        b.set_block(check_bs[i]);

        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard => {
                lower_check_wildcard(b, bind_b);
            }
            crate::ast::PatternKind::BoolLit(p) => {
                lower_check_bool_lit(arms[i].pat.span, *p, b, v_subj, t_subj, bind_b, next_check)?;
            }
            crate::ast::PatternKind::I32Lit(p) => {
                lower_check_i32_lit(arms[i].pat.span, *p, b, v_subj, t_subj, bind_b, next_check)?;
            }
            crate::ast::PatternKind::I8Lit(p) => {
                lower_check_i8_lit(arms[i].pat.span, *p, b, v_subj, t_subj, bind_b, next_check)?;
            }
            crate::ast::PatternKind::I16Lit(p) => {
                lower_check_i16_lit(arms[i].pat.span, *p, b, v_subj, t_subj, bind_b, next_check)?;
            }
            crate::ast::PatternKind::Bind(_) => {
                lower_check_bind(b, bind_b);
            }
            crate::ast::PatternKind::Pin(name) => {
                lower_check_pin(
                    arms[i].pat.span,
                    name.as_str(),
                    ctx,
                    b,
                    v_subj,
                    t_subj,
                    bind_b,
                    next_check,
                )?;
            }
            crate::ast::PatternKind::Obj(fields) => {
                lower_check_obj(
                    i,
                    arms[i].pat.span,
                    fields,
                    ctx,
                    b,
                    v_subj,
                    t_subj,
                    bind_b,
                    next_check,
                )?;
            }
            crate::ast::PatternKind::TupleExact(elems) => {
                lower_check_tuple_exact(
                    i,
                    arms[i].pat.span,
                    elems,
                    ctx,
                    b,
                    v_subj,
                    t_subj,
                    bind_b,
                    next_check,
                )?;
            }
            crate::ast::PatternKind::ArrayExact(elems) => {
                lower_check_array_exact(
                    i,
                    arms[i].pat.span,
                    elems,
                    ctx,
                    b,
                    v_subj,
                    t_subj,
                    bind_b,
                    next_check,
                )?;
            }
            crate::ast::PatternKind::ArrayHeadTail { head, .. } => {
                lower_check_array_head_tail(
                    i,
                    arms[i].pat.span,
                    head,
                    ctx,
                    b,
                    v_subj,
                    t_subj,
                    bind_b,
                    next_check,
                )?;
            }
            crate::ast::PatternKind::ArrayPrefixRest { prefix, .. } => {
                lower_check_array_prefix_rest(
                    i,
                    arms[i].pat.span,
                    prefix,
                    ctx,
                    b,
                    v_subj,
                    t_subj,
                    bind_b,
                    next_check,
                )?;
            }
        }

        // --- bind block (introduce bindings, then jump to when/body)
        b.set_block(bind_b);
        let arm_ended_control_flow = ctx.with_env_scope(|ctx| {
            // If present, compute `rest` as a fresh array slice starting at `start_idx`.
            let mut need_slice: Option<(usize, String)> = None;

            match &arms[i].pat.node {
                crate::ast::PatternKind::Bind(name) => {
                    bind_local(ctx, name.as_str(), v_subj, t_subj);
                }
                crate::ast::PatternKind::Obj(fields) => {
                    bind_obj_fields(ctx, b, v_subj, fields);
                }
                crate::ast::PatternKind::TupleExact(elems) => {
                    bind_tuple_elems(ctx, b, v_subj, t_subj, arms[i].pat.span, elems)?;
                }
                crate::ast::PatternKind::ArrayExact(..)
                | crate::ast::PatternKind::ArrayHeadTail { .. }
                | crate::ast::PatternKind::ArrayPrefixRest { .. } => {
                    need_slice = bind_seq_pattern(
                        ctx,
                        b,
                        arms[i].pat.span,
                        &arms[i].pat.node,
                        v_subj,
                        t_subj,
                    )?;
                }
                _ => {}
            }

            // Compute rest slice if needed.
            let mut bind_cont_block = bind_b;
            if let Some((start_idx, rest_name)) = need_slice {
                bind_cont_block = bind_array_rest_slice(
                    ctx,
                    b,
                    i,
                    arms[i].pat.span,
                    v_subj,
                    t_subj,
                    start_idx,
                    rest_name.as_str(),
                )?;
            }

            // --- when guard (optional)
            if let Some(gb) = guard_b {
                b.set_block(bind_cont_block);
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
                    else_tgt: next_check,
                });
            } else {
                b.set_block(bind_cont_block);
                b.term(IrTerminator::Jmp { target: body_b });
            }

            // --- body
            b.set_block(body_b);
            for st in &arms[i].body {
                lower_stmt(st, ctx, b)?;
            }

            // If the arm already terminated control flow (eg `return`), don't try to
            // fall through / join.
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
                let fallthrough_target = bind_bs[i + 1];
                b.term(IrTerminator::Jmp { target: fallthrough_target });
            }

            Ok(false)
        })?;
        if arm_ended_control_flow {
            continue;
        }
    }

    let dst = v_out.expect("match output");
    b.set_block(join_b);
    Ok((dst, out_tid))
}
