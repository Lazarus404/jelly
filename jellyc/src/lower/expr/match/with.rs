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

// Lowering for `with` expression (Elixir-style chained pattern matching).

use super::super::{lower_expr_expect, T_DYNAMIC};
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
use super::driver::lower_match_arms;
use crate::ast::{Expr, PatternKind};
use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{bind_local, ensure_open_block, LowerCtx};

pub fn lower_with_expr(
    e: &Expr,
    clauses: &[(crate::ast::Pattern, Expr)],
    body: &Expr,
    else_arms: &Option<Vec<crate::ast::MatchArm>>,
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
                "missing semantic type for with",
            )
        })?;

    ensure_open_block(b);

    let n = clauses.len();
    let mut clause_blocks: Vec<BlockId> = Vec::with_capacity(n);
    clause_blocks.push(b.cur_block());
    for _ in 1..n {
        clause_blocks.push(b.new_block(Some("with_clause".to_string())));
    }
    let body_b = b.new_block(Some("with_body".to_string()));
    let join_b = b.new_block(Some("with_join".to_string()));

    let mut fail_blocks: Vec<BlockId> = Vec::with_capacity(n);
    for i in 0..n {
        fail_blocks.push(b.new_block(Some(format!("with_fail{}", i))));
    }

    let (fail_values, _body_end, _body_reaches_join, join_incomings) = ctx.with_env_scope(|ctx| {
        let mut fail_values = Vec::with_capacity(n);
        for i in 0..n {
            let (pat, expr) = &clauses[i];
            let next_clause_or_body = if i + 1 < n {
                clause_blocks[i + 1]
            } else {
                body_b
            };

            b.set_block(clause_blocks[i]);
            let (v_i, t_i) = lower_expr_expect(expr, ctx, b)?;
            fail_values.push(v_i);

            let bind_b = b.new_block(Some(format!("with_bind{}", i)));
            let fail_b = fail_blocks[i];

            match &pat.node {
                PatternKind::Wildcard => lower_check_wildcard(b, bind_b),
                PatternKind::Bind(_) => lower_check_bind(b, bind_b),
                PatternKind::BoolLit(p) => {
                    lower_check_bool_lit(pat.span, *p, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::I32Lit(p) => {
                    lower_check_i32_lit(pat.span, *p, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::I8Lit(p) => {
                    lower_check_i8_lit(pat.span, *p, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::I16Lit(p) => {
                    lower_check_i16_lit(pat.span, *p, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::Pin(name) => {
                    lower_check_pin(pat.span, name.as_str(), ctx, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::Obj(fields) => {
                    lower_check_obj(i, pat.span, fields, ctx, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::TupleExact(elems) => {
                    lower_check_tuple_exact(i, pat.span, elems, ctx, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::ArrayExact(elems) => {
                    lower_check_array_exact(i, pat.span, elems, ctx, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::ArrayHeadTail { head, .. } => {
                    lower_check_array_head_tail(i, pat.span, head, ctx, b, v_i, t_i, bind_b, fail_b)?;
                }
                PatternKind::ArrayPrefixRest { prefix, .. } => {
                    lower_check_array_prefix_rest(i, pat.span, prefix, ctx, b, v_i, t_i, bind_b, fail_b)?;
                }
            }

            b.set_block(bind_b);
            let mut need_slice: Option<(usize, String)> = None;
            match &pat.node {
                PatternKind::Bind(name) => bind_local(ctx, name.as_str(), v_i, t_i),
                PatternKind::Obj(fields) => bind_obj_fields(ctx, b, v_i, fields),
                PatternKind::TupleExact(elems) => {
                    bind_tuple_elems(ctx, b, v_i, t_i, pat.span, elems)?;
                }
                PatternKind::ArrayExact(..)
                | PatternKind::ArrayHeadTail { .. }
                | PatternKind::ArrayPrefixRest { .. } => {
                    need_slice = bind_seq_pattern(ctx, b, pat.span, &pat.node, v_i, t_i)?;
                }
                _ => {}
            }
            let mut bind_cont_block = bind_b;
            if let Some((start_idx, rest_name)) = need_slice {
                bind_cont_block = bind_array_rest_slice(
                    ctx, b, i, pat.span, v_i, t_i, start_idx, rest_name.as_str(),
                )?;
            }
            b.set_block(bind_cont_block);
            b.term(IrTerminator::Jmp {
                target: next_clause_or_body,
            });
        }

        b.set_block(body_b);
        let (v_body, _t_body) = lower_expr_expect(body, ctx, b)?;
        let body_end = b.cur_block();
        let body_reaches_join = b.is_open();
        if body_reaches_join {
            b.term(IrTerminator::Jmp { target: join_b });
        }
        let join_incomings: Vec<(BlockId, VRegId)> = if body_reaches_join {
            vec![(body_end, v_body)]
        } else {
            vec![]
        };
        Ok((fail_values, body_end, body_reaches_join, join_incomings))
    })?;

    let v_res = b.new_vreg(out_tid);
    let mut join_incomings = join_incomings;

    if let Some(arms) = else_arms {
        let fail_merge = b.new_block(Some("with_fail_merge".to_string()));
        for &fail_b in fail_blocks.iter() {
            b.set_block(fail_b);
            b.term(IrTerminator::Jmp {
                target: fail_merge,
            });
        }

        b.set_block(fail_merge);
        let v_fail = b.new_vreg(T_DYNAMIC);
        b.emit(
            e.span,
            IrOp::Phi {
                dst: v_fail,
                incomings: fail_blocks
                    .iter()
                    .zip(fail_values.iter())
                    .map(|(blk, v)| (*blk, *v))
                    .collect(),
            },
        );

        let (v_else, _) = lower_match_arms(e, arms, v_fail, T_DYNAMIC, out_tid, ctx, b)?;
        let else_end = b.cur_block();
        b.term(IrTerminator::Jmp { target: join_b });
        join_incomings.push((else_end, v_else));
    } else {
        for (i, &fail_b) in fail_blocks.iter().enumerate() {
            b.set_block(fail_b);
            let v_fail = fail_values[i];
            b.term(IrTerminator::Jmp { target: join_b });
            join_incomings.push((fail_b, v_fail));
        }
    }

    b.set_block(join_b);
    b.emit(
        e.span,
        IrOp::Phi {
            dst: v_res,
            incomings: join_incomings,
        },
    );
    Ok((v_res, out_tid))
}
