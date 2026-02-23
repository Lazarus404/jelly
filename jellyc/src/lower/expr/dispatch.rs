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

pub(super) fn lower_expr_expect_impl(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    if let Some(res) = lit::lower_scalar_lit_expr(e, expect, ctx, b) {
        return res;
    }

    #[allow(unreachable_patterns)]
    match &e.node {
        ExprKind::Var(name) => access::lower_var_expr(e, name.as_str(), expect, ctx, b),
        ExprKind::Member { base, name } => {
            access::lower_member_expr(e, base, name.as_str(), expect, ctx, b)
        }
        ExprKind::ArrayLit(elems) => lit::lower_array_lit(e, elems, ctx, b),
        ExprKind::TupleLit(elems) => lit::lower_tuple_lit(e, elems, ctx, b),
        ExprKind::ObjLit(fields) => {
            let out_tid = expect.ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    e.span,
                    "missing semantic type for object literal",
                )
            })?;
            lit::lower_obj_lit(e, fields, out_tid, ctx, b)
        }
        ExprKind::Index { base, index } => access::lower_index_expr(e, base, index, ctx, b),
        ExprKind::IndexAssign { base, index, expr } => {
            access::lower_index_assign_expr(e, base, index, expr, ctx, b)
        }
        ExprKind::Assign { name, expr: init } => {
            crate::lower::stmt::lower_assign_expr(e, name, init, ctx, b)
        }
        ExprKind::MemberAssign { base, name, expr: init } => {
            crate::lower::stmt::lower_member_assign_expr(e, base, name, init, ctx, b)
        }
        ExprKind::Add(_a, _b) => op::lower_add_expr(e, expect, ctx, b),
        ExprKind::Sub(a, bb) => op::lower_sub_expr(e, a, bb, expect, ctx, b),
        ExprKind::Mul(a, bb) => op::lower_mul_expr(e, a, bb, expect, ctx, b),
        ExprKind::Div(a, bb) => op::lower_div_expr(e, a, bb, expect, ctx, b),
        ExprKind::Mod(a, bb) => op::lower_mod_expr(e, a, bb, expect, ctx, b),
        ExprKind::Shl(a, bb) => op::lower_shl_expr(e, a, bb, expect, ctx, b),
        ExprKind::Shr(a, bb) => op::lower_shr_expr(e, a, bb, expect, ctx, b),
        ExprKind::Neg(inner) => op::lower_neg_expr(e, inner, ctx, b),
        ExprKind::Truthy(inner) => op::lower_truthy_expr(e, inner, ctx, b),
        ExprKind::Not(inner) => op::lower_not_expr(e, inner, ctx, b),
        ExprKind::Eq(a, bb) => op::lower_eq_expr(e, a, bb, ctx, b),
        ExprKind::Ne(a, bb) => op::lower_ne_expr(e, a, bb, ctx, b),
        ExprKind::Lt(a, bb) => op::lower_lt_expr(e, a, bb, ctx, b),
        ExprKind::Gt(a, bb) => op::lower_gt_expr(e, a, bb, ctx, b),
        ExprKind::Le(a, bb) => op::lower_le_expr(e, a, bb, ctx, b),
        ExprKind::Ge(a, bb) => op::lower_ge_expr(e, a, bb, ctx, b),
        ExprKind::And(a, bb) => op::lower_and_expr(e, a, bb, ctx, b),
        ExprKind::Or(a, bb) => op::lower_or_expr(e, a, bb, ctx, b),
        ExprKind::TypeApp { .. } => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "unexpected type application (templates must be expanded before lowering)",
        )),
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => call::lower_call_expr(e, callee, type_args, args, ctx, b),
        ExprKind::Block { stmts, expr } => control::lower_block_expr(stmts, expr, ctx, b),
        ExprKind::Let {
            is_const,
            name,
            type_params,
            ty,
            expr: init,
        } => crate::lower::stmt::lower_let_expr(
            e,
            *is_const,
            name,
            type_params,
            ty,
            init,
            ctx,
            b,
        ),
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => control::lower_if_expr(e, cond, then_br, else_br, ctx, b),
        ExprKind::Try {
            body,
            catch_name,
            catch_body,
        } => control::lower_try_expr(e, body, catch_name, catch_body, ctx, b),
        ExprKind::Match { subject, arms } => r#match::lower_match_expr(e, subject, arms, ctx, b),
        ExprKind::With {
            clauses,
            body,
            else_arms,
        } => r#match::lower_with_expr(e, clauses, body, else_arms, ctx, b),
        ExprKind::New { proto, args } => new_::lower_new_expr(e, proto, args, ctx, b),
        ExprKind::Fn { params, body, tail } => fn_::lower_fn_expr(e, params, body, tail, ctx, b),
        _ => Err(CompileError::new(
            ErrorKind::Codegen,
            e.span,
            "IR lowering: expression not implemented yet",
        )),
    }
}
