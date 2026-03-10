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

use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Ty};

use super::arm::subst_arm;
use super::stmt::subst_stmt;
use super::ty::subst_ty;

/// Substitute an expression with a substitution.
pub(in crate::templates) fn subst_expr(e: &Expr, subst: &HashMap<String, Ty>) -> Expr {
    let node = match &e.node {
        ExprKind::BytesLit(b) => ExprKind::BytesLit(b.clone()),
        ExprKind::BoolLit(x) => ExprKind::BoolLit(*x),
        ExprKind::I32Lit(x) => ExprKind::I32Lit(*x),
        ExprKind::I8Lit(x) => ExprKind::I8Lit(*x),
        ExprKind::I16Lit(x) => ExprKind::I16Lit(*x),
        ExprKind::I64Lit(x) => ExprKind::I64Lit(*x),
        ExprKind::F16Lit(x) => ExprKind::F16Lit(*x),
        ExprKind::F64Lit(x) => ExprKind::F64Lit(*x),
        ExprKind::AtomLit(n) => ExprKind::AtomLit(n.clone()),
        ExprKind::Null => ExprKind::Null,
        ExprKind::Var(n) => ExprKind::Var(n.clone()),
        ExprKind::Member { base, name } => ExprKind::Member {
            base: Box::new(subst_expr(base, subst)),
            name: name.clone(),
        },
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => ExprKind::Call {
            callee: Box::new(subst_expr(callee, subst)),
            type_args: type_args.iter().map(|t| subst_ty(t, subst)).collect(),
            args: args.iter().map(|a| subst_expr(a, subst)).collect(),
        },
        ExprKind::TypeApp { base, type_args } => ExprKind::TypeApp {
            base: Box::new(subst_expr(base, subst)),
            type_args: type_args.iter().map(|t| subst_ty(t, subst)).collect(),
        },
        ExprKind::ArrayLit(elems) => {
            ExprKind::ArrayLit(elems.iter().map(|a| subst_expr(a, subst)).collect())
        }
        ExprKind::TupleLit(elems) => {
            ExprKind::TupleLit(elems.iter().map(|a| subst_expr(a, subst)).collect())
        }
        ExprKind::ObjLit(fields) => ExprKind::ObjLit(
            fields
                .iter()
                .map(|(k, v)| (k.clone(), subst_expr(v, subst)))
                .collect(),
        ),
        ExprKind::Index { base, index } => ExprKind::Index {
            base: Box::new(subst_expr(base, subst)),
            index: Box::new(subst_expr(index, subst)),
        },
        ExprKind::IndexAssign { base, index, expr } => ExprKind::IndexAssign {
            base: Box::new(subst_expr(base, subst)),
            index: Box::new(subst_expr(index, subst)),
            expr: Box::new(subst_expr(expr, subst)),
        },
        ExprKind::Fn { params, body, tail } => ExprKind::Fn {
            params: params
                .iter()
                .map(|(n, t)| (n.clone(), t.as_ref().map(|ty| subst_ty(ty, subst))))
                .collect(),
            body: body.iter().map(|s| subst_stmt(s, subst)).collect(),
            tail: tail.as_ref().map(|t| Box::new(subst_expr(t, subst))),
        },
        ExprKind::Truthy(a) => ExprKind::Truthy(Box::new(subst_expr(a, subst))),
        ExprKind::Not(a) => ExprKind::Not(Box::new(subst_expr(a, subst))),
        ExprKind::Neg(a) => ExprKind::Neg(Box::new(subst_expr(a, subst))),
        ExprKind::Add(a, b) => ExprKind::Add(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Sub(a, b) => ExprKind::Sub(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Mul(a, b) => ExprKind::Mul(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Div(a, b) => ExprKind::Div(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Mod(a, b) => ExprKind::Mod(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Shl(a, b) => ExprKind::Shl(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Shr(a, b) => ExprKind::Shr(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Eq(a, b) => ExprKind::Eq(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Ne(a, b) => ExprKind::Ne(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Lt(a, b) => ExprKind::Lt(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Le(a, b) => ExprKind::Le(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Gt(a, b) => ExprKind::Gt(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Ge(a, b) => ExprKind::Ge(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::And(a, b) => ExprKind::And(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::Or(a, b) => ExprKind::Or(
            Box::new(subst_expr(a, subst)),
            Box::new(subst_expr(b, subst)),
        ),
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => ExprKind::If {
            cond: Box::new(subst_expr(cond, subst)),
            then_br: Box::new(subst_expr(then_br, subst)),
            else_br: Box::new(subst_expr(else_br, subst)),
        },
        ExprKind::Block { stmts, expr } => ExprKind::Block {
            stmts: stmts.iter().map(|s| subst_stmt(s, subst)).collect(),
            expr: Box::new(subst_expr(expr, subst)),
        },
        ExprKind::Assign { name, expr } => ExprKind::Assign {
            name: name.clone(),
            expr: Box::new(subst_expr(expr, subst)),
        },
        ExprKind::MemberAssign { base, name, expr } => ExprKind::MemberAssign {
            base: Box::new(subst_expr(base, subst)),
            name: name.clone(),
            expr: Box::new(subst_expr(expr, subst)),
        },
        ExprKind::Let {
            is_const,
            name,
            type_params,
            ty,
            expr: init,
        } => ExprKind::Let {
            is_const: *is_const,
            name: name.clone(),
            type_params: type_params.clone(),
            ty: ty.as_ref().map(|t| subst_ty(t, subst)),
            expr: Box::new(subst_expr(init, subst)),
        },
        ExprKind::Try {
            body,
            catch_name,
            catch_body,
        } => ExprKind::Try {
            body: Box::new(subst_expr(body, subst)),
            catch_name: catch_name.clone(),
            catch_body: Box::new(subst_expr(catch_body, subst)),
        },
        ExprKind::Match { subject, arms } => ExprKind::Match {
            subject: Box::new(subst_expr(subject, subst)),
            arms: arms.iter().map(|a| subst_arm(a, subst)).collect(),
        },
        ExprKind::With {
            clauses,
            body,
            else_arms,
        } => ExprKind::With {
            clauses: clauses
                .iter()
                .map(|(pat, expr)| (pat.clone(), subst_expr(expr, subst)))
                .collect(),
            body: Box::new(subst_expr(body, subst)),
            else_arms: else_arms
                .as_ref()
                .map(|arms| arms.iter().map(|a| subst_arm(a, subst)).collect()),
        },
        ExprKind::New { proto, args } => ExprKind::New {
            proto: Box::new(subst_expr(proto, subst)),
            args: args.iter().map(|a| subst_expr(a, subst)).collect(),
        },
    };
    Expr::new(node, e.span)
}
