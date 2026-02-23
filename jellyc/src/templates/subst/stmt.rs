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

use std::collections::HashMap;

use crate::ast::{Stmt, StmtKind, Ty};

use super::expr::subst_expr;
use super::ty::subst_ty;

/// Substitute a statement with a substitution.
pub(in crate::templates::subst) fn subst_stmt(s: &Stmt, subst: &HashMap<String, Ty>) -> Stmt {
    let node = match &s.node {
        StmtKind::Let {
            is_const,
            exported,
            name,
            type_params,
            ty,
            expr,
        } => StmtKind::Let {
            is_const: *is_const,
            exported: *exported,
            name: name.clone(),
            type_params: type_params.clone(),
            ty: ty.as_ref().map(|t| subst_ty(t, subst)),
            expr: subst_expr(expr, subst),
        },
        StmtKind::Prototype {
            exported,
            name,
            type_params,
            fields,
        } => StmtKind::Prototype {
            exported: *exported,
            name: name.clone(),
            type_params: type_params.clone(),
            fields: fields
                .iter()
                .map(|(k, v)| (k.clone(), subst_expr(v, subst)))
                .collect(),
        },
        StmtKind::ImportModule { path, alias } => StmtKind::ImportModule {
            path: path.clone(),
            alias: alias.clone(),
        },
        StmtKind::ImportFrom {
            type_only,
            items,
            from,
        } => StmtKind::ImportFrom {
            type_only: *type_only,
            items: items.clone(),
            from: from.clone(),
        },
        StmtKind::Assign { name, expr } => StmtKind::Assign {
            name: name.clone(),
            expr: subst_expr(expr, subst),
        },
        StmtKind::MemberAssign { base, name, expr } => StmtKind::MemberAssign {
            base: subst_expr(base, subst),
            name: name.clone(),
            expr: subst_expr(expr, subst),
        },
        StmtKind::IndexAssign { base, index, expr } => StmtKind::IndexAssign {
            base: subst_expr(base, subst),
            index: subst_expr(index, subst),
            expr: subst_expr(expr, subst),
        },
        StmtKind::While { cond, body } => StmtKind::While {
            cond: subst_expr(cond, subst),
            body: body.iter().map(|st| subst_stmt(st, subst)).collect(),
        },
        StmtKind::Break => StmtKind::Break,
        StmtKind::Continue => StmtKind::Continue,
        StmtKind::Throw { expr } => StmtKind::Throw {
            expr: subst_expr(expr, subst),
        },
        StmtKind::Return { expr } => StmtKind::Return {
            expr: expr.as_ref().map(|e| subst_expr(e, subst)),
        },
        StmtKind::Expr { expr } => StmtKind::Expr {
            expr: subst_expr(expr, subst),
        },
    };
    Stmt::new(node, s.span)
}
