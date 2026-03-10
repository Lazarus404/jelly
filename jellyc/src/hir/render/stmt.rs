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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

use crate::ast::{Stmt, StmtKind};

use super::super::{type_name, NodeId, SemanticInfo};

pub(super) fn render_stmt(s: &Stmt, info: &SemanticInfo, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    let span = s.span;
    let bind_t = info.binding_types.get(&NodeId(span)).copied();
    match &s.node {
        StmtKind::Let {
            is_const,
            exported,
            name,
            ty,
            expr,
            ..
        } => {
            out.push_str(&format!(
                "{pad}{}{} {}{} @{}..{}{}\n",
                if *is_const { "const" } else { "let" },
                if *exported { " export" } else { "" },
                name,
                ty.as_ref()
                    .map(|t| format!(": {:?}", t.node))
                    .unwrap_or_default(),
                span.start,
                span.end,
                bind_t
                    .map(|t| format!(" : {}", type_name(t, &info.type_ctx)))
                    .unwrap_or_default()
            ));
            super::render_expr(expr, info, indent + 1, out);
        }
        StmtKind::Assign { name, expr } => {
            out.push_str(&format!(
                "{pad}assign {name} @{}..{}\n",
                span.start, span.end
            ));
            super::render_expr(expr, info, indent + 1, out);
        }
        StmtKind::Expr { expr } => {
            out.push_str(&format!("{pad}stmt_expr @{}..{}\n", span.start, span.end));
            super::render_expr(expr, info, indent + 1, out);
        }
        StmtKind::While { cond, body } => {
            out.push_str(&format!("{pad}while @{}..{}\n", span.start, span.end));
            out.push_str(&format!("{pad}  cond:\n"));
            super::render_expr(cond, info, indent + 2, out);
            out.push_str(&format!("{pad}  body:\n"));
            for st in body {
                render_stmt(st, info, indent + 2, out);
            }
        }
        StmtKind::DoWhile { body, cond } => {
            out.push_str(&format!("{pad}do_while @{}..{}\n", span.start, span.end));
            out.push_str(&format!("{pad}  body:\n"));
            for st in body {
                render_stmt(st, info, indent + 2, out);
            }
            out.push_str(&format!("{pad}  cond:\n"));
            super::render_expr(cond, info, indent + 2, out);
        }
        StmtKind::Return { expr } => {
            out.push_str(&format!("{pad}return @{}..{}\n", span.start, span.end));
            if let Some(e) = expr {
                super::render_expr(e, info, indent + 1, out);
            }
        }
        StmtKind::Break => out.push_str(&format!("{pad}break @{}..{}\n", span.start, span.end)),
        StmtKind::Continue => {
            out.push_str(&format!("{pad}continue @{}..{}\n", span.start, span.end))
        }
        StmtKind::Throw { expr } => {
            out.push_str(&format!("{pad}throw @{}..{}\n", span.start, span.end));
            super::render_expr(expr, info, indent + 1, out);
        }
        StmtKind::MemberAssign { base, name, expr } => {
            out.push_str(&format!(
                "{pad}member_assign .{name} @{}..{}\n",
                span.start, span.end
            ));
            super::render_expr(base, info, indent + 1, out);
            super::render_expr(expr, info, indent + 1, out);
        }
        StmtKind::IndexAssign { base, index, expr } => {
            out.push_str(&format!(
                "{pad}index_assign @{}..{}\n",
                span.start, span.end
            ));
            super::render_expr(base, info, indent + 1, out);
            super::render_expr(index, info, indent + 1, out);
            super::render_expr(expr, info, indent + 1, out);
        }
        StmtKind::ImportModule { path, alias } => {
            out.push_str(&format!(
                "{pad}import {} as {alias} @{}..{}\n",
                path.join("."),
                span.start,
                span.end
            ));
        }
        StmtKind::ImportFrom { from, items, .. } => {
            out.push_str(&format!(
                "{pad}import {{...}} from {} @{}..{}\n",
                from.join("."),
                span.start,
                span.end
            ));
            for (n, a) in items {
                out.push_str(&format!(
                    "{pad}  item {}{}\n",
                    n,
                    a.as_ref().map(|x| format!(" as {x}")).unwrap_or_default()
                ));
            }
        }
        StmtKind::Prototype { .. } => {
            out.push_str(&format!("{pad}prototype @{}..{}\n", span.start, span.end))
        }
    }
}
