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

use crate::ast::{Expr, ExprKind};

use super::super::{type_name, NodeId, SemanticInfo};
use super::helpers::{render_arm, render_bin};

pub(super) fn render_expr(e: &Expr, info: &SemanticInfo, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    let span = e.span;
    let ty = info.expr_types.get(&NodeId(span)).copied();
    let ty_s = ty
        .map(|t| type_name(t, &info.type_ctx))
        .unwrap_or_else(|| "?".to_string());

    match &e.node {
        ExprKind::BytesLit(_) => out.push_str(&format!(
            "{pad}BytesLit : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::BoolLit(v) => out.push_str(&format!(
            "{pad}BoolLit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::I32Lit(v) => out.push_str(&format!(
            "{pad}I32Lit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::I8Lit(v) => out.push_str(&format!(
            "{pad}I8Lit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::I16Lit(v) => out.push_str(&format!(
            "{pad}I16Lit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::I64Lit(v) => out.push_str(&format!(
            "{pad}I64Lit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::F16Lit(v) => out.push_str(&format!(
            "{pad}F16Lit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::F64Lit(v) => out.push_str(&format!(
            "{pad}F64Lit({v}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::AtomLit(n) => out.push_str(&format!(
            "{pad}AtomLit({n}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::Null => out.push_str(&format!(
            "{pad}Null : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::Var(n) => out.push_str(&format!(
            "{pad}Var({n}) : {ty_s} @{}..{}\n",
            span.start, span.end
        )),
        ExprKind::Member { base, name } => {
            out.push_str(&format!(
                "{pad}Member(.{name}) : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            render_expr(base, info, indent + 1, out);
        }
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => {
            out.push_str(&format!(
                "{pad}Call{} : {ty_s} @{}..{}\n",
                if type_args.is_empty() {
                    "".to_string()
                } else {
                    format!("<{}>", type_args.len())
                },
                span.start,
                span.end
            ));
            render_expr(callee, info, indent + 1, out);
            for a in args {
                render_expr(a, info, indent + 1, out);
            }
        }
        ExprKind::TypeApp { base, type_args } => {
            out.push_str(&format!(
                "{pad}TypeApp<{}> : {ty_s} @{}..{}\n",
                type_args.len(),
                span.start,
                span.end
            ));
            render_expr(base, info, indent + 1, out);
        }
        ExprKind::ArrayLit(elems) => {
            out.push_str(&format!(
                "{pad}ArrayLit({}) : {ty_s} @{}..{}\n",
                elems.len(),
                span.start,
                span.end
            ));
            for el in elems {
                render_expr(el, info, indent + 1, out);
            }
        }
        ExprKind::TupleLit(elems) => {
            out.push_str(&format!(
                "{pad}TupleLit({}) : {ty_s} @{}..{}\n",
                elems.len(),
                span.start,
                span.end
            ));
            for el in elems {
                render_expr(el, info, indent + 1, out);
            }
        }
        ExprKind::ObjLit(fields) => {
            out.push_str(&format!(
                "{pad}ObjLit({}) : {ty_s} @{}..{}\n",
                fields.len(),
                span.start,
                span.end
            ));
            for (k, v) in fields {
                out.push_str(&format!("{pad}  field {k}:\n"));
                render_expr(v, info, indent + 2, out);
            }
        }
        ExprKind::Index { base, index } => {
            out.push_str(&format!(
                "{pad}Index : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            render_expr(base, info, indent + 1, out);
            render_expr(index, info, indent + 1, out);
        }
        ExprKind::IndexAssign { base, index, expr } => {
            out.push_str(&format!(
                "{pad}IndexAssign : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            render_expr(base, info, indent + 1, out);
            render_expr(index, info, indent + 1, out);
            render_expr(expr, info, indent + 1, out);
        }
        ExprKind::Fn { params, body, tail } => {
            out.push_str(&format!(
                "{pad}Fn(params={}) : {ty_s} @{}..{}\n",
                params.len(),
                span.start,
                span.end
            ));
            if let Some(caps) = info.captures.get(&NodeId(span)) {
                if !caps.is_empty() {
                    out.push_str(&format!("{pad}  captures:\n"));
                    for c in caps {
                        out.push_str(&format!(
                            "{pad}    {} : {}\n",
                            c.name,
                            type_name(c.tid, &info.type_ctx)
                        ));
                    }
                }
            }
            for st in body {
                super::render_stmt(st, info, indent + 1, out);
            }
            if let Some(t) = tail {
                out.push_str(&format!("{pad}  tail:\n"));
                render_expr(t, info, indent + 2, out);
            }
        }
        ExprKind::Truthy(x) => {
            out.push_str(&format!(
                "{pad}Truthy : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            render_expr(x, info, indent + 1, out);
        }
        ExprKind::Not(x) => {
            out.push_str(&format!(
                "{pad}Not : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            render_expr(x, info, indent + 1, out);
        }
        ExprKind::Neg(x) => {
            out.push_str(&format!(
                "{pad}Neg : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            render_expr(x, info, indent + 1, out);
        }
        ExprKind::Add(a, b) => render_bin("Add", a, b, span, ty_s, info, indent, out),
        ExprKind::Sub(a, b) => render_bin("Sub", a, b, span, ty_s, info, indent, out),
        ExprKind::Mul(a, b) => render_bin("Mul", a, b, span, ty_s, info, indent, out),
        ExprKind::Div(a, b) => render_bin("Div", a, b, span, ty_s, info, indent, out),
        ExprKind::Mod(a, b) => render_bin("Mod", a, b, span, ty_s, info, indent, out),
        ExprKind::Shl(a, b) => render_bin("Shl", a, b, span, ty_s, info, indent, out),
        ExprKind::Shr(a, b) => render_bin("Shr", a, b, span, ty_s, info, indent, out),
        ExprKind::Eq(a, b) => render_bin("Eq", a, b, span, ty_s, info, indent, out),
        ExprKind::Ne(a, b) => render_bin("Ne", a, b, span, ty_s, info, indent, out),
        ExprKind::Lt(a, b) => render_bin("Lt", a, b, span, ty_s, info, indent, out),
        ExprKind::Le(a, b) => render_bin("Le", a, b, span, ty_s, info, indent, out),
        ExprKind::Gt(a, b) => render_bin("Gt", a, b, span, ty_s, info, indent, out),
        ExprKind::Ge(a, b) => render_bin("Ge", a, b, span, ty_s, info, indent, out),
        ExprKind::And(a, b) => render_bin("And", a, b, span, ty_s, info, indent, out),
        ExprKind::Or(a, b) => render_bin("Or", a, b, span, ty_s, info, indent, out),
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => {
            out.push_str(&format!("{pad}If : {ty_s} @{}..{}\n", span.start, span.end));
            out.push_str(&format!("{pad}  cond:\n"));
            render_expr(cond, info, indent + 2, out);
            out.push_str(&format!("{pad}  then:\n"));
            render_expr(then_br, info, indent + 2, out);
            out.push_str(&format!("{pad}  else:\n"));
            render_expr(else_br, info, indent + 2, out);
        }
        ExprKind::Assign { name, expr: init } => {
            out.push_str(&format!("{pad}Assign({name} = "));
            render_expr(init, info, indent + 1, out);
            out.push_str(")");
        }
        ExprKind::MemberAssign { base, name, expr: init } => {
            out.push_str(&format!("{pad}MemberAssign("));
            render_expr(base, info, indent + 1, out);
            out.push_str(&format!(".{name} = "));
            render_expr(init, info, indent + 1, out);
            out.push_str(")");
        }
        ExprKind::Let { name, expr: init, .. } => {
            out.push_str(&format!(
                "{pad}Let({name}) : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            out.push_str(&format!("{pad}  init:\n"));
            render_expr(init, info, indent + 2, out);
        }
        ExprKind::Block { stmts, expr } => {
            out.push_str(&format!(
                "{pad}Block : {ty_s} @{}..{}\n",
                span.start, span.end
            ));
            for st in stmts {
                super::render_stmt(st, info, indent + 1, out);
            }
            render_expr(expr, info, indent + 1, out);
        }
        ExprKind::Try {
            body,
            catch_name,
            catch_body,
        } => {
            out.push_str(&format!(
                "{pad}Try(catch={}) : {ty_s} @{}..{}\n",
                catch_name.as_deref().unwrap_or("_"),
                span.start,
                span.end
            ));
            render_expr(body, info, indent + 1, out);
            render_expr(catch_body, info, indent + 1, out);
        }
        ExprKind::Match { subject, arms } => {
            out.push_str(&format!(
                "{pad}Match(arms={}) : {ty_s} @{}..{}\n",
                arms.len(),
                span.start,
                span.end
            ));
            render_expr(subject, info, indent + 1, out);
            for a in arms {
                render_arm(a, info, indent + 1, out);
            }
        }
        ExprKind::With {
            clauses,
            body,
            else_arms,
        } => {
            let else_len = else_arms.as_ref().map(|a| a.len()).unwrap_or(0);
            out.push_str(&format!(
                "{pad}With(clauses={}, else={}) : {ty_s} @{}..{}\n",
                clauses.len(),
                else_len,
                span.start,
                span.end
            ));
            for (_pat, expr) in clauses {
                render_expr(expr, info, indent + 1, out);
            }
            render_expr(body, info, indent + 1, out);
            if let Some(arms) = else_arms {
                for a in arms {
                    render_arm(a, info, indent + 1, out);
                }
            }
        }
        ExprKind::New { proto, args } => {
            out.push_str(&format!(
                "{pad}New(args={}) : {ty_s} @{}..{}\n",
                args.len(),
                span.start,
                span.end
            ));
            render_expr(proto, info, indent + 1, out);
            for a in args {
                render_expr(a, info, indent + 1, out);
            }
        }
    }
}
