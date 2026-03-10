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

use crate::ast::{Expr, ExprKind, MatchArm, Span, Stmt, StmtKind};

use super::Expander;

impl Expander {
    pub(super) fn shift_span(sp: Span, delta: i64) -> Span {
        if delta == 0 {
            return sp;
        }
        if delta > 0 {
            let d = delta as usize;
            Span {
                start: sp.start.saturating_add(d),
                end: sp.end.saturating_add(d),
            }
        } else {
            let d = (-delta) as usize;
            Span {
                start: sp.start.saturating_sub(d),
                end: sp.end.saturating_sub(d),
            }
        }
    }

    pub(super) fn shift_stmt_spans(&mut self, s: &mut Stmt, delta: i64) {
        s.span = Self::shift_span(s.span, delta);
        match &mut s.node {
            StmtKind::Let { ty, expr, .. } => {
                if let Some(t) = ty {
                    t.span = Self::shift_span(t.span, delta);
                }
                self.shift_expr_spans(expr, delta);
            }
            StmtKind::Assign { expr, .. } => self.shift_expr_spans(expr, delta),
            StmtKind::Expr { expr } => self.shift_expr_spans(expr, delta),
            StmtKind::While { cond, body } => {
                self.shift_expr_spans(cond, delta);
                for st in body {
                    self.shift_stmt_spans(st, delta);
                }
            }
            StmtKind::DoWhile { body, cond } => {
                for st in body {
                    self.shift_stmt_spans(st, delta);
                }
                self.shift_expr_spans(cond, delta);
            }
            StmtKind::Return { expr } => {
                if let Some(e) = expr {
                    self.shift_expr_spans(e, delta);
                }
            }
            StmtKind::Throw { expr } => self.shift_expr_spans(expr, delta),
            StmtKind::MemberAssign { base, expr, .. } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(expr, delta);
            }
            StmtKind::IndexAssign { base, index, expr } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(index, delta);
                self.shift_expr_spans(expr, delta);
            }
            StmtKind::ImportModule { .. }
            | StmtKind::ImportFrom { .. }
            | StmtKind::Prototype { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    pub(super) fn shift_arm_spans(&mut self, a: &mut MatchArm, delta: i64) {
        a.pat.span = Self::shift_span(a.pat.span, delta);
        // Patterns are nested; for now we only shift the top pattern span,
        // which is sufficient to avoid cross-specialization collisions.
        if let Some(w) = &mut a.when {
            self.shift_expr_spans(w, delta);
        }
        for st in &mut a.body {
            self.shift_stmt_spans(st, delta);
        }
        if let Some(t) = &mut a.tail {
            self.shift_expr_spans(t, delta);
        }
    }

    pub(super) fn shift_expr_spans(&mut self, e: &mut Expr, delta: i64) {
        e.span = Self::shift_span(e.span, delta);
        match &mut e.node {
            ExprKind::Member { base, .. } => self.shift_expr_spans(base, delta),
            ExprKind::Call {
                callee,
                args,
                type_args,
                ..
            } => {
                self.shift_expr_spans(callee, delta);
                for a in args {
                    self.shift_expr_spans(a, delta);
                }
                for ta in type_args {
                    ta.span = Self::shift_span(ta.span, delta);
                }
            }
            ExprKind::TypeApp { base, type_args } => {
                self.shift_expr_spans(base, delta);
                for ta in type_args {
                    ta.span = Self::shift_span(ta.span, delta);
                }
            }
            ExprKind::Index { base, index } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(index, delta);
            }
            ExprKind::IndexAssign { base, index, expr } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(index, delta);
                self.shift_expr_spans(expr, delta);
            }
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
                for el in elems {
                    self.shift_expr_spans(el, delta);
                }
            }
            ExprKind::ObjLit(fields) => {
                for (_k, v) in fields {
                    self.shift_expr_spans(v, delta);
                }
            }
            ExprKind::Fn { params, body, tail } => {
                for (_pn, ann) in params {
                    if let Some(t) = ann {
                        t.span = Self::shift_span(t.span, delta);
                    }
                }
                for st in body {
                    self.shift_stmt_spans(st, delta);
                }
                if let Some(t) = tail.as_deref_mut() {
                    self.shift_expr_spans(t, delta);
                }
            }
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => {
                self.shift_expr_spans(cond, delta);
                self.shift_expr_spans(then_br, delta);
                self.shift_expr_spans(else_br, delta);
            }
            ExprKind::Block { stmts, expr } => {
                for st in stmts {
                    self.shift_stmt_spans(st, delta);
                }
                self.shift_expr_spans(expr, delta);
            }
            ExprKind::Assign { expr, .. } => self.shift_expr_spans(expr, delta),
            ExprKind::MemberAssign { base, expr, .. } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(expr, delta);
            }
            ExprKind::Let { ty, expr, .. } => {
                if let Some(t) = ty {
                    t.span = Self::shift_span(t.span, delta);
                }
                self.shift_expr_spans(expr, delta);
            }
            ExprKind::Try {
                body, catch_body, ..
            } => {
                self.shift_expr_spans(body, delta);
                self.shift_expr_spans(catch_body, delta);
            }
            ExprKind::Match { subject, arms } => {
                self.shift_expr_spans(subject, delta);
                for a in arms {
                    self.shift_arm_spans(a, delta);
                }
            }
            ExprKind::With {
                clauses,
                body,
                else_arms,
            } => {
                for (pat, expr) in clauses {
                    pat.span = Self::shift_span(pat.span, delta);
                    self.shift_expr_spans(expr, delta);
                }
                self.shift_expr_spans(body, delta);
                if let Some(arms) = else_arms {
                    for a in arms {
                        self.shift_arm_spans(a, delta);
                    }
                }
            }
            ExprKind::New { proto, args } => {
                self.shift_expr_spans(proto, delta);
                for a in args {
                    self.shift_expr_spans(a, delta);
                }
            }
            ExprKind::Truthy(x) | ExprKind::Not(x) | ExprKind::Neg(x) => {
                self.shift_expr_spans(x, delta)
            }
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Mod(a, b)
            | ExprKind::Shl(a, b)
            | ExprKind::Shr(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b)
            | ExprKind::And(a, b)
            | ExprKind::Or(a, b) => {
                self.shift_expr_spans(a, delta);
                self.shift_expr_spans(b, delta);
            }
            ExprKind::BytesLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::I32Lit(_)
            | ExprKind::I8Lit(_)
            | ExprKind::I16Lit(_)
            | ExprKind::I64Lit(_)
            | ExprKind::F16Lit(_)
            | ExprKind::F64Lit(_)
            | ExprKind::AtomLit(_)
            | ExprKind::Null
            | ExprKind::Var(_) => {}
        }
    }
}
