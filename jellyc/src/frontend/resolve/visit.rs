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

use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Pattern, PatternKind, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::visit::Visitor;

use super::Resolver;

impl Visitor for Resolver {
    type Err = CompileError;

    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), Self::Err> {
        match &s.node {
            StmtKind::ImportModule { alias, .. } => {
                if self.fn_depth != 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "import is only allowed at module top-level",
                    ));
                }
                // Allow imports to shadow builtins (eg `import math as Math` overrides builtin Math)
                if self.builtins.contains_key(alias) {
                    self.scopes
                        .last_mut()
                        .expect("scopes")
                        .insert(alias.to_string(), s.span);
                } else {
                    self.define_no_shadow(alias, s.span)?;
                }
                Ok(())
            }
            StmtKind::ImportFrom {
                type_only, items, ..
            } => {
                if self.fn_depth != 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "import is only allowed at module top-level",
                    ));
                }
                if *type_only {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "type imports are reserved (user-defined types not implemented yet)",
                    ));
                }
                for (name, alias) in items {
                    let bind = alias.as_ref().unwrap_or(name);
                    if self.builtins.contains_key(bind) {
                        self.scopes
                            .last_mut()
                            .expect("scopes")
                            .insert(bind.to_string(), s.span);
                    } else {
                        self.define_no_shadow(bind, s.span)?;
                    }
                }
                Ok(())
            }
            StmtKind::Prototype { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "prototype must be expanded before resolution",
            )),
            StmtKind::Let {
                exported,
                name,
                ty,
                expr,
                ..
            } => {
                if *exported && self.fn_depth != 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "export is only allowed at module top-level",
                    ));
                }
                if *exported && ty.is_none() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "export let requires an explicit type annotation",
                    ));
                }

                // Initializer cannot see the newly bound name, except for
                // `let f = fn(...) { ... f(...) ... }` recursion sugar.
                // `_` is a discard pattern: multiple `let _ = expr` are allowed.
                if name == "_" {
                    self.visit_expr(expr)
                } else if matches!(&expr.node, ExprKind::Fn { .. }) {
                    self.define_no_shadow(name, s.span)?;
                    self.visit_expr(expr)
                } else {
                    self.visit_expr(expr)?;
                    self.define_no_shadow(name, s.span)?;
                    Ok(())
                }
            }
            StmtKind::Assign { name, expr } => {
                self.require_defined(name, s.span)?;
                self.visit_expr(expr)
            }
            StmtKind::MemberAssign { base, .. } => {
                // Current parser only allows `name.field = expr;`. Keep this explicit so
                // "invalid assignment target" has a consistent diagnostic as we extend.
                if !matches!(base.node, ExprKind::Var(_)) {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        base.span,
                        "invalid assignment target",
                    ));
                }
                crate::visit::walk_stmt(self, s)
            }
            StmtKind::IndexAssign { base, .. } => {
                // Current parser only allows `name[index] = expr;`. Keep this explicit so
                // "invalid assignment target" has a consistent diagnostic as we extend.
                if !matches!(base.node, ExprKind::Var(_)) {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        base.span,
                        "invalid assignment target",
                    ));
                }
                crate::visit::walk_stmt(self, s)
            }
            StmtKind::While { cond, body } => {
                self.visit_expr(cond)?;
                self.loop_depth += 1;
                self.with_scope(|r| {
                    for st in body {
                        r.visit_stmt(st)?;
                    }
                    Ok(())
                })?;
                self.loop_depth -= 1;
                Ok(())
            }
            StmtKind::DoWhile { body, cond } => {
                self.loop_depth += 1;
                self.with_scope(|r| {
                    for st in body {
                        r.visit_stmt(st)?;
                    }
                    Ok(())
                })?;
                self.visit_expr(cond)?;
                self.loop_depth -= 1;
                Ok(())
            }
            StmtKind::Break => {
                if self.loop_depth == 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "break used outside of loop",
                    ));
                }
                Ok(())
            }
            StmtKind::Continue => {
                if self.loop_depth == 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "continue used outside of loop",
                    ));
                }
                Ok(())
            }
            StmtKind::Return { expr } => {
                if self.fn_depth == 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "return used outside of fn body",
                    ));
                }
                if let Some(e) = expr {
                    self.visit_expr(e)?;
                }
                Ok(())
            }
            StmtKind::Throw { .. } | StmtKind::Expr { .. } => crate::visit::walk_stmt(self, s),
        }
    }

    fn visit_expr(&mut self, e: &Expr) -> Result<(), Self::Err> {
        match &e.node {
            ExprKind::Var(name) => self.require_defined(name, e.span),
            ExprKind::Member { base, name } => {
                self.visit_expr(base)?;
                self.validate_builtin_member(base, name, e.span)?;
                Ok(())
            }
            ExprKind::Fn { params, body, tail } => {
                // No captures yet: functions see only their params and locals.
                let saved_scopes = std::mem::take(&mut self.scopes);
                let saved_loop = self.loop_depth;
                let saved_fn = self.fn_depth;

                self.outer_scopes_stack.push(saved_scopes.clone());
                self.scopes = vec![HashMap::new()];
                self.loop_depth = 0;
                self.fn_depth = saved_fn + 1;

                for (pn, _pty) in params {
                    self.define_no_shadow(pn, e.span)?;
                }
                for st in body {
                    self.visit_stmt(st)?;
                }
                if let Some(t) = tail {
                    self.visit_expr(t)?;
                }

                self.scopes = saved_scopes;
                self.loop_depth = saved_loop;
                self.fn_depth = saved_fn;
                self.outer_scopes_stack.pop();
                Ok(())
            }
            ExprKind::Assign { name, expr: init } => {
                self.require_defined(name, e.span)?;
                self.visit_expr(init)
            }
            ExprKind::MemberAssign { base, expr: init, .. } => {
                self.visit_expr(base)?;
                self.visit_expr(init)
            }
            ExprKind::Let { name, expr: init, .. } => {
                // let-as-expression (e.g. if (let x = 42) { x }): define name after init so it's visible in enclosing scope
                if *name == "_" {
                    self.visit_expr(init)
                } else if matches!(&init.node, ExprKind::Fn { .. }) {
                    self.define_no_shadow(name, e.span)?;
                    self.visit_expr(init)
                } else {
                    self.visit_expr(init)?;
                    self.define_no_shadow(name, e.span)?;
                    Ok(())
                }
            }
            ExprKind::Block { stmts, expr } => self.with_scope(|r| {
                for st in stmts {
                    r.visit_stmt(st)?;
                }
                r.visit_expr(expr)?;
                Ok(())
            }),
            ExprKind::Try {
                body,
                catch_name,
                catch_body,
            } => {
                self.visit_expr(body)?;
                self.with_scope(|r| {
                    if let Some(n) = catch_name {
                        r.define_no_shadow(n, e.span)?;
                    }
                    r.visit_expr(catch_body)?;
                    Ok(())
                })
            }
            ExprKind::With {
                clauses,
                body,
                else_arms,
            } => self.with_scope(|r| {
                for (pat, expr) in clauses {
                    r.visit_pattern(pat)?;
                    r.visit_expr(expr)?;
                }
                r.visit_expr(body)?;
                if let Some(arms) = else_arms {
                    for a in arms {
                        r.visit_match_arm(a)?;
                    }
                }
                Ok(())
            }),
            _ => crate::visit::walk_expr(self, e),
        }
    }

    fn visit_match_arm(&mut self, a: &crate::ast::MatchArm) -> Result<(), Self::Err> {
        // `match` arms get their own scope so bindings don't leak across arms.
        self.with_scope(|r| {
            // Bindings are definitions; pins are uses. Resolve the pattern first.
            r.visit_pattern(&a.pat)?;
            if let Some(w) = &a.when {
                r.visit_expr(w)?;
            }
            for st in &a.body {
                r.visit_stmt(st)?;
            }
            if let Some(t) = &a.tail {
                r.visit_expr(t)?;
            }
            Ok(())
        })
    }

    fn visit_pattern(&mut self, p: &Pattern) -> Result<(), Self::Err> {
        match &p.node {
            PatternKind::Bind(name) => self.define_no_shadow(name, p.span),
            PatternKind::Pin(name) => self.require_defined(name, p.span),
            PatternKind::ArrayHeadTail { head, rest } => {
                self.visit_pattern(head)?;
                self.define_no_shadow(rest, p.span)
            }
            PatternKind::ArrayPrefixRest { prefix, rest } => {
                for el in prefix {
                    self.visit_pattern(el)?;
                }
                self.define_no_shadow(rest, p.span)
            }
            _ => crate::visit::walk_pattern(self, p),
        }
    }
}
