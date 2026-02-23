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
use std::convert::Infallible;

use crate::ast::{Expr, ExprKind, Pattern, PatternKind, Program, Span, Stmt, StmtKind};
use crate::hir::{Capture, NodeId, SemanticInfo};
use crate::typectx::TypeCtx;
use crate::typectx::{T_DYNAMIC, T_OBJECT};
use crate::visit::Visitor;

pub(super) fn capture_analysis(p: &Program, info: &SemanticInfo) -> HashMap<NodeId, Vec<Capture>> {
    #[derive(Clone)]
    struct Analyzer<'a> {
        info: &'a SemanticInfo,
        scopes: Vec<HashMap<String, u32>>,
        outer_scopes_stack: Vec<Vec<HashMap<String, u32>>>,
        fn_span_stack: Vec<Span>,
        out: HashMap<NodeId, Vec<Capture>>,
    }

    impl<'a> Analyzer<'a> {
        fn new(info: &'a SemanticInfo) -> Self {
            Self {
                info,
                scopes: vec![HashMap::new()],
                outer_scopes_stack: Vec::new(),
                fn_span_stack: Vec::new(),
                out: HashMap::new(),
            }
        }

        fn lookup_local(&self, name: &str) -> Option<u32> {
            self.scopes.iter().rev().find_map(|s| s.get(name)).copied()
        }

        fn lookup_outer(&self, name: &str) -> Option<u32> {
            self.outer_scopes_stack
                .last()
                .and_then(|outer| outer.iter().rev().find_map(|s| s.get(name)).copied())
        }

        fn bind(&mut self, name: &str, tid: u32) {
            self.scopes
                .last_mut()
                .expect("scopes")
                .insert(name.to_string(), tid);
        }

        fn push_scope(&mut self) {
            self.scopes.push(HashMap::new());
        }

        fn pop_scope(&mut self) {
            self.scopes.pop();
            if self.scopes.is_empty() {
                self.scopes.push(HashMap::new());
            }
        }

        fn maybe_capture_use(&mut self, name: &str) {
            if self.lookup_local(name).is_some() {
                return;
            }
            let Some(cur_fn) = self.fn_span_stack.last().copied() else {
                return;
            };
            let Some(tid) = self.lookup_outer(name) else {
                return;
            };
            let caps = self.out.entry(NodeId(cur_fn)).or_default();
            if !caps.iter().any(|c| c.name == name) {
                caps.push(Capture {
                    name: name.to_string(),
                    tid,
                });
            }
        }

        fn bind_pattern(&mut self, p: &Pattern) {
            match &p.node {
                PatternKind::Bind(n) => self.bind(n, T_DYNAMIC),
                PatternKind::Obj(fields) => {
                    for (_k, v) in fields {
                        self.bind_pattern(v);
                    }
                }
                PatternKind::TupleExact(elems) | PatternKind::ArrayExact(elems) => {
                    for el in elems {
                        self.bind_pattern(el);
                    }
                }
                PatternKind::ArrayHeadTail { head, rest } => {
                    self.bind_pattern(head);
                    self.bind(rest, T_DYNAMIC);
                }
                PatternKind::ArrayPrefixRest { prefix, rest } => {
                    for el in prefix {
                        self.bind_pattern(el);
                    }
                    self.bind(rest, T_DYNAMIC);
                }
                PatternKind::Wildcard
                | PatternKind::BoolLit(_)
                | PatternKind::I8Lit(_)
                | PatternKind::I16Lit(_)
                | PatternKind::I32Lit(_)
                | PatternKind::Pin(_) => {}
            }
        }
    }

    impl<'a> Visitor for Analyzer<'a> {
        type Err = Infallible;

        fn visit_stmt(&mut self, s: &Stmt) -> Result<(), Self::Err> {
            match &s.node {
                StmtKind::ImportModule { alias, .. } => {
                    self.bind(alias, T_OBJECT);
                    Ok(())
                }
                StmtKind::ImportFrom { items, .. } => {
                    for (name, alias) in items {
                        let bind = alias.as_deref().unwrap_or(name.as_str());
                        // We don't have a first-class type environment here yet; default to Dynamic.
                        self.bind(bind, T_DYNAMIC);
                    }
                    Ok(())
                }
                StmtKind::Let { name, expr, .. } => {
                    // Mirror resolver behavior: allow recursion sugar by binding the name
                    // before analyzing the fn literal initializer.
                    let is_discard = name == "_";
                    if !is_discard && matches!(&expr.node, ExprKind::Fn { .. }) {
                        let tid = self
                            .info
                            .binding_types
                            .get(&NodeId(s.span))
                            .copied()
                            .unwrap_or(T_DYNAMIC);
                        self.bind(name, tid);
                        self.visit_expr(expr)?;
                    } else {
                        self.visit_expr(expr)?;
                        if !is_discard {
                            let tid = self
                                .info
                                .binding_types
                                .get(&NodeId(s.span))
                                .copied()
                                .unwrap_or(T_DYNAMIC);
                            self.bind(name, tid);
                        }
                    }
                    Ok(())
                }
                StmtKind::While { cond, body } => {
                    self.visit_expr(cond)?;
                    self.push_scope();
                    for st in body {
                        self.visit_stmt(st)?;
                    }
                    self.pop_scope();
                    Ok(())
                }
                StmtKind::Prototype { .. } | StmtKind::Break | StmtKind::Continue => Ok(()),
                _ => crate::visit::walk_stmt(self, s),
            }
        }

        fn visit_expr(&mut self, e: &Expr) -> Result<(), Self::Err> {
            match &e.node {
                ExprKind::Var(n) => {
                    self.maybe_capture_use(n);
                    Ok(())
                }
                ExprKind::Fn { params, body, tail } => {
                    self.fn_span_stack.push(e.span);
                    let saved_scopes = std::mem::take(&mut self.scopes);
                    self.outer_scopes_stack.push(saved_scopes.clone());
                    self.scopes = vec![HashMap::new()];

                    // Bind params in local scope (use annotations if present, else Dynamic).
                    let mut tc = TypeCtx::new_program_base();
                    for (pn, pty) in params {
                        let tid = pty
                            .as_ref()
                            .and_then(|t| tc.resolve_ty(t).ok())
                            .unwrap_or(T_DYNAMIC);
                        self.bind(pn, tid);
                    }

                    // Establish capture list for this fn literal.
                    self.out.entry(NodeId(e.span)).or_default();
                    for st in body {
                        self.visit_stmt(st)?;
                    }
                    if let Some(t) = tail {
                        self.visit_expr(t)?;
                    }

                    self.scopes = saved_scopes;
                    self.outer_scopes_stack.pop();
                    self.fn_span_stack.pop();
                    Ok(())
                }
                ExprKind::Block { stmts, expr } => {
                    self.push_scope();
                    for st in stmts {
                        self.visit_stmt(st)?;
                    }
                    self.visit_expr(expr)?;
                    self.pop_scope();
                    Ok(())
                }
                ExprKind::Try {
                    body,
                    catch_name,
                    catch_body,
                } => {
                    self.visit_expr(body)?;
                    self.push_scope();
                    if let Some(n) = catch_name {
                        self.bind(n, T_DYNAMIC);
                    }
                    self.visit_expr(catch_body)?;
                    self.pop_scope();
                    Ok(())
                }
                ExprKind::Let { name, expr, .. } => {
                    let is_discard = name == "_";
                    if !is_discard && matches!(&expr.node, ExprKind::Fn { .. }) {
                        let tid = self
                            .info
                            .binding_types
                            .get(&NodeId(e.span))
                            .copied()
                            .unwrap_or(T_DYNAMIC);
                        self.bind(name, tid);
                        self.visit_expr(expr)?;
                    } else {
                        self.visit_expr(expr)?;
                        if !is_discard {
                            let tid = self
                                .info
                                .binding_types
                                .get(&NodeId(e.span))
                                .copied()
                                .unwrap_or(T_DYNAMIC);
                            self.bind(name, tid);
                        }
                    }
                    Ok(())
                }
                ExprKind::Match { subject, arms } => {
                    self.visit_expr(subject)?;
                    for a in arms {
                        self.push_scope();
                        self.bind_pattern(&a.pat);
                        self.visit_pattern(&a.pat)?;
                        if let Some(w) = &a.when {
                            self.visit_expr(w)?;
                        }
                        for st in &a.body {
                            self.visit_stmt(st)?;
                        }
                        if let Some(t) = &a.tail {
                            self.visit_expr(t)?;
                        }
                        self.pop_scope();
                    }
                    Ok(())
                }
                ExprKind::With {
                    clauses,
                    body,
                    else_arms,
                } => {
                    self.push_scope();
                    for (pat, expr) in clauses {
                        self.bind_pattern(pat);
                        self.visit_pattern(pat)?;
                        self.visit_expr(expr)?;
                    }
                    self.visit_expr(body)?;
                    self.pop_scope();
                    if let Some(arms) = else_arms {
                        for a in arms {
                            self.push_scope();
                            self.bind_pattern(&a.pat);
                            self.visit_pattern(&a.pat)?;
                            if let Some(w) = &a.when {
                                self.visit_expr(w)?;
                            }
                            for st in &a.body {
                                self.visit_stmt(st)?;
                            }
                            if let Some(t) = &a.tail {
                                self.visit_expr(t)?;
                            }
                            self.pop_scope();
                        }
                    }
                    Ok(())
                }
                _ => crate::visit::walk_expr(self, e),
            }
        }

        fn visit_pattern(&mut self, p: &Pattern) -> Result<(), Self::Err> {
            match &p.node {
                PatternKind::Pin(n) => {
                    self.maybe_capture_use(n);
                    Ok(())
                }
                PatternKind::Obj(fields) => {
                    for (_k, v) in fields {
                        self.visit_pattern(v)?;
                    }
                    Ok(())
                }
                PatternKind::TupleExact(elems) | PatternKind::ArrayExact(elems) => {
                    for el in elems {
                        self.visit_pattern(el)?;
                    }
                    Ok(())
                }
                PatternKind::ArrayHeadTail { head, .. } => self.visit_pattern(head),
                PatternKind::ArrayPrefixRest { prefix, .. } => {
                    for el in prefix {
                        self.visit_pattern(el)?;
                    }
                    Ok(())
                }
                PatternKind::Wildcard
                | PatternKind::BoolLit(_)
                | PatternKind::I8Lit(_)
                | PatternKind::I16Lit(_)
                | PatternKind::I32Lit(_)
                | PatternKind::Bind(_) => Ok(()),
            }
        }
    }

    let mut a = Analyzer::new(info);
    a.visit_program(p).unwrap();
    a.out
}
