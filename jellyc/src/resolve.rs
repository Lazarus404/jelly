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

use crate::ast::{Expr, ExprKind, Pattern, PatternKind, Program, Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};

/// Resolver for the compiler.
/// This is used to resolve names in the program.
struct Resolver {
    builtins: HashMap<String, Span>,
    scopes: Vec<HashMap<String, Span>>,
    loop_depth: usize,
    fn_depth: usize,
    // When resolving a function literal, we intentionally hide outer scopes (no captures yet).
    // However, we still want to detect and report captures cleanly.
    outer_scopes_stack: Vec<Vec<HashMap<String, Span>>>,
}

impl Resolver {
    fn new() -> Self {
        // Namespaces (for namespaced builtins)
        const BUILTINS: &[&str] = &[
            "System",
            "Bytes",
            "Integer",
            "Float",
            "Math",
            "Atom",
            "Object",
            "Array",
            "List",
        ];

        let mut builtins = HashMap::new();
        for &name in BUILTINS {
            builtins.insert(name.to_string(), Span::point(0));
        }
        Self {
            builtins,
            scopes: vec![HashMap::new()],
            loop_depth: 0,
            fn_depth: 0,
            outer_scopes_stack: Vec::new(),
        }
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

    fn is_defined(&self, name: &str) -> bool {
        self.builtins.contains_key(name) || self.scopes.iter().rev().any(|s| s.contains_key(name))
    }

    /// Define a name in the current scope, without shadowing any existing bindings.
    fn define_no_shadow(&mut self, name: &str, span: Span) -> Result<(), CompileError> {
        if self.builtins.contains_key(name) || self.scopes.iter().any(|s| s.contains_key(name)) {
            return Err(CompileError::new(
                ErrorKind::Name,
                span,
                format!("duplicate let binding '{}'", name),
            ));
        }
        self.scopes
            .last_mut()
            .expect("scopes")
            .insert(name.to_string(), span);
        Ok(())
    }

    /// Require a name to be defined in the current scope.
    fn require_defined(&self, name: &str, span: Span) -> Result<(), CompileError> {
        // If it's defined in the current environment (including builtins), it's fine.
        if self.is_defined(name) {
            return Ok(());
        }
        // Otherwise, if we're inside a fn literal and the name exists in outer scopes, it's a
        // capture (handled by the lowering/backend).
        if let Some(outer) = self.outer_scopes_stack.last() {
            if outer.iter().rev().any(|s| s.contains_key(name)) {
                return Ok(());
            }
        }

        Err(CompileError::new(
            ErrorKind::Name,
            span,
            format!("unknown variable '{}'", name),
        ))
    }

    fn resolve_pattern(&mut self, p: &Pattern) -> Result<(), CompileError> {
        match &p.node {
            PatternKind::Wildcard
                | PatternKind::BoolLit(_)
                | PatternKind::I8Lit(_)
                | PatternKind::I16Lit(_)
                | PatternKind::I32Lit(_) => Ok(()),
            PatternKind::Bind(name) => self.define_no_shadow(name, p.span),
            PatternKind::Pin(name) => self.require_defined(name, p.span),
            PatternKind::Obj(fields) => {
                for (_k, v) in fields {
                    self.resolve_pattern(v)?;
                }
                Ok(())
            }
            PatternKind::TupleExact(elems) => {
                for el in elems {
                    self.resolve_pattern(el)?;
                }
                Ok(())
            }
            PatternKind::ArrayExact(elems) => {
                for el in elems {
                    self.resolve_pattern(el)?;
                }
                Ok(())
            }
            PatternKind::ArrayHeadTail { head, rest } => {
                self.resolve_pattern(head)?;
                self.define_no_shadow(rest, p.span)
            }
            PatternKind::ArrayPrefixRest { prefix, rest } => {
                for el in prefix {
                    self.resolve_pattern(el)?;
                }
                self.define_no_shadow(rest, p.span)
            }
        }
    }

    fn resolve_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
        match &s.node {
            StmtKind::ImportModule { path: _, alias } => {
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
                type_only,
                items,
                from: _,
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
                    self.resolve_expr(expr)
                } else if matches!(&expr.node, ExprKind::Fn { .. }) {
                    self.define_no_shadow(name, s.span)?;
                    self.resolve_expr(expr)
                } else {
                    self.resolve_expr(expr)?;
                    self.define_no_shadow(name, s.span)?;
                    Ok(())
                }
            }
            StmtKind::Assign { name, expr } => {
                self.require_defined(name, s.span)?;
                self.resolve_expr(expr)
            }
            StmtKind::MemberAssign { base, name: _name, expr } => {
                // Current parser only allows `name.field = expr;`. Keep this explicit so
                // "invalid assignment target" has a consistent diagnostic as we extend.
                if !matches!(base.node, ExprKind::Var(_)) {
                    return Err(CompileError::new(ErrorKind::Name, base.span, "invalid assignment target"));
                }
                self.resolve_expr(base)?;
                self.resolve_expr(expr)
            }
            StmtKind::IndexAssign { base, index, expr } => {
                // Current parser only allows `name[index] = expr;`. Keep this explicit so
                // "invalid assignment target" has a consistent diagnostic as we extend.
                if !matches!(base.node, ExprKind::Var(_)) {
                    return Err(CompileError::new(ErrorKind::Name, base.span, "invalid assignment target"));
                }
                self.resolve_expr(base)?;
                self.resolve_expr(index)?;
                self.resolve_expr(expr)
            }
            StmtKind::While { cond, body } => {
                self.resolve_expr(cond)?;
                self.loop_depth += 1;
                self.push_scope();
                for st in body {
                    self.resolve_stmt(st)?;
                }
                self.pop_scope();
                self.loop_depth -= 1;
                Ok(())
            }
            StmtKind::Break => {
                if self.loop_depth == 0 {
                    return Err(CompileError::new(ErrorKind::Name, s.span, "break used outside of while"));
                }
                Ok(())
            }
            StmtKind::Continue => {
                if self.loop_depth == 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "continue used outside of while",
                    ));
                }
                Ok(())
            }
            StmtKind::Throw { expr } => self.resolve_expr(expr),
            StmtKind::Return { expr } => {
                if self.fn_depth == 0 {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        "return used outside of fn body",
                    ));
                }
                if let Some(e) = expr {
                    self.resolve_expr(e)?;
                }
                Ok(())
            }
            StmtKind::Expr { expr } => self.resolve_expr(expr),
        }
    }

    fn resolve_expr(&mut self, e: &Expr) -> Result<(), CompileError> {
        match &e.node {
            ExprKind::BytesLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::I32Lit(_)
            | ExprKind::I8Lit(_)
            | ExprKind::I16Lit(_)
            | ExprKind::F16Lit(_)
            | ExprKind::I64Lit(_)
            | ExprKind::F64Lit(_)
            | ExprKind::AtomLit(_)
            | ExprKind::Null => Ok(()),
            ExprKind::Var(name) => self.require_defined(name, e.span),
            ExprKind::Member { base, name } => {
                self.resolve_expr(base)?;

                // If this is a builtin namespace (and not shadowed by import/let), validate the member name.
                if let ExprKind::Var(ns) = &base.node {
                    // If ns is in scopes (eg from `import math as Math`), treat as normal property access.
                    let shadowed = self.scopes.iter().rev().any(|s| s.contains_key(ns));
                    if !shadowed {
                        let ok = match ns.as_str() {
                            "Bytes" => matches!(name.as_str(), "new" | "len" | "get_u8" | "set_u8" | "slice" | "eq"),
                            "Array" => matches!(name.as_str(), "new" | "len" | "get" | "set"),
                            "List" => matches!(name.as_str(), "nil" | "cons" | "head" | "tail" | "is_nil"),
                            "Atom" => matches!(name.as_str(), "intern"),
                            "Object" => matches!(name.as_str(), "get" | "set"),
                            "System" => matches!(name.as_str(), "assert"),
                            "Integer" => matches!(name.as_str(), "to_i8" | "to_i16" | "to_i32" | "to_i64"),
                            "Float" => matches!(name.as_str(), "to_f16" | "to_f32" | "to_f64"),
                            "Math" => matches!(name.as_str(), "sqrt"),
                            _ => true,
                        };
                        if !ok {
                            return Err(CompileError::new(
                                ErrorKind::Name,
                                e.span,
                                format!("unknown builtin '{}.{}'", ns, name),
                            ));
                        }
                    }
                }

                Ok(())
            }
            ExprKind::Call { callee, type_args: _type_args, args } => {
                self.resolve_expr(callee)?;
                for a in args {
                    self.resolve_expr(a)?;
                }
                Ok(())
            }
            ExprKind::TypeApp { base, type_args: _type_args } => self.resolve_expr(base),
            ExprKind::ArrayLit(elems) => {
                for el in elems {
                    self.resolve_expr(el)?;
                }
                Ok(())
            }
            ExprKind::TupleLit(elems) => {
                for el in elems {
                    self.resolve_expr(el)?;
                }
                Ok(())
            }
            ExprKind::ObjLit(fields) => {
                for (_k, v) in fields {
                    self.resolve_expr(v)?;
                }
                Ok(())
            }
            ExprKind::Index { base, index } => {
                self.resolve_expr(base)?;
                self.resolve_expr(index)
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
                    self.resolve_stmt(st)?;
                }
                if let Some(t) = tail {
                    self.resolve_expr(t)?;
                }

                self.scopes = saved_scopes;
                self.loop_depth = saved_loop;
                self.fn_depth = saved_fn;
                self.outer_scopes_stack.pop();
                Ok(())
            }
            ExprKind::Truthy(x) => self.resolve_expr(x),
            ExprKind::Not(x) | ExprKind::Neg(x) => self.resolve_expr(x),
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b)
            | ExprKind::And(a, b)
            | ExprKind::Or(a, b) => {
                self.resolve_expr(a)?;
                self.resolve_expr(b)
            }
            ExprKind::If { cond, then_br, else_br } => {
                self.resolve_expr(cond)?;
                self.resolve_expr(then_br)?;
                self.resolve_expr(else_br)
            }
            ExprKind::Block { stmts, expr } => {
                self.push_scope();
                for st in stmts {
                    self.resolve_stmt(st)?;
                }
                self.resolve_expr(expr)?;
                self.pop_scope();
                Ok(())
            }
            ExprKind::Try {
                body,
                catch_name,
                catch_body,
            } => {
                self.resolve_expr(body)?;
                self.push_scope();
                if let Some(n) = catch_name {
                    self.define_no_shadow(n, e.span)?;
                }
                self.resolve_expr(catch_body)?;
                self.pop_scope();
                Ok(())
            }
            ExprKind::Match { subject, arms } => {
                self.resolve_expr(subject)?;
                for a in arms {
                    self.push_scope();
                    self.resolve_pattern(&a.pat)?;
                    if let Some(w) = &a.when {
                        self.resolve_expr(w)?;
                    }
                    for st in &a.body {
                        self.resolve_stmt(st)?;
                    }
                    if let Some(t) = &a.tail {
                        self.resolve_expr(t)?;
                    }
                    self.pop_scope();
                }
                Ok(())
            }
            ExprKind::New { proto, args } => {
                self.resolve_expr(proto)?;
                for a in args {
                    self.resolve_expr(a)?;
                }
                Ok(())
            }
        }
    }
}

pub fn resolve_program(p: &Program) -> Result<(), CompileError> {
    let mut r = Resolver::new();
    for s in &p.stmts {
        r.resolve_stmt(s)?;
    }
    r.resolve_expr(&p.expr)?;
    Ok(())
}

