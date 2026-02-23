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

use std::collections::{HashMap, HashSet};
use std::convert::Infallible;

use crate::ast::{Expr, ExprKind, Program, StmtKind};
use crate::error::CompileError;
use crate::hir::{NodeId, SemanticInfo};
use crate::typectx::{type_repr_from_ty, type_repr_from_typectx, TypeRepr};
use crate::visit::Visitor;

pub fn collect_import_keys_from_program(p: &Program) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for s in &p.stmts {
        let key = match &s.node {
            StmtKind::ImportModule { path, .. } => Some(path.join(".")),
            StmtKind::ImportFrom { from, .. } => Some(from.join(".")),
            _ => None,
        };
        if let Some(k) = key {
            if seen.insert(k.clone()) {
                out.push(k);
            }
        }
    }
    out
}

pub fn collect_exports_from_program(
    p: &Program,
) -> Result<HashMap<String, TypeRepr>, CompileError> {
    let mut out: HashMap<String, TypeRepr> = HashMap::new();
    for s in &p.stmts {
        if let StmtKind::Let {
            exported: true,
            name,
            ty: Some(ty),
            ..
        } = &s.node
        {
            out.insert(name.clone(), type_repr_from_ty(ty)?);
        }
    }
    Ok(out)
}

/// Collect all top-level let bindings (name → TypeRepr) from a REPL program.
/// Used for session context: prior bindings visible to the next incremental compile.
pub fn collect_repl_bindings(
    p: &Program,
    info: &SemanticInfo,
) -> Result<HashMap<String, TypeRepr>, CompileError> {
    let mut out: HashMap<String, TypeRepr> = HashMap::new();
    for s in &p.stmts {
        if let StmtKind::Let {
            name,
            ty,
            expr,
            ..
        } = &s.node
        {
            if name == "_" {
                continue;
            }
            let tr = if let Some(ann) = ty {
                type_repr_from_ty(ann)?
            } else if let Some(&tid) = info.expr_types.get(&NodeId(expr.span)) {
                type_repr_from_typectx(&info.type_ctx, tid)?
            } else {
                continue;
            };
            out.insert(name.clone(), tr);
        }
    }
    // Also collect from the program's final expression when it's a Let (e.g. "let i = 1 + 1").
    if let ExprKind::Let {
        name,
        ty,
        expr,
        ..
    } = &p.expr.node
    {
        if name != "_" {
            let tr = if let Some(ann) = ty {
                Some(type_repr_from_ty(ann)?)
            } else if let Some(&tid) = info.expr_types.get(&NodeId(expr.span)) {
                Some(type_repr_from_typectx(&info.type_ctx, tid)?)
            } else {
                None
            };
            if let Some(tr) = tr {
                out.insert(name.clone(), tr);
            }
        }
    }
    Ok(out)
}

/// Collect variable names defined (bound) in the program.
fn vars_defined_in_program(p: &Program) -> HashSet<String> {
    let mut out = HashSet::new();
    for s in &p.stmts {
        match &s.node {
            StmtKind::Let { name, .. } if name != "_" => {
                out.insert(name.clone());
            }
            StmtKind::Assign { name, .. } => {
                out.insert(name.clone());
            }
            StmtKind::ImportFrom { items, .. } => {
                for (name, as_name) in items {
                    let bind = as_name.as_deref().unwrap_or(name.as_str()).to_string();
                    out.insert(bind);
                }
            }
            StmtKind::ImportModule { alias, .. } => {
                out.insert(alias.clone());
            }
            _ => {}
        }
    }
    if let ExprKind::Let { name, .. } = &p.expr.node {
        if name != "_" {
            out.insert(name.clone());
        }
    }
    out
}

/// Collect variable names used (read) in the program.
fn vars_used_in_program(p: &Program) -> HashSet<String> {
    #[derive(Default)]
    struct Collector {
        out: HashSet<String>,
    }

    impl Visitor for Collector {
        type Err = Infallible;

        fn visit_expr(&mut self, e: &Expr) -> Result<(), Self::Err> {
            if let ExprKind::Var(n) = &e.node {
                self.out.insert(n.clone());
            }
            crate::visit::walk_expr(self, e)
        }
    }

    let mut c = Collector::default();
    c.visit_program(p).unwrap();
    c.out
}

/// Collect free variables: names used but not defined in the program.
/// These are names that must be in scope from the session (e.g. prior REPL bindings).
/// Note: Assignments are treated as "defining" for this purpose, so "i = i + 1" does not
/// import i. That causes incremental to fail and fall back to full compile, which preserves
/// the full session state (including post-assignment values in the exports).
pub fn collect_free_vars_in_program(p: &Program) -> HashSet<String> {
    let defined = vars_defined_in_program(p);
    let used = vars_used_in_program(p);
    used.into_iter().filter(|n| !defined.contains(n)).collect()
}
