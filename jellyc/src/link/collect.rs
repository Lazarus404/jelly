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
pub fn collect_free_vars_in_program(p: &Program) -> HashSet<String> {
    let defined = vars_defined_in_program(p);
    let used = vars_used_in_program(p);
    used.into_iter().filter(|n| !defined.contains(n)).collect()
}
