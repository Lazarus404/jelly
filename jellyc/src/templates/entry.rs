use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, ExprKind, Program, Stmt, StmtKind, Ty, TyKind};
use crate::error::{CompileError, ErrorKind};

use super::expander::{Expander, TemplateDef};

/// Expand templates in a program.
/// This is used to expand templates in a program into concrete specialized types and expressions.
/// This is the main entry point for template expansion.
pub fn expand_templates(prog: &mut Program) -> Result<(), CompileError> {
    // First, desugar `prototype Name<T> { ... }` into `let Name<T>: Object = {...};`
    let mut desugared: Vec<Stmt> = Vec::with_capacity(prog.stmts.len());
    for s in &prog.stmts {
        match &s.node {
            StmtKind::Prototype {
                exported,
                name,
                type_params,
                fields,
            } => {
                let obj_ty = if type_params.is_empty() {
                    Ty::new(TyKind::Named(name.clone()), s.span)
                } else {
                    let args: Vec<Ty> = type_params
                        .iter()
                        .map(|tp| Ty::new(TyKind::Named(tp.clone()), s.span))
                        .collect();
                    Ty::new(
                        TyKind::Generic {
                            base: name.clone(),
                            args,
                        },
                        s.span,
                    )
                };
                let obj_expr = Expr::new(ExprKind::ObjLit(fields.clone()), s.span);
                desugared.push(Stmt::new(
                    StmtKind::Let {
                        is_const: false,
                        exported: *exported,
                        name: name.clone(),
                        type_params: type_params.clone(),
                        ty: Some(obj_ty),
                        expr: obj_expr,
                    },
                    s.span,
                ));
            }
            _ => desugared.push(s.clone()),
        }
    }
    prog.stmts = desugared;

    // Collect template definitions (top-level only for MVP).
    let mut templates: HashMap<String, TemplateDef> = HashMap::new();
    for s in &prog.stmts {
        if let StmtKind::Let {
            is_const: _,
            exported,
            name,
            type_params,
            ty,
            expr,
        } = &s.node
        {
            if type_params.is_empty() {
                continue;
            }
            if *exported {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "exporting templated lets is not supported yet (export a concrete specialization instead)",
                ));
            }
            let ty = ty.clone().ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "templated let requires an explicit type annotation (MVP limitation)",
                )
            })?;
            templates.insert(
                name.clone(),
                TemplateDef {
                    span: s.span,
                    name: name.clone(),
                    type_params: type_params.clone(),
                    ty,
                    expr: expr.clone(),
                },
            );
        }
    }

    if templates.is_empty() {
        return Ok(());
    }

    let mut ex = Expander {
        templates,
        emitted: HashSet::new(),
        stack: Vec::new(),
        out_specs: Vec::new(),
        known_vars: HashMap::new(),
        tmp_counter: 0,
    };

    // Rewrite runtime stmts and expression, instantiating templates on demand.
    let mut out_runtime: Vec<Stmt> = Vec::new();
    for s in &prog.stmts {
        match &s.node {
            StmtKind::Let { type_params, .. } if !type_params.is_empty() => {
                // Drop template definitions from runtime.
            }
            StmtKind::Prototype { .. } => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "internal: prototype must be desugared before template expansion",
                ));
            }
            _ => out_runtime.push(ex.rewrite_stmt(s)?),
        }
    }
    let out_expr = ex.rewrite_expr(&prog.expr)?;

    // Emit specializations before runtime code (MVP).
    let mut all: Vec<Stmt> = Vec::new();
    all.extend(ex.out_specs);
    all.extend(out_runtime);
    prog.stmts = all;
    prog.expr = out_expr;
    Ok(())
}
