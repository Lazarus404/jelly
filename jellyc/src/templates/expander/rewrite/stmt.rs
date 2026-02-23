use crate::ast::{Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};

use super::super::Expander;

impl Expander {
    pub(in crate::templates) fn rewrite_stmt(&mut self, s: &Stmt) -> Result<Stmt, CompileError> {
        let node = match &s.node {
            StmtKind::Let {
                is_const: _,
                exported,
                name,
                type_params,
                ty,
                expr,
            } => {
                if !type_params.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "internal: template let must be removed before rewriting statements",
                    ));
                }
                if let Some(t) = ty {
                    self.known_vars.insert(name.clone(), t.clone());
                }
                StmtKind::Let {
                    is_const: false,
                    exported: *exported,
                    name: name.clone(),
                    type_params: Vec::new(),
                    ty: ty.clone(),
                    expr: self.rewrite_expr(expr)?,
                }
            }
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
            StmtKind::Prototype { .. } => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "internal: prototype must be desugared before rewriting statements",
                ));
            }
            StmtKind::Assign { name, expr } => StmtKind::Assign {
                name: name.clone(),
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::MemberAssign { base, name, expr } => StmtKind::MemberAssign {
                base: self.rewrite_expr(base)?,
                name: name.clone(),
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::IndexAssign { base, index, expr } => StmtKind::IndexAssign {
                base: self.rewrite_expr(base)?,
                index: self.rewrite_expr(index)?,
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::While { cond, body } => StmtKind::While {
                cond: self.rewrite_expr(cond)?,
                body: body
                    .iter()
                    .map(|st| self.rewrite_stmt(st))
                    .collect::<Result<_, _>>()?,
            },
            StmtKind::Break => StmtKind::Break,
            StmtKind::Continue => StmtKind::Continue,
            StmtKind::Throw { expr } => StmtKind::Throw {
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::Return { expr } => StmtKind::Return {
                expr: match expr {
                    Some(e) => Some(self.rewrite_expr(e)?),
                    None => None,
                },
            },
            StmtKind::Expr { expr } => StmtKind::Expr {
                expr: self.rewrite_expr(expr)?,
            },
        };
        Ok(Stmt::new(node, s.span))
    }
}
