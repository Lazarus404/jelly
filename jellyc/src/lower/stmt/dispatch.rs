use crate::ast::{Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::IrBuilder;

use super::super::LowerCtx;

mod assign;
mod import;
pub mod let_;
mod loop_;
mod term;

pub fn lower_stmt(
    s: &Stmt,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    match &s.node {
        StmtKind::ImportModule { path, alias } => {
            import::lower_import_module_stmt(s, path.join(".").as_str(), alias.as_str(), ctx)
        }
        StmtKind::ImportFrom {
            type_only: _,
            items,
            from,
        } => import::lower_import_from_stmt(s, items, from.join(".").as_str(), ctx, b),
        StmtKind::Prototype { .. } => Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "prototype must be expanded before lowering",
        )),
        StmtKind::Let {
            is_const,
            exported,
            name,
            type_params,
            ty,
            expr,
        } => let_::lower_let_stmt(
            s,
            *is_const,
            *exported,
            name.as_str(),
            type_params,
            ty,
            expr,
            ctx,
            b,
        ),
        StmtKind::Assign { name, expr } => {
            assign::lower_assign_stmt(s, name.as_str(), expr, ctx, b)
        }
        StmtKind::MemberAssign { base, name, expr } => {
            assign::lower_member_assign_stmt(s, base, name.as_str(), expr, ctx, b)
        }
        StmtKind::IndexAssign { base, index, expr } => {
            assign::lower_index_assign_stmt(s, base, index, expr, ctx, b)
        }
        StmtKind::While { cond, body } => loop_::lower_while_stmt(s, cond, body, ctx, b),
        StmtKind::Break => loop_::lower_break_stmt(s, ctx, b),
        StmtKind::Continue => loop_::lower_continue_stmt(s, ctx, b),
        StmtKind::Throw { expr } => term::lower_throw_stmt(s, expr, ctx, b),
        StmtKind::Return { expr } => term::lower_return_stmt(s, expr.as_ref(), ctx, b),
        StmtKind::Expr { expr } => term::lower_expr_stmt(s, expr, ctx, b),
    }
}
