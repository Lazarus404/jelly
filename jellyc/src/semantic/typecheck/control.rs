use crate::ast::{Expr, Stmt};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::T_DYNAMIC;

use super::TypeChecker;

pub(super) fn type_if(
    tc: &mut TypeChecker,
    e: &Expr,
    cond: &Expr,
    then_br: &Expr,
    else_br: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let _ = tc.check_expr(cond, None)?;
    let tt = tc.check_expr(then_br, expect)?;
    let te = tc
        .check_expr(else_br, Some(tt))
        .or_else(|_| tc.check_expr(else_br, expect))?;
    if tt != te {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "if branches must have same type",
        ));
    }
    Ok(tt)
}

pub(super) fn type_block(
    tc: &mut TypeChecker,
    stmts: &[Stmt],
    expr: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    tc.with_scope(|tc| {
        for s in stmts {
            tc.check_stmt(s)?;
        }
        tc.check_expr(expr, expect)
    })
}

pub(super) fn type_try(
    tc: &mut TypeChecker,
    e: &Expr,
    body: &Expr,
    catch_name: &Option<String>,
    catch_body: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let tb = tc.check_expr(body, expect)?;
    let tc_t = tc.with_scope(|tcx| {
        if let Some(n) = catch_name {
            tcx.bind_local(n, T_DYNAMIC);
        }
        tcx.check_expr(catch_body, Some(tb))
            .or_else(|_| tcx.check_expr(catch_body, expect))
    })?;
    if tb != tc_t {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "try/catch branches must have same type",
        ));
    }
    Ok(tb)
}
