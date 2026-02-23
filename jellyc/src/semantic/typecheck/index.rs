use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_I32};

use super::TypeChecker;

pub(super) fn type_index(
    tc: &mut TypeChecker,
    e: &Expr,
    base: &Expr,
    index: &Expr,
) -> Result<TypeId, CompileError> {
    let tb = tc.check_expr(base, None)?;
    let ti = tc.check_expr(index, Some(T_I32))?;
    let ti = tc.coerce_type(ti, T_I32, index.span)?;
    // Record the coerced type so lowering doesn't need contextual guessing.
    tc.record_expr(index.span, ti);
    match tb {
        T_ARRAY_I32 => Ok(T_I32),
        T_ARRAY_BYTES => Ok(T_BYTES),
        T_BYTES => Ok(T_I32),
        _ => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "indexing not supported for this type yet",
        )),
    }
}

pub(super) fn type_index_assign(
    tc: &mut TypeChecker,
    e: &Expr,
    base: &Expr,
    index: &Expr,
    expr: &Expr,
) -> Result<TypeId, CompileError> {
    let tb = tc.check_expr(base, None)?;
    let ti = tc.check_expr(index, Some(T_I32))?;
    let ti = tc.coerce_type(ti, T_I32, index.span)?;
    tc.record_expr(index.span, ti);
    let elem_tid = match tb {
        T_ARRAY_I32 => T_I32,
        T_ARRAY_BYTES => T_BYTES,
        T_BYTES => T_I32,
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "index assignment not supported for this type yet",
            ))
        }
    };
    let _ = tc.check_expr(expr, Some(elem_tid))?;
    Ok(elem_tid)
}
