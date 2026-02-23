use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_DYNAMIC, T_OBJECT};

use super::TypeChecker;

pub(super) fn type_array_lit(
    tc: &mut TypeChecker,
    e: &Expr,
    elems: &[Expr],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    if elems.is_empty() {
        let Some(et) = expect else {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "empty array literal requires a type annotation",
            ));
        };
        if et != T_ARRAY_I32 && et != T_ARRAY_BYTES {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "empty array literal requires Array<I32> or Array<Bytes>",
            ));
        }
        return Ok(et);
    }
    let t0 = tc.check_expr(&elems[0], None)?;
    for el in &elems[1..] {
        let t = tc.check_expr(el, Some(t0))?;
        if t != t0 {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "array literal elements must have same type",
            ));
        }
    }
    match t0 {
        crate::typectx::T_I32 => Ok(T_ARRAY_I32),
        T_BYTES => Ok(T_ARRAY_BYTES),
        _ => Ok(T_DYNAMIC),
    }
}

pub(super) fn type_tuple_lit(tc: &mut TypeChecker, elems: &[Expr]) -> Result<TypeId, CompileError> {
    let mut ts: Vec<TypeId> = Vec::with_capacity(elems.len());
    for el in elems {
        ts.push(tc.check_expr(el, None)?);
    }
    Ok(tc.intern_tuple_type(&ts))
}

pub(super) fn type_obj_lit(
    tc: &mut TypeChecker,
    fields: &[(String, Expr)],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    for (_k, v) in fields {
        let _ = tc.check_expr(v, None)?;
    }
    if let Some(et) = expect {
        if tc.is_object_kind(et) && !tc.is_tuple_type(et) {
            return Ok(et);
        }
    }
    Ok(T_OBJECT)
}
