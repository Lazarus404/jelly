use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_BOOL, T_BYTES, T_DYNAMIC, T_F32, T_F64};

use super::{is_numeric, join_numeric, TypeChecker};

pub(super) fn type_add(
    tc: &mut TypeChecker,
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    if let Some(et) = expect {
        if et == T_BYTES {
            let ta = tc.check_expr(a, Some(T_BYTES))?;
            let tb = tc.check_expr(b, None)?;
            // bytes + bytes or bytes + numeric (numeric is converted to bytes for interpolation)
            if ta != T_BYTES {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "'+' expects bytes or numeric operands",
                ));
            }
            if tb != T_BYTES && !is_numeric(tb) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "'+' expects bytes or numeric operands",
                ));
            }
            return Ok(T_BYTES);
        }
        if is_numeric(et) {
            let ta = tc.check_expr(a, Some(et))?;
            let tb = tc.check_expr(b, Some(et))?;
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "'+' expects numeric operands",
                ));
            }
            return Ok(et);
        }
    }

    let ta = tc.check_expr(a, None)?;
    // If `ta` is Dynamic, avoid passing `expect=Dynamic` to `b`, since that can
    // "downcast" a more precise type (e.g. Bytes/I32) back to Dynamic and break
    // operator typing/coercions.
    let tb = if ta == T_DYNAMIC {
        tc.check_expr(b, None)?
    } else {
        tc.check_expr(b, Some(ta))
            .or_else(|_| tc.check_expr(b, None))?
    };
    if ta == T_BYTES && tb == T_BYTES {
        Ok(T_BYTES)
    } else if is_numeric(ta) && is_numeric(tb) {
        Ok(join_numeric(ta, tb))
    } else if ta == T_DYNAMIC && tb == T_BYTES {
        let ta2 = tc.check_expr(a, Some(T_BYTES))?;
        if ta2 == T_BYTES {
            Ok(T_BYTES)
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "'+' expects bytes operands",
            ))
        }
    } else if tb == T_DYNAMIC && ta == T_BYTES {
        let tb2 = tc.check_expr(b, Some(T_BYTES))?;
        if tb2 == T_BYTES {
            Ok(T_BYTES)
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "'+' expects bytes operands",
            ))
        }
    } else if ta == T_DYNAMIC && is_numeric(tb) {
        let ta2 = tc.check_expr(a, Some(tb))?;
        if is_numeric(ta2) {
            Ok(join_numeric(ta2, tb))
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "'+' expects numeric operands",
            ))
        }
    } else if tb == T_DYNAMIC && is_numeric(ta) {
        let tb2 = tc.check_expr(b, Some(ta))?;
        if is_numeric(tb2) {
            Ok(join_numeric(ta, tb2))
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "'+' expects numeric operands",
            ))
        }
    } else if ta == T_BYTES && is_numeric(tb) {
        // bytes + numeric: numeric is converted to bytes (e.g. for template interpolation)
        Ok(T_BYTES)
    } else if tb == T_BYTES && is_numeric(ta) {
        // numeric + bytes: numeric is converted to bytes
        Ok(T_BYTES)
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'+' expects bytes or numeric operands",
        ))
    }
}

pub(super) fn type_sub_mul(
    tc: &mut TypeChecker,
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    if let Some(et) = expect {
        if is_numeric(et) {
            let ta = tc.check_expr(a, Some(et))?;
            let tb = tc.check_expr(b, Some(et))?;
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "numeric operator expects numeric operands",
                ));
            }
            return Ok(et);
        }
    }

    let ta = tc.check_expr(a, None)?;
    let tb = if ta == T_DYNAMIC {
        tc.check_expr(b, None)?
    } else {
        tc.check_expr(b, Some(ta))
            .or_else(|_| tc.check_expr(b, None))?
    };
    if is_numeric(ta) && is_numeric(tb) {
        Ok(join_numeric(ta, tb))
    } else if ta == T_DYNAMIC && is_numeric(tb) {
        let ta2 = tc.check_expr(a, Some(tb))?;
        if is_numeric(ta2) {
            Ok(join_numeric(ta2, tb))
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "numeric operator expects numeric operands",
            ))
        }
    } else if tb == T_DYNAMIC && is_numeric(ta) {
        let tb2 = tc.check_expr(b, Some(ta))?;
        if is_numeric(tb2) {
            Ok(join_numeric(ta, tb2))
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "numeric operator expects numeric operands",
            ))
        }
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "numeric operator expects numeric operands",
        ))
    }
}

pub(super) fn type_div(
    tc: &mut TypeChecker,
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    // If the surrounding context expects a float, push that expectation into both
    // operands so Dynamic object properties can be typed/unboxed mechanically later.
    if expect == Some(T_F64) {
        let ta = tc.check_expr(a, Some(T_F64))?;
        let tb = tc.check_expr(b, Some(T_F64))?;
        if !is_numeric(ta) || !is_numeric(tb) {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "'/' expects numeric operands",
            ));
        }
        return Ok(T_F64);
    }
    if expect == Some(T_F32) {
        let ta = tc.check_expr(a, Some(T_F32))?;
        let tb = tc.check_expr(b, Some(T_F32))?;
        if !is_numeric(ta) || !is_numeric(tb) {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "'/' expects numeric operands",
            ));
        }
        return Ok(T_F32);
    }

    let ta = tc.check_expr(a, None)?;
    let tb = tc
        .check_expr(b, Some(ta))
        .or_else(|_| tc.check_expr(b, None))?;
    if !is_numeric(ta) || !is_numeric(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'/' expects numeric operands",
        ));
    }
    // Division always produces a float.
    Ok(if ta == T_F64 || tb == T_F64 {
        T_F64
    } else {
        T_F32
    })
}

pub(super) fn type_eq_ne(
    tc: &mut TypeChecker,
    e: &Expr,
    a: &Expr,
    b: &Expr,
) -> Result<TypeId, CompileError> {
    let mut ta = tc.check_expr(a, None)?;
    // Avoid pushing `expect=Dynamic` into the other operand: that would eagerly coerce
    // concrete types (like I32 literals) into Dynamic, which then prevents the
    // Dynamic→numeric pinning logic below from firing.
    let mut tb = if ta == T_DYNAMIC {
        tc.check_expr(b, None)?
    } else {
        tc.check_expr(b, Some(ta))
            .or_else(|_| tc.check_expr(b, None))?
    };

    // If one side is Dynamic and the other is a concrete type, try to pin the Dynamic side
    // to the concrete type so lowering can be mechanical (typed member unboxing, etc).
    //
    // Keep this conservative: do not try to "coerce" a Bool (Bool comparisons are special-cased below).
    if ta == T_DYNAMIC && tb != T_DYNAMIC && tb != T_BOOL {
        let ta2 = tc.check_expr(a, Some(tb))?;
        if ta2 == tb {
            ta = tb;
        }
    } else if tb == T_DYNAMIC && ta != T_DYNAMIC && ta != T_BOOL {
        let tb2 = tc.check_expr(b, Some(ta))?;
        if tb2 == ta {
            tb = ta;
        }
    }

    // Keep current semantics: allow bool comparisons against any type.
    if ta == T_BOOL || tb == T_BOOL {
        Ok(T_BOOL)
    } else if is_numeric(ta) && is_numeric(tb) {
        Ok(T_BOOL)
    } else if ta == tb {
        Ok(T_BOOL)
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'==' expects operands of same type",
        ))
    }
}

pub(super) fn type_rel(
    tc: &mut TypeChecker,
    e: &Expr,
    a: &Expr,
    b: &Expr,
) -> Result<TypeId, CompileError> {
    let mut ta = tc.check_expr(a, None)?;
    // Same rule as equality: don't erase concrete types by pushing expect=Dynamic.
    let mut tb = if ta == T_DYNAMIC {
        tc.check_expr(b, None)?
    } else {
        tc.check_expr(b, Some(ta))
            .or_else(|_| tc.check_expr(b, None))?
    };

    // If one side is Dynamic but the other is numeric, push the numeric expectation into
    // the Dynamic side (eg. object property access) so lowering doesn't need heuristics.
    if ta == T_DYNAMIC && is_numeric(tb) {
        let ta2 = tc.check_expr(a, Some(tb))?;
        if is_numeric(ta2) {
            ta = ta2;
        }
    } else if tb == T_DYNAMIC && is_numeric(ta) {
        let tb2 = tc.check_expr(b, Some(ta))?;
        if is_numeric(tb2) {
            tb = tb2;
        }
    }

    if is_numeric(ta) && is_numeric(tb) {
        Ok(T_BOOL)
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "relational op expects numeric operands",
        ))
    }
}

pub(super) fn type_and_or(
    tc: &mut TypeChecker,
    a: &Expr,
    b: &Expr,
) -> Result<TypeId, CompileError> {
    // Jelly truthiness: operands need not be `Bool` (everything except `null` and
    // `false` is truthy). Lowering will insert truthiness conversion if needed.
    let _ = tc.check_expr(a, None)?;
    let _ = tc.check_expr(b, None)?;
    Ok(T_BOOL)
}
