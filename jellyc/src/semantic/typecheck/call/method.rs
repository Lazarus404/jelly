use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::T_DYNAMIC;

use super::super::TypeChecker;

pub(super) fn try_type_method_sugar_call(
    tc: &mut TypeChecker,
    _e: &Expr,
    callee: &Expr,
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<Option<TypeId>, CompileError> {
    let ExprKind::Member { base, name: _ } = &callee.node else {
        return Ok(None);
    };

    // Don't steal module namespace calls (`Mod.f(...)`), which are typed via exports.
    if let ExprKind::Var(alias) = &base.node {
        if tc.is_module_alias(alias.as_str()) {
            return Ok(None);
        }
    }

    let tb = super::expect_object_kind(tc, base)?;
    if !tc.is_object_kind(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "method receiver must be Object",
        ));
    }

    let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
    for a in args {
        arg_tids.push(tc.check_expr(a, None)?);
    }

    // Return type comes from context when available; otherwise Dynamic.
    let ret_tid = expect.unwrap_or(T_DYNAMIC);
    let fun_tid = tc.intern_fun_type(ret_tid, &arg_tids);
    tc.record_expr(callee.span, fun_tid);
    Ok(Some(ret_tid))
}
