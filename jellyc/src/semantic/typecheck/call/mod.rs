use crate::ast::{Expr, ExprKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

use super::{is_numeric, TypeChecker};

mod builtins;
mod method;

pub(super) fn type_call(
    tc: &mut TypeChecker,
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    // Builtin namespaces can be shadowed by imports/lets (e.g. `import m as Bytes`).
    // If shadowed, treat this as a normal call; don't apply builtin typing rules.
    let shadowed_ns = match &callee.node {
        ExprKind::Member { base, .. } => match &base.node {
            ExprKind::Var(ns) => tc.lookup(ns).is_some(),
            _ => false,
        },
        _ => false,
    };

    // Builtins + special-cased builtins (Object.set receiver type behavior).
    if let Some(ret) =
        builtins::try_type_builtin_like_call(tc, e, callee, type_args, args, expect, shadowed_ns)?
    {
        return Ok(ret);
    }

    // Method call sugar and module namespace calls are handled by typing the callee span.
    //
    // Method call sugar: `obj.m(args...)` where `obj` is an Object value.
    // We treat `obj.m` as a bound function value `(A...) -> R` for the purpose of typing the call.
    // Lowering is responsible for emitting `ObjGetAtom` + `BindThis` + `Call`.
    if let Some(ret) = method::try_type_method_sugar_call(tc, e, callee, args, expect)? {
        return Ok(ret);
    }

    // Otherwise, typecheck callee to discover a function type (possibly via module export).
    let callee_tid = tc.check_expr(callee, None)?;
    let (sig_args, sig_ret) = tc.fun_sig(callee_tid, e.span)?;
    if sig_args.len() != args.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "call arity mismatch",
        ));
    }
    for (i, a) in args.iter().enumerate() {
        let _ = tc.check_expr(a, Some(sig_args[i]))?;
    }
    Ok(sig_ret)
}

fn expect_object_kind(tc: &mut TypeChecker, recv: &Expr) -> Result<TypeId, CompileError> {
    let t0 = tc.check_expr(recv, None)?;
    let t = if t0 == T_DYNAMIC {
        tc.check_expr(recv, Some(T_OBJECT))?
    } else {
        t0
    };
    Ok(t)
}

fn is_numeric_tid(t: TypeId) -> bool {
    is_numeric(t)
}
