use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

use super::TypeChecker;

pub(super) fn type_member(
    tc: &mut TypeChecker,
    e: &Expr,
    base: &Expr,
    name: &str,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    // Builtin namespaces must be called, not extracted as values.
    // But allow shadowing (e.g. `import foo as Bytes` or `let Bytes = {...}`).
    if let ExprKind::Var(ns) = &base.node {
        let shadowed = tc.lookup(ns).is_some();
        if !shadowed
            && matches!(
                ns.as_str(),
                "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
            )
        {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "namespace members must be called (e.g. Bytes.len(x))",
            ));
        }
    }

    // Module namespace object: use declared export type if available.
    if let ExprKind::Var(alias) = &base.node {
        if let Some(exp_tid) = tc.module_export_tid(alias, name) {
            // Ensure the module alias itself is typed/recorded (lowering requires it).
            let _ = tc.check_expr(base, Some(T_OBJECT))?;
            let out_tid = Some(exp_tid).or(expect).unwrap_or(T_DYNAMIC);
            if let Some(et) = expect {
                return tc.coerce_type(out_tid, et, e.span);
            }
            return Ok(out_tid);
        }
    }

    // Regular object / nominal object kinds.
    // Preserve nominal object types, but allow `dynamic` to be treated as `object` here.
    let t_obj0 = tc.check_expr(base, None)?;
    let t_obj = if t_obj0 == T_DYNAMIC {
        tc.check_expr(base, Some(T_OBJECT))?
    } else {
        t_obj0
    };
    if !tc.is_object_kind(t_obj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "member access currently only supported for Object (obj.field)",
        ));
    }

    // Tuple element access: `t.0`.
    if tc.is_tuple_type(t_obj) {
        let idx: usize = name.parse().map_err(|_| {
            CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple element access must be .<index>",
            )
        })?;
        let elems = tc.tuple_elems(t_obj, e.span)?;
        if idx >= elems.len() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple index out of range",
            ));
        }
        let elem_tid = elems[idx];
        if let Some(et) = expect {
            return tc.coerce_type(elem_tid, et, e.span);
        }
        return Ok(elem_tid);
    }

    // Object property access yields Dynamic unless context requires typed unboxing.
    if let Some(et) = expect {
        if et != T_DYNAMIC {
            return Ok(et);
        }
    }
    Ok(T_DYNAMIC)
}
