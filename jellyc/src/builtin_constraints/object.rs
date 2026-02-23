use crate::ast::{Span, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{
    TypeCtx, T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64,
    T_I16, T_I32, T_I64, T_I8, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

use super::{ArgConstraint, BuiltinConstraints};

pub(super) fn err(span: Span, msg: impl Into<String>) -> CompileError {
    CompileError::new(ErrorKind::Type, span, msg)
}

pub fn object_get_ret_tid(
    type_args: &[Ty],
    expect: Option<TypeId>,
    tc: &mut TypeCtx,
    span: Span,
) -> Result<TypeId, CompileError> {
    fn is_numeric_tid(tid: TypeId) -> bool {
        matches!(tid, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
    }

    fn is_object_kind_tid(tc: &TypeCtx, tid: TypeId) -> bool {
        if tc.is_tuple_type(tid) {
            return false;
        }
        tc.types
            .get(tid as usize)
            .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
    }

    fn is_supported_object_get_ret_tid(tc: &TypeCtx, tid: TypeId) -> bool {
        tid == T_DYNAMIC
            || tid == T_BOOL
            || tid == T_BYTES
            || tid == T_OBJECT
            || tid == T_ARRAY_I32
            || tid == T_ARRAY_BYTES
            || tid == T_LIST_I32
            || tid == T_LIST_BYTES
            || is_numeric_tid(tid)
            || is_object_kind_tid(tc, tid)
    }

    let tid = if type_args.is_empty() {
        expect.unwrap_or(T_DYNAMIC)
    } else if type_args.len() == 1 {
        tc.resolve_ty(&type_args[0])?
    } else {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "Object.get<T>(obj, key): expects 1 type arg",
        ));
    };

    if !is_supported_object_get_ret_tid(tc, tid) {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "Object.get cannot produce this type",
        ));
    }
    Ok(tid)
}

pub(super) fn namespace_constraints(
    name: &str,
    type_args: &[Ty],
    args_len: usize,
    expect: Option<TypeId>,
    tc: &mut TypeCtx,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    let no_targs = |what: &str| {
        if !type_args.is_empty() {
            return Err(err(span, format!("{what} does not take type arguments")));
        }
        Ok(())
    };

    match name {
        "get" => {
            if args_len != 2 {
                return Err(err(span, "Object.get expects 2 args"));
            }
            let ret = object_get_ret_tid(type_args, expect, tc, span)?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::ObjectKind, ArgConstraint::Exact(T_ATOM)],
                ret,
            }))
        }
        "set" => {
            no_targs("Object.set")?;
            if args_len != 3 {
                return Err(err(span, "Object.set expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::ObjectKind,
                    ArgConstraint::Exact(T_ATOM),
                    ArgConstraint::Any,
                ],
                // Lowering/semantic may preserve nominal object kinds here; allow the contextual
                // expected type to drive the return type when available.
                ret: expect.unwrap_or(T_OBJECT),
            }))
        }
        _ => Ok(None),
    }
}
