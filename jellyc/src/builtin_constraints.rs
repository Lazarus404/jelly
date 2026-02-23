use crate::ast::{Expr, ExprKind, Span, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::TypeCtx;
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8, T_LIST_BYTES,
    T_LIST_I32, T_OBJECT,
};

#[derive(Clone, Copy, Debug)]
pub enum ArgConstraint {
    Exact(TypeId),
    Numeric,
    Any,
}

#[derive(Clone, Debug)]
pub struct BuiltinConstraints {
    pub args: Vec<ArgConstraint>,
    pub ret: TypeId,
}

fn builtin_name(callee: &Expr) -> Option<(&str, &str)> {
    match &callee.node {
        ExprKind::Var(n) => Some(("", n.as_str())),
        ExprKind::Member { base, name } => match &base.node {
            ExprKind::Var(ns) => Some((ns.as_str(), name.as_str())),
            _ => None,
        },
        _ => None,
    }
}

pub fn builtin_constraints(
    callee: &Expr,
    type_args: &[Ty],
    args_len: usize,
    expect: Option<TypeId>,
    tc: &mut TypeCtx,
    allow_ambiguous_generics: bool,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    let Some((ns, name)) = builtin_name(callee) else {
        return Ok(None);
    };

    fn err(span: Span, msg: impl Into<String>) -> CompileError {
        CompileError::new(ErrorKind::Type, span, msg)
    }

    let no_targs = |what: &str| {
        if !type_args.is_empty() {
            return Err(err(span, format!("{what} does not take type arguments")));
        }
        Ok(())
    };

    match (ns, name) {
        ("System", "assert") => {
            no_targs("assert")?;
            if args_len != 1 {
                return Err(err(span, "assert expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_BOOL)], ret: T_BOOL }))
        }
        ("Integer", "to_i8") => {
            no_targs("Integer.to_i8")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I8 }))
        }
        ("Integer", "to_i16") => {
            no_targs("Integer.to_i16")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I16 }))
        }
        ("Integer", "to_i32") => {
            no_targs("Integer.to_i32")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I32 }))
        }
        ("Integer", "to_i64") => {
            no_targs("Integer.to_i64")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I64 }))
        }
        ("Float", "to_f16") => {
            no_targs("Float.to_f16")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F16 }))
        }
        ("Float", "to_f32") => {
            no_targs("Float.to_f32")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F32 }))
        }
        ("Float", "to_f64") => {
            no_targs("Float.to_f64")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F64 }))
        }
        ("Math", "sqrt") => {
            no_targs("Math.sqrt")?;
            if args_len != 1 {
                return Err(err(span, "Math.sqrt expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F64 }))
        }
        ("Bytes", "new") => {
            no_targs("Bytes.new")?;
            if args_len != 1 {
                return Err(err(span, "Bytes.new expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_I32)], ret: T_BYTES }))
        }
        ("Bytes", "len") => {
            no_targs("Bytes.len")?;
            if args_len != 1 {
                return Err(err(span, "Bytes.len expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_BYTES)], ret: T_I32 }))
        }
        ("Bytes", "get_u8") => {
            no_targs("Bytes.get_u8")?;
            if args_len != 2 {
                return Err(err(span, "Bytes.get_u8 expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_I32)],
                ret: T_I32,
            }))
        }
        ("Bytes", "set_u8") => {
            no_targs("Bytes.set_u8")?;
            if args_len != 3 {
                return Err(err(span, "Bytes.set_u8 expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_I32), ArgConstraint::Exact(T_I32)],
                ret: T_BYTES,
            }))
        }
        ("Bytes", "slice") => {
            no_targs("Bytes.slice")?;
            if args_len != 3 {
                return Err(err(span, "Bytes.slice expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_I32), ArgConstraint::Exact(T_I32)],
                ret: T_BYTES,
            }))
        }
        ("Bytes", "eq") => {
            no_targs("Bytes.eq")?;
            if args_len != 2 {
                return Err(err(span, "Bytes.eq expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_BYTES)],
                ret: T_BOOL,
            }))
        }
        ("Atom", "intern") => {
            no_targs("Atom.intern")?;
            if args_len != 1 {
                return Err(err(span, "Atom.intern expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_BYTES)], ret: T_ATOM }))
        }
        ("Object", "get") => {
            if args_len != 2 {
                return Err(err(span, "Object.get expects 2 args"));
            }
            let ret = if type_args.is_empty() {
                expect.unwrap_or(T_DYNAMIC)
            } else if type_args.len() == 1 {
                tc.resolve_ty(&type_args[0])?
            } else {
                return Err(err(span, "Object.get<T>(obj, key): expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_OBJECT), ArgConstraint::Exact(T_ATOM)],
                ret,
            }))
        }
        ("Object", "set") => {
            no_targs("Object.set")?;
            if args_len != 3 {
                return Err(err(span, "Object.set expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_OBJECT), ArgConstraint::Exact(T_ATOM), ArgConstraint::Any],
                ret: T_OBJECT,
            }))
        }
        ("Array", "new") => {
            if args_len != 1 {
                return Err(err(span, "Array.new expects 1 arg"));
            }
            let ret = if type_args.is_empty() {
                if allow_ambiguous_generics {
                    expect.unwrap_or(T_DYNAMIC)
                } else {
                    match expect {
                        Some(t) if t == T_ARRAY_I32 || t == T_ARRAY_BYTES => t,
                        _ => return Err(err(span, "Array.new<T>(len): missing type argument")),
                    }
                }
            } else if type_args.len() == 1 {
                let elem = tc.resolve_ty(&type_args[0])?;
                match elem {
                    T_I32 => T_ARRAY_I32,
                    T_BYTES => T_ARRAY_BYTES,
                    _ => {
                        if allow_ambiguous_generics {
                            T_DYNAMIC
                        } else {
                            return Err(err(span, "only Array<I32> and Array<Bytes> supported for now"));
                        }
                    }
                }
            } else {
                return Err(err(span, "Array.new expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_I32)], ret }))
        }
        ("Array", "len") => {
            no_targs("Array.len")?;
            if args_len != 1 {
                return Err(err(span, "Array.len expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_I32 }))
        }
        ("Array", "get") => {
            no_targs("Array.get")?;
            if args_len != 2 {
                return Err(err(span, "Array.get expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Exact(T_I32)],
                ret: T_DYNAMIC,
            }))
        }
        ("Array", "set") => {
            no_targs("Array.set")?;
            if args_len != 3 {
                return Err(err(span, "Array.set expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Exact(T_I32), ArgConstraint::Any],
                ret: T_DYNAMIC,
            }))
        }
        ("List", "nil") => {
            if args_len != 0 {
                return Err(err(span, "List.nil expects 0 args"));
            }
            let ret = if type_args.is_empty() {
                if allow_ambiguous_generics {
                    expect.unwrap_or(T_DYNAMIC)
                } else {
                    match expect {
                        Some(t) if t == T_LIST_I32 || t == T_LIST_BYTES => t,
                        _ => return Err(err(span, "List.nil<T>(): missing type argument")),
                    }
                }
            } else if type_args.len() == 1 {
                let elem = tc.resolve_ty(&type_args[0])?;
                match elem {
                    T_I32 => T_LIST_I32,
                    T_BYTES => T_LIST_BYTES,
                    _ => {
                        if allow_ambiguous_generics {
                            T_DYNAMIC
                        } else {
                            return Err(err(span, "only List<I32> and List<Bytes> supported for now"));
                        }
                    }
                }
            } else {
                return Err(err(span, "List.nil expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints { args: vec![], ret }))
        }
        ("List", "cons") => {
            if args_len != 2 {
                return Err(err(span, "List.cons expects 2 args"));
            }
            if type_args.len() == 1 {
                let elem = tc.resolve_ty(&type_args[0])?;
                let list_tid = match elem {
                    T_I32 => T_LIST_I32,
                    T_BYTES => T_LIST_BYTES,
                    _ => {
                        if allow_ambiguous_generics {
                            return Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any, ArgConstraint::Any], ret: T_DYNAMIC }));
                        }
                        return Err(err(span, "only List<I32> and List<Bytes> supported for now"));
                    }
                };
                return Ok(Some(BuiltinConstraints {
                    args: vec![ArgConstraint::Exact(elem), ArgConstraint::Exact(list_tid)],
                    ret: list_tid,
                }));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any, ArgConstraint::Any], ret: T_DYNAMIC }))
        }
        ("List", "head") => {
            no_targs("List.head")?;
            if args_len != 1 {
                return Err(err(span, "List.head expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_DYNAMIC }))
        }
        ("List", "tail") => {
            no_targs("List.tail")?;
            if args_len != 1 {
                return Err(err(span, "List.tail expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_DYNAMIC }))
        }
        ("List", "is_nil") => {
            no_targs("List.is_nil")?;
            if args_len != 1 {
                return Err(err(span, "List.is_nil expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_BOOL }))
        }
        _ => Ok(None),
    }
}

