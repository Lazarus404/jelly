/*
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{TypeCtx, T_BOOL, T_BYTES, T_DYNAMIC, T_I32, T_LIST_BYTES, T_LIST_I32};

use super::{ArgConstraint, BuiltinConstraints};

pub(super) fn err(span: Span, msg: impl Into<String>) -> CompileError {
    CompileError::new(ErrorKind::Type, span, msg)
}

pub fn list_elem_tid(list_tid: TypeId) -> Option<TypeId> {
    match list_tid {
        T_LIST_I32 => Some(T_I32),
        T_LIST_BYTES => Some(T_BYTES),
        _ => None,
    }
}

pub fn list_constraints_from_list_tid(
    name: &str,
    args_len: usize,
    list_tid: TypeId,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    match name {
        "head" => {
            if args_len != 1 {
                return Err(err(span, "List.head expects 1 arg"));
            }
            let Some(elem_tid) = list_elem_tid(list_tid) else {
                return Err(err(span, "List.head expects List<I32> or List<Bytes>"));
            };
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(list_tid)],
                ret: elem_tid,
            }))
        }
        "tail" => {
            if args_len != 1 {
                return Err(err(span, "List.tail expects 1 arg"));
            }
            if list_elem_tid(list_tid).is_none() {
                return Err(err(span, "List.tail expects List<I32> or List<Bytes>"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(list_tid)],
                ret: list_tid,
            }))
        }
        "is_nil" => {
            if args_len != 1 {
                return Err(err(span, "List.is_nil expects 1 arg"));
            }
            if list_elem_tid(list_tid).is_none() {
                return Err(err(span, "List.is_nil expects List<I32> or List<Bytes>"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(list_tid)],
                ret: T_BOOL,
            }))
        }
        _ => Ok(None),
    }
}

pub(super) fn namespace_constraints(
    name: &str,
    type_args: &[crate::ast::Ty],
    args_len: usize,
    expect: Option<TypeId>,
    tc: &mut TypeCtx,
    allow_ambiguous_generics: bool,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    let no_targs = |what: &str| {
        if !type_args.is_empty() {
            return Err(err(span, format!("{what} does not take type arguments")));
        }
        Ok(())
    };

    match name {
        "nil" => {
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
                            return Err(err(
                                span,
                                "only List<I32> and List<Bytes> supported for now",
                            ));
                        }
                    }
                }
            } else {
                return Err(err(span, "List.nil expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints { args: vec![], ret }))
        }
        "cons" => {
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
                            return Ok(Some(BuiltinConstraints {
                                args: vec![ArgConstraint::Any, ArgConstraint::Any],
                                ret: T_DYNAMIC,
                            }));
                        }
                        return Err(err(
                            span,
                            "only List<I32> and List<Bytes> supported for now",
                        ));
                    }
                };
                return Ok(Some(BuiltinConstraints {
                    args: vec![ArgConstraint::Exact(elem), ArgConstraint::Exact(list_tid)],
                    ret: list_tid,
                }));
            }
            if let Some(t) = expect {
                if t == T_LIST_I32 {
                    return Ok(Some(BuiltinConstraints {
                        args: vec![
                            ArgConstraint::Exact(T_I32),
                            ArgConstraint::Exact(T_LIST_I32),
                        ],
                        ret: T_LIST_I32,
                    }));
                }
                if t == T_LIST_BYTES {
                    return Ok(Some(BuiltinConstraints {
                        args: vec![
                            ArgConstraint::Exact(T_BYTES),
                            ArgConstraint::Exact(T_LIST_BYTES),
                        ],
                        ret: T_LIST_BYTES,
                    }));
                }
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Any],
                ret: T_DYNAMIC,
            }))
        }
        "head" => {
            no_targs("List.head")?;
            if args_len != 1 {
                return Err(err(span, "List.head expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any],
                ret: T_DYNAMIC,
            }))
        }
        "tail" => {
            no_targs("List.tail")?;
            if args_len != 1 {
                return Err(err(span, "List.tail expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any],
                ret: T_DYNAMIC,
            }))
        }
        "is_nil" => {
            no_targs("List.is_nil")?;
            if args_len != 1 {
                return Err(err(span, "List.is_nil expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any],
                ret: T_BOOL,
            }))
        }
        _ => Ok(None),
    }
}
