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
use crate::typectx::{TypeCtx, T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_DYNAMIC, T_I32};

use super::{ArgConstraint, BuiltinConstraints};

pub(super) fn err(span: Span, msg: impl Into<String>) -> CompileError {
    CompileError::new(ErrorKind::Type, span, msg)
}

pub fn array_elem_tid(arr_tid: TypeId) -> Option<TypeId> {
    match arr_tid {
        T_ARRAY_I32 => Some(T_I32),
        T_ARRAY_BYTES => Some(T_BYTES),
        _ => None,
    }
}

pub fn array_constraints_from_arr_tid(
    name: &str,
    args_len: usize,
    arr_tid: TypeId,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    match name {
        "len" => {
            if args_len != 1 {
                return Err(err(span, "Array.len expects 1 arg"));
            }
            if arr_tid != T_ARRAY_I32 && arr_tid != T_ARRAY_BYTES {
                return Err(err(span, "Array.len expects Array<T>"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(arr_tid)],
                ret: T_I32,
            }))
        }
        "get" => {
            if args_len != 2 {
                return Err(err(span, "Array.get expects 2 args"));
            }
            let Some(elem_tid) = array_elem_tid(arr_tid) else {
                return Err(err(span, "Array.get expects Array<I32> or Array<Bytes>"));
            };
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(arr_tid), ArgConstraint::Exact(T_I32)],
                ret: elem_tid,
            }))
        }
        "set" => {
            if args_len != 3 {
                return Err(err(span, "Array.set expects 3 args"));
            }
            let Some(elem_tid) = array_elem_tid(arr_tid) else {
                return Err(err(span, "Array.set expects Array<I32> or Array<Bytes>"));
            };
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::Exact(arr_tid),
                    ArgConstraint::Exact(T_I32),
                    ArgConstraint::Exact(elem_tid),
                ],
                ret: arr_tid,
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
        "new" => {
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
                            return Err(err(
                                span,
                                "only Array<I32> and Array<Bytes> supported for now",
                            ));
                        }
                    }
                }
            } else {
                return Err(err(span, "Array.new expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_I32)],
                ret,
            }))
        }
        "len" => {
            no_targs("Array.len")?;
            if args_len != 1 {
                return Err(err(span, "Array.len expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any],
                ret: T_I32,
            }))
        }
        "get" => {
            no_targs("Array.get")?;
            if args_len != 2 {
                return Err(err(span, "Array.get expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Exact(T_I32)],
                ret: T_DYNAMIC,
            }))
        }
        "set" => {
            no_targs("Array.set")?;
            if args_len != 3 {
                return Err(err(span, "Array.set expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::Any,
                    ArgConstraint::Exact(T_I32),
                    ArgConstraint::Any,
                ],
                ret: T_DYNAMIC,
            }))
        }
        _ => Ok(None),
    }
}
