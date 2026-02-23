/**
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
use std::collections::HashMap;

use crate::ast::{Ty, TyKind};
use crate::error::{CompileError, ErrorKind};

use super::base::{T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_I32, T_LIST_BYTES, T_LIST_I32};
use super::TypeCtx;

impl TypeCtx {
    /// Resolve a type to a type ID.
    pub fn resolve_ty(&mut self, t: &Ty) -> Result<u32, CompileError> {
        match &t.node {
            TyKind::Named(n) => {
                if let Some(tid) = Self::type_name_to_tid(n.as_str()) {
                    return Ok(tid);
                }
                Ok(self.intern_nominal_object_type(n))
            }
            TyKind::Generic { base, args } => {
                if base == "Array" || base == "array" {
                    if args.len() != 1 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "Array<T> expects 1 type arg",
                        ));
                    }
                    let elem = self.resolve_ty(&args[0])?;
                    return match elem {
                        T_I32 => Ok(T_ARRAY_I32),
                        T_BYTES => Ok(T_ARRAY_BYTES),
                        _ => Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "only Array<i32> and Array<bytes> supported for now",
                        )),
                    };
                }
                if base == "List" || base == "list" {
                    if args.len() != 1 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "List<T> expects 1 type arg",
                        ));
                    }
                    let elem = self.resolve_ty(&args[0])?;
                    return match elem {
                        T_I32 => Ok(T_LIST_I32),
                        T_BYTES => Ok(T_LIST_BYTES),
                        _ => Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "only List<i32> and List<bytes> supported for now",
                        )),
                    };
                }
                if base == "Tuple" || base == "tuple" {
                    let mut elem_tids: Vec<u32> = Vec::with_capacity(args.len());
                    for a in args {
                        elem_tids.push(self.resolve_ty(a)?);
                    }
                    return Ok(self.intern_tuple_type(&elem_tids));
                }
                let key = Self::ty_key(t)?;
                Ok(self.intern_nominal_object_type(&key))
            }
            TyKind::Tuple(elems) => {
                let mut elem_tids: Vec<u32> = Vec::with_capacity(elems.len());
                for e in elems {
                    elem_tids.push(self.resolve_ty(e)?);
                }
                Ok(self.intern_tuple_type(&elem_tids))
            }
            TyKind::Fun { args, ret } => {
                let mut arg_tids: Vec<u32> = Vec::with_capacity(args.len());
                for a in args {
                    arg_tids.push(self.resolve_ty(a)?);
                }
                let ret_tid = self.resolve_ty(ret)?;

                Ok(self.intern_fun_type(ret_tid, &arg_tids))
            }
        }
    }

    /// Resolve a type with a substitution.
    #[cfg_attr(not(test), allow(dead_code))]
    pub fn resolve_ty_with_subst(
        &mut self,
        t: &Ty,
        subst: &HashMap<String, u32>,
    ) -> Result<u32, CompileError> {
        match &t.node {
            TyKind::Named(n) => {
                if let Some(&tid) = subst.get(n) {
                    return Ok(tid);
                }
                if let Some(tid) = Self::type_name_to_tid(n.as_str()) {
                    return Ok(tid);
                }
                if n.len() == 1 && n.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        t.span,
                        format!("unknown type '{}'", n),
                    ));
                }
                Ok(self.intern_nominal_object_type(n))
            }
            TyKind::Generic { base, args } => {
                if base == "Array" || base == "array" {
                    if args.len() != 1 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "Array<T> expects 1 type arg",
                        ));
                    }
                    let elem = self.resolve_ty_with_subst(&args[0], subst)?;
                    return match elem {
                        T_I32 => Ok(T_ARRAY_I32),
                        T_BYTES => Ok(T_ARRAY_BYTES),
                        _ => Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "only Array<i32> and Array<bytes> supported for now",
                        )),
                    };
                }
                if base == "List" || base == "list" {
                    if args.len() != 1 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "List<T> expects 1 type arg",
                        ));
                    }
                    let elem = self.resolve_ty_with_subst(&args[0], subst)?;
                    return match elem {
                        T_I32 => Ok(T_LIST_I32),
                        T_BYTES => Ok(T_LIST_BYTES),
                        _ => Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "only List<i32> and List<bytes> supported for now",
                        )),
                    };
                }
                if base == "Tuple" || base == "tuple" {
                    let mut elem_tids: Vec<u32> = Vec::with_capacity(args.len());
                    for a in args {
                        elem_tids.push(self.resolve_ty_with_subst(a, subst)?);
                    }
                    return Ok(self.intern_tuple_type(&elem_tids));
                }
                if base.len() == 1 && base.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        t.span,
                        format!("unknown generic type '{}'", base),
                    ));
                }
                let key = Self::ty_key(t)?;
                Ok(self.intern_nominal_object_type(&key))
            }
            TyKind::Tuple(elems) => {
                let mut elem_tids: Vec<u32> = Vec::with_capacity(elems.len());
                for e in elems {
                    elem_tids.push(self.resolve_ty_with_subst(e, subst)?);
                }
                Ok(self.intern_tuple_type(&elem_tids))
            }
            TyKind::Fun { args, ret } => {
                let mut arg_tids: Vec<u32> = Vec::with_capacity(args.len());
                for a in args {
                    arg_tids.push(self.resolve_ty_with_subst(a, subst)?);
                }
                let ret_tid = self.resolve_ty_with_subst(ret, subst)?;
                Ok(self.intern_fun_type(ret_tid, &arg_tids))
            }
        }
    }
}
