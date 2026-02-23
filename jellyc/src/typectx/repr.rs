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
use crate::ast::{Span, Ty, TyKind};
use crate::error::{CompileError, ErrorKind};
use crate::jlyb::{Module, TypeKind};

use super::TypeCtx;

/// AST-level type representation used for module ABI and import/export interfaces.
#[derive(Clone, Debug)]
pub enum TypeRepr {
    Bytes,
    Bool,
    I8,
    I16,
    I32,
    I64,
    F16,
    F32,
    F64,
    Dynamic,
    Object,
    Atom,
    Array(Box<TypeRepr>),
    List(Box<TypeRepr>),
    Fun {
        args: Vec<TypeRepr>,
        ret: Box<TypeRepr>,
    },
    Tuple(Vec<TypeRepr>),
}

/// Convert an AST type to a type representation.
pub fn type_repr_from_ty(t: &Ty) -> Result<TypeRepr, CompileError> {
    fn go(t: &Ty) -> Result<TypeRepr, CompileError> {
        match &t.node {
            // Named types
            TyKind::Named(n) => match n.as_str() {
                "Bytes" | "bytes" => Ok(TypeRepr::Bytes),
                "Bool" | "bool" => Ok(TypeRepr::Bool),
                "I8" | "i8" => Ok(TypeRepr::I8),
                "I16" | "i16" => Ok(TypeRepr::I16),
                "I32" | "i32" => Ok(TypeRepr::I32),
                "I64" | "i64" => Ok(TypeRepr::I64),
                "F16" | "f16" => Ok(TypeRepr::F16),
                "F32" | "f32" => Ok(TypeRepr::F32),
                "F64" | "f64" => Ok(TypeRepr::F64),
                "Any" | "Dynamic" | "dynamic" => Ok(TypeRepr::Dynamic),
                "Object" | "object" => Ok(TypeRepr::Object),
                "Atom" | "atom" => Ok(TypeRepr::Atom),
                _ => Err(CompileError::new(
                    ErrorKind::Type,
                    t.span,
                    format!("unknown type '{}'", n),
                )),
            },
            // Generic types
            TyKind::Generic { base, args } => {
                if base == "Array" || base == "array" {
                    if args.len() != 1 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "Array<T> expects 1 type arg",
                        ));
                    }
                    return Ok(TypeRepr::Array(Box::new(go(&args[0])?)));
                }
                if base == "List" || base == "list" {
                    if args.len() != 1 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            t.span,
                            "List<T> expects 1 type arg",
                        ));
                    }
                    return Ok(TypeRepr::List(Box::new(go(&args[0])?)));
                }
                if base == "Tuple" || base == "tuple" {
                    let mut elems: Vec<TypeRepr> = Vec::with_capacity(args.len());
                    for a in args {
                        elems.push(go(a)?);
                    }
                    return Ok(TypeRepr::Tuple(elems));
                }
                Err(CompileError::new(
                    ErrorKind::Type,
                    t.span,
                    format!("unknown generic type '{}'", base),
                ))
            }
            // Tuple types
            TyKind::Tuple(elems) => {
                let mut out: Vec<TypeRepr> = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(go(e)?);
                }
                Ok(TypeRepr::Tuple(out))
            }
            // Function types
            TyKind::Fun { args, ret } => {
                let mut as_out: Vec<TypeRepr> = Vec::with_capacity(args.len());
                for a in args {
                    as_out.push(go(a)?);
                }
                let r = go(ret)?;
                Ok(TypeRepr::Fun {
                    args: as_out,
                    ret: Box::new(r),
                })
            }
        }
    }
    go(t)
}

/// Convert a JLYB type ID to a type representation.
pub fn type_repr_from_jlyb(m: &Module, tid: u32) -> Result<TypeRepr, CompileError> {
    const TUPLE_TAG: u32 = 0x8000_0000;
    fn go(m: &Module, tid: u32) -> Result<TypeRepr, CompileError> {
        let te = m.types.get(tid as usize).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                Span::point(0),
                "bad type id in module ABI",
            )
        })?;
        match te.kind {
            TypeKind::Bytes => Ok(TypeRepr::Bytes),
            TypeKind::Bool => Ok(TypeRepr::Bool),
            TypeKind::I8 => Ok(TypeRepr::I8),
            TypeKind::I16 => Ok(TypeRepr::I16),
            TypeKind::I32 => Ok(TypeRepr::I32),
            TypeKind::I64 => Ok(TypeRepr::I64),
            TypeKind::F16 => Ok(TypeRepr::F16),
            TypeKind::F32 => Ok(TypeRepr::F32),
            TypeKind::F64 => Ok(TypeRepr::F64),
            TypeKind::Dynamic => Ok(TypeRepr::Dynamic),
            TypeKind::Object => {
                if (te.p0 & TUPLE_TAG) != 0 {
                    let sig_id = te.p0 & !TUPLE_TAG;
                    let sig = m.sigs.get(sig_id as usize).ok_or_else(|| {
                        CompileError::new(
                            ErrorKind::Internal,
                            Span::point(0),
                            "bad tuple sig id in module ABI",
                        )
                    })?;
                    let mut elems: Vec<TypeRepr> = Vec::with_capacity(sig.args.len());
                    for &et in &sig.args {
                        elems.push(go(m, et)?);
                    }
                    Ok(TypeRepr::Tuple(elems))
                } else {
                    Ok(TypeRepr::Object)
                }
            }
            TypeKind::Atom => Ok(TypeRepr::Atom),
            TypeKind::Array => Ok(TypeRepr::Array(Box::new(go(m, te.p0)?))),
            TypeKind::List => Ok(TypeRepr::List(Box::new(go(m, te.p0)?))),
            TypeKind::Function => {
                let sig = m.sigs.get(te.p0 as usize).ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Internal,
                        Span::point(0),
                        "bad fun sig id in module ABI",
                    )
                })?;
                let mut args: Vec<TypeRepr> = Vec::with_capacity(sig.args.len());
                for &a in &sig.args {
                    args.push(go(m, a)?);
                }
                let ret = go(m, sig.ret_type)?;
                Ok(TypeRepr::Fun {
                    args,
                    ret: Box::new(ret),
                })
            }
            _ => Err(CompileError::new(
                ErrorKind::Type,
                Span::point(0),
                "unsupported type in module ABI",
            )),
        }
    }
    go(m, tid)
}

/// Convert a TypeCtx type ID to a type representation.
/// Used for REPL session bindings (name → TypeRepr).
pub fn type_repr_from_typectx(ctx: &TypeCtx, tid: u32) -> Result<TypeRepr, CompileError> {
    const TUPLE_TAG: u32 = 0x8000_0000;
    fn go(ctx: &TypeCtx, tid: u32) -> Result<TypeRepr, CompileError> {
        let te = ctx.types.get(tid as usize).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                Span::point(0),
                "bad type id in type context",
            )
        })?;
        match te.kind {
            TypeKind::Bytes => Ok(TypeRepr::Bytes),
            TypeKind::Bool => Ok(TypeRepr::Bool),
            TypeKind::I8 => Ok(TypeRepr::I8),
            TypeKind::I16 => Ok(TypeRepr::I16),
            TypeKind::I32 => Ok(TypeRepr::I32),
            TypeKind::I64 => Ok(TypeRepr::I64),
            TypeKind::F16 => Ok(TypeRepr::F16),
            TypeKind::F32 => Ok(TypeRepr::F32),
            TypeKind::F64 => Ok(TypeRepr::F64),
            TypeKind::Dynamic => Ok(TypeRepr::Dynamic),
            TypeKind::Object => {
                if (te.p0 & TUPLE_TAG) != 0 {
                    let sig_id = te.p0 & !TUPLE_TAG;
                    let sig = ctx.sigs.get(sig_id as usize).ok_or_else(|| {
                        CompileError::new(
                            ErrorKind::Internal,
                            Span::point(0),
                            "bad tuple sig id in type context",
                        )
                    })?;
                    let mut elems: Vec<TypeRepr> = Vec::with_capacity(sig.args.len());
                    for &et in &sig.args {
                        elems.push(go(ctx, et)?);
                    }
                    Ok(TypeRepr::Tuple(elems))
                } else {
                    Ok(TypeRepr::Object)
                }
            }
            TypeKind::Atom => Ok(TypeRepr::Atom),
            TypeKind::Array => Ok(TypeRepr::Array(Box::new(go(ctx, te.p0)?))),
            TypeKind::List => Ok(TypeRepr::List(Box::new(go(ctx, te.p0)?))),
            TypeKind::Function => {
                let sig = ctx.sigs.get(te.p0 as usize).ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Internal,
                        Span::point(0),
                        "bad fun sig id in type context",
                    )
                })?;
                let mut args: Vec<TypeRepr> = Vec::with_capacity(sig.args.len());
                for &a in &sig.args {
                    args.push(go(ctx, a)?);
                }
                let ret = go(ctx, sig.ret_type)?;
                Ok(TypeRepr::Fun {
                    args,
                    ret: Box::new(ret),
                })
            }
            _ => Err(CompileError::new(
                ErrorKind::Type,
                Span::point(0),
                "unsupported type in type context",
            )),
        }
    }
    go(ctx, tid)
}
