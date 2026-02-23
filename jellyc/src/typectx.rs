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

use crate::ast::{Span, Ty, TyKind};
use crate::error::{CompileError, ErrorKind};
use crate::jlyb::{FunSig, TypeEntry, TypeKind};

// Base program-module type IDs (type table indices).
// Must match `TypeCtx::new_program_base()` and is shared across compiler pipelines.
pub const T_BOOL: u32 = 0;
pub const T_ATOM: u32 = 1;
pub const T_I8: u32 = 2;
pub const T_I16: u32 = 3;
pub const T_I32: u32 = 4;
pub const T_I64: u32 = 5;
pub const T_F16: u32 = 6;
pub const T_F32: u32 = 7;
pub const T_F64: u32 = 8;
pub const T_BYTES: u32 = 9;
pub const T_DYNAMIC: u32 = 10;
pub const T_OBJECT: u32 = 11;
pub const T_ARRAY_I32: u32 = 12;
pub const T_ARRAY_BYTES: u32 = 13;
pub const T_LIST_I32: u32 = 14;
pub const T_LIST_BYTES: u32 = 15;

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
    Fun { args: Vec<TypeRepr>, ret: Box<TypeRepr> },
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
                        return Err(CompileError::new(ErrorKind::Type, t.span, "Array<T> expects 1 type arg"));
                    }
                    return Ok(TypeRepr::Array(Box::new(go(&args[0])?)));
                }
                if base == "List" || base == "list" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, t.span, "List<T> expects 1 type arg"));
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
pub fn type_repr_from_jlyb(m: &crate::jlyb::Module, tid: u32) -> Result<TypeRepr, CompileError> {
    const TUPLE_TAG: u32 = 0x8000_0000;
    fn go(m: &crate::jlyb::Module, tid: u32) -> Result<TypeRepr, CompileError> {
        let te = m.types.get(tid as usize).ok_or_else(|| {
            CompileError::new(ErrorKind::Internal, Span::point(0), "bad type id in module ABI")
        })?;
        match te.kind {
            crate::jlyb::TypeKind::Bytes => Ok(TypeRepr::Bytes),
            crate::jlyb::TypeKind::Bool => Ok(TypeRepr::Bool),
            crate::jlyb::TypeKind::I8 => Ok(TypeRepr::I8),
            crate::jlyb::TypeKind::I16 => Ok(TypeRepr::I16),
            crate::jlyb::TypeKind::I32 => Ok(TypeRepr::I32),
            crate::jlyb::TypeKind::I64 => Ok(TypeRepr::I64),
            crate::jlyb::TypeKind::F16 => Ok(TypeRepr::F16),
            crate::jlyb::TypeKind::F32 => Ok(TypeRepr::F32),
            crate::jlyb::TypeKind::F64 => Ok(TypeRepr::F64),
            crate::jlyb::TypeKind::Dynamic => Ok(TypeRepr::Dynamic),
            crate::jlyb::TypeKind::Object => {
                if (te.p0 & TUPLE_TAG) != 0 {
                    let sig_id = te.p0 & !TUPLE_TAG;
                    let sig = m.sigs.get(sig_id as usize).ok_or_else(|| {
                        CompileError::new(ErrorKind::Internal, Span::point(0), "bad tuple sig id in module ABI")
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
            crate::jlyb::TypeKind::Atom => Ok(TypeRepr::Atom),
            crate::jlyb::TypeKind::Array => Ok(TypeRepr::Array(Box::new(go(m, te.p0)?))),
            crate::jlyb::TypeKind::List => Ok(TypeRepr::List(Box::new(go(m, te.p0)?))),
            crate::jlyb::TypeKind::Function => {
                let sig = m.sigs.get(te.p0 as usize).ok_or_else(|| {
                    CompileError::new(ErrorKind::Internal, Span::point(0), "bad fun sig id in module ABI")
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

/// Type context for the compiler.
#[derive(Clone, Debug)]
pub struct TypeCtx {
    pub types: Vec<TypeEntry>,
    pub sigs: Vec<FunSig>,
    sig_key_to_id: HashMap<String, u32>,
    fun_sig_to_type: HashMap<u32, u32>,
    tuple_sig_to_type: HashMap<u32, u32>,
    nominal_obj_key_to_tid: HashMap<String, u32>,
}

impl TypeCtx {
    const TUPLE_TAG: u32 = 0x8000_0000;

    pub fn new_program_base() -> Self {
        // Must match the program-module base table in `jlyb.rs`.
        let types = vec![
            TypeEntry { kind: TypeKind::Bool, p0: 0 },           // 0
            TypeEntry { kind: TypeKind::Atom, p0: 0 },           // 1
            TypeEntry { kind: TypeKind::I8, p0: 0 },             // 2
            TypeEntry { kind: TypeKind::I16, p0: 0 },            // 3
            TypeEntry { kind: TypeKind::I32, p0: 0 },            // 4
            TypeEntry { kind: TypeKind::I64, p0: 0 },            // 5
            TypeEntry { kind: TypeKind::F16, p0: 0 },            // 6
            TypeEntry { kind: TypeKind::F32, p0: 0 },            // 7
            TypeEntry { kind: TypeKind::F64, p0: 0 },            // 8
            TypeEntry { kind: TypeKind::Bytes, p0: 0 },          // 9
            TypeEntry { kind: TypeKind::Dynamic, p0: 0 },        // 10
            TypeEntry { kind: TypeKind::Object, p0: 0 },         // 11
            TypeEntry { kind: TypeKind::Array, p0: T_I32 },      // 12: Array<i32>
            TypeEntry { kind: TypeKind::Array, p0: T_BYTES },    // 13: Array<bytes>
            TypeEntry { kind: TypeKind::List, p0: T_I32 },       // 14: List<i32>
            TypeEntry { kind: TypeKind::List, p0: T_BYTES },     // 15: List<bytes>
        ];
        Self {
            types,
            sigs: Vec::new(),
            sig_key_to_id: HashMap::new(),
            fun_sig_to_type: HashMap::new(),
            tuple_sig_to_type: HashMap::new(),
            nominal_obj_key_to_tid: HashMap::new(),
        }
    }

    /// Hash a string to a 32-bit integer.
    fn hash32(s: &str) -> u32 {
        // FNV-1a 32-bit.
        let mut h: u32 = 0x811c9dc5;
        for &b in s.as_bytes() {
            h ^= b as u32;
            h = h.wrapping_mul(0x01000193);
        }
        h
    }

    /// Intern a nominal object type.
    fn intern_nominal_object_type(&mut self, key: &str) -> u32 {
        if let Some(&tid) = self.nominal_obj_key_to_tid.get(key) {
            return tid;
        }
        let tid = self.types.len() as u32;
        let mut tag = Self::hash32(key) & !Self::TUPLE_TAG;
        if tag == 0 {
            tag = 1;
        }
        self.types.push(TypeEntry { kind: TypeKind::Object, p0: tag });
        self.nominal_obj_key_to_tid.insert(key.to_string(), tid);
        tid
    }

    /// Get the key for a type.
    fn ty_key(t: &Ty) -> Result<String, CompileError> {
        match &t.node {
            TyKind::Named(n) => Ok(n.clone()),
            TyKind::Generic { base, args } => {
                let mut out = String::new();
                out.push_str(base);
                out.push('<');
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        out.push(',');
                    }
                    out.push_str(&Self::ty_key(a)?);
                }
                out.push('>');
                Ok(out)
            }
            TyKind::Tuple(elems) => {
                let mut out = "Tuple<".to_string();
                for (i, e) in elems.iter().enumerate() {
                    if i != 0 {
                        out.push(',');
                    }
                    out.push_str(&Self::ty_key(e)?);
                }
                out.push('>');
                Ok(out)
            }
            TyKind::Fun { .. } => Err(CompileError::new(
                ErrorKind::Type,
                t.span,
                "function types are not supported as nominal prototype types",
            )),
        }
    }

    /// Convert a type name to a type ID.
    fn type_name_to_tid(name: &str) -> Option<u32> {
        match name {
            "Bool" | "bool" => Some(T_BOOL),
            "Atom" | "atom" => Some(T_ATOM),
            "I8" | "i8" => Some(T_I8),
            "I16" | "i16" => Some(T_I16),
            "I32" | "i32" => Some(T_I32),
            "I64" | "i64" => Some(T_I64),
            "F16" | "f16" => Some(T_F16),
            "F32" | "f32" => Some(T_F32),
            "F64" | "f64" => Some(T_F64),
            "Bytes" | "bytes" => Some(T_BYTES),
            "Any" | "Dynamic" | "dynamic" => Some(T_DYNAMIC),
            "Object" | "object" => Some(T_OBJECT),
            _ => None,
        }
    }

    /// Get the key for a signature.
    fn sig_key(ret: u32, args: &[u32]) -> String {
        let mut s = format!("r{}", ret);
        for &a in args {
            s.push(',');
            s.push_str(&a.to_string());
        }
        s
    }

    pub fn intern_sig(&mut self, ret_tid: u32, args: &[u32]) -> u32 {
        let key = Self::sig_key(ret_tid, args);
        if let Some(&id) = self.sig_key_to_id.get(&key) {
            return id;
        }
        let id = self.sigs.len() as u32;
        self.sigs.push(FunSig {
            ret_type: ret_tid,
            args: args.to_vec(),
        });
        self.sig_key_to_id.insert(key, id);
        id
    }

    pub fn intern_fun_type(&mut self, ret_tid: u32, args: &[u32]) -> u32 {
        let sig_id = self.intern_sig(ret_tid, args);
        if let Some(&tid) = self.fun_sig_to_type.get(&sig_id) {
            return tid;
        }
        let tid = self.types.len() as u32;
        self.types.push(TypeEntry { kind: TypeKind::Function, p0: sig_id });
        self.fun_sig_to_type.insert(sig_id, tid);
        tid
    }

    pub fn intern_tuple_type(&mut self, elems: &[u32]) -> u32 {
        // Encode tuple element types via a signature (ret type is ignored for tuples).
        let sig_id = self.intern_sig(3, elems); // 3 = Dynamic
        if let Some(&tid) = self.tuple_sig_to_type.get(&sig_id) {
            return tid;
        }
        let tid = self.types.len() as u32;
        self.types.push(TypeEntry {
            kind: TypeKind::Object,
            p0: Self::TUPLE_TAG | sig_id,
        });
        self.tuple_sig_to_type.insert(sig_id, tid);
        tid
    }

    pub fn is_tuple_type(&self, tid: u32) -> bool {
        self.types
            .get(tid as usize)
            .is_some_and(|te| te.kind == TypeKind::Object && (te.p0 & Self::TUPLE_TAG) != 0)
    }

    /// Get the elements of a tuple type.
    pub fn tuple_elems(&self, tid: u32) -> Option<&[u32]> {
        let te = self.types.get(tid as usize)?;
        if te.kind != TypeKind::Object {
            return None;
        }
        if (te.p0 & Self::TUPLE_TAG) == 0 {
            return None;
        }
        let sig_id = te.p0 & !Self::TUPLE_TAG;
        let sig = self.sigs.get(sig_id as usize)?;
        Some(sig.args.as_slice())
    }

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
                        return Err(CompileError::new(ErrorKind::Type, t.span, "Array<T> expects 1 type arg"));
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
                        return Err(CompileError::new(ErrorKind::Type, t.span, "List<T> expects 1 type arg"));
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
    pub fn resolve_ty_with_subst(&mut self, t: &Ty, subst: &HashMap<String, u32>) -> Result<u32, CompileError> {
        match &t.node {
            TyKind::Named(n) => {
                if let Some(&tid) = subst.get(n) {
                    return Ok(tid);
                }
                if let Some(tid) = Self::type_name_to_tid(n.as_str()) {
                    return Ok(tid);
                }
                if n.len() == 1 && n.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    return Err(CompileError::new(ErrorKind::Type, t.span, format!("unknown type '{}'", n)));
                }
                Ok(self.intern_nominal_object_type(n))
            }
            TyKind::Generic { base, args } => {
                if base == "Array" || base == "array" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, t.span, "Array<T> expects 1 type arg"));
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
                        return Err(CompileError::new(ErrorKind::Type, t.span, "List<T> expects 1 type arg"));
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Spanned};

    fn ty_named(name: &str) -> Ty {
        Spanned::new(TyKind::Named(name.to_string()), Span::point(0))
    }

    #[test]
    fn resolve_ty_with_subst_named_typevar() {
        let mut tc = TypeCtx::new_program_base();
        let subst = HashMap::from([("T".to_string(), T_I32)]);
        assert_eq!(tc.resolve_ty_with_subst(&ty_named("T"), &subst).unwrap(), T_I32);
    }

    #[test]
    fn resolve_ty_with_subst_generic_array() {
        let mut tc = TypeCtx::new_program_base();
        let t = Spanned::new(
            TyKind::Generic {
                base: "Array".to_string(),
                args: vec![ty_named("T")],
            },
            Span::point(0),
        );
        let subst = HashMap::from([("T".to_string(), T_I32)]);
        assert_eq!(tc.resolve_ty_with_subst(&t, &subst).unwrap(), T_ARRAY_I32);
    }

    #[test]
    fn resolve_ty_with_subst_unknown_still_errors() {
        let mut tc = TypeCtx::new_program_base();
        let subst: HashMap<String, u32> = HashMap::new();
        let err = tc.resolve_ty_with_subst(&ty_named("T"), &subst).unwrap_err();
        assert!(err.message.contains("unknown type"));
    }
}

