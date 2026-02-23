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
use crate::ast::{Ty, TyKind};
use crate::error::{CompileError, ErrorKind};
use crate::jlyb::{FunSig, TypeEntry, TypeKind};

use super::base::{
    T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8, T_OBJECT,
};
use super::TypeCtx;

impl TypeCtx {
    pub(super) const TUPLE_TAG: u32 = 0x8000_0000;

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

    /// True if tid is a nominal object type (from prototype/let Name: Name = ...).
    pub fn is_nominal_object_type(&self, tid: u32) -> bool {
        self.nominal_obj_key_to_tid.values().any(|&t| t == tid)
    }

    /// Intern a nominal object type.
    pub(super) fn intern_nominal_object_type(&mut self, key: &str) -> u32 {
        if let Some(&tid) = self.nominal_obj_key_to_tid.get(key) {
            return tid;
        }
        let tid = self.types.len() as u32;
        let mut tag = Self::hash32(key) & !Self::TUPLE_TAG;
        if tag == 0 {
            tag = 1;
        }
        self.types.push(TypeEntry {
            kind: TypeKind::Object,
            p0: tag,
        });
        self.nominal_obj_key_to_tid.insert(key.to_string(), tid);
        tid
    }

    /// Get the key for a type.
    pub(super) fn ty_key(t: &Ty) -> Result<String, CompileError> {
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
    pub(super) fn type_name_to_tid(name: &str) -> Option<u32> {
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
        self.types.push(TypeEntry {
            kind: TypeKind::Function,
            p0: sig_id,
        });
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
}
