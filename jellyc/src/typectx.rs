use std::collections::HashMap;

use crate::ast::{Ty, TyKind};
use crate::error::{CompileError, ErrorKind};
use crate::jlyb::{FunSig, TypeEntry, TypeKind};

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
            TypeEntry { kind: TypeKind::Bytes, p0: 0 },   // 0
            TypeEntry { kind: TypeKind::Bool, p0: 0 },    // 1
            TypeEntry { kind: TypeKind::I32, p0: 0 },     // 2
            TypeEntry { kind: TypeKind::Dynamic, p0: 0 }, // 3
            TypeEntry { kind: TypeKind::Array, p0: 2 },   // 4: Array<i32>
            TypeEntry { kind: TypeKind::Array, p0: 0 },   // 5: Array<bytes>
            TypeEntry { kind: TypeKind::Object, p0: 0 },  // 6
            TypeEntry { kind: TypeKind::List, p0: 2 },    // 7: List<i32>
            TypeEntry { kind: TypeKind::List, p0: 0 },    // 8: List<bytes>
            TypeEntry { kind: TypeKind::Atom, p0: 0 },    // 9
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

    fn hash32(s: &str) -> u32 {
        // FNV-1a 32-bit.
        let mut h: u32 = 0x811c9dc5;
        for &b in s.as_bytes() {
            h ^= b as u32;
            h = h.wrapping_mul(0x01000193);
        }
        h
    }

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

    fn type_name_to_tid(name: &str) -> Option<u32> {
        match name {
            "I32" | "i32" => Some(2),
            "Bool" | "bool" => Some(1),
            "Bytes" | "bytes" => Some(0),
            "Any" | "Dynamic" | "dynamic" => Some(3),
            "Object" | "object" => Some(6),
            "Atom" | "atom" => Some(9),
            _ => None,
        }
    }

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
                        2 => Ok(4), // Array<i32>
                        0 => Ok(5), // Array<bytes>
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
                        2 => Ok(7), // List<i32>
                        0 => Ok(8), // List<bytes>
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
                        2 => Ok(4), // Array<i32>
                        0 => Ok(5), // Array<bytes>
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
                        2 => Ok(7), // List<i32>
                        0 => Ok(8), // List<bytes>
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
        let subst = HashMap::from([("T".to_string(), 2u32)]);
        assert_eq!(tc.resolve_ty_with_subst(&ty_named("T"), &subst).unwrap(), 2);
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
        let subst = HashMap::from([("T".to_string(), 2u32)]);
        assert_eq!(tc.resolve_ty_with_subst(&t, &subst).unwrap(), 4);
    }

    #[test]
    fn resolve_ty_with_subst_unknown_still_errors() {
        let mut tc = TypeCtx::new_program_base();
        let subst: HashMap<String, u32> = HashMap::new();
        let err = tc.resolve_ty_with_subst(&ty_named("T"), &subst).unwrap_err();
        assert!(err.message.contains("unknown type"));
    }
}

