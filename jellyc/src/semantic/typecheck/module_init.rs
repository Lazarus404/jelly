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

use std::collections::HashMap;

use crate::ast::{Program, Span, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::hir::SemanticInfo;
use crate::ir::TypeId;
use crate::typectx::{TypeCtx, TypeRepr};
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16,
    T_I32, T_I64, T_I8, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

use super::{TypeChecker, TypecheckInputs};

pub(super) fn typecheck_module_init(
    p: &Program,
    import_exports: &HashMap<String, HashMap<String, TypeRepr>>,
    is_repl: bool,
) -> Result<SemanticInfo, CompileError> {
    // Build module alias export type IDs from TypeRepr (mirrors lowering).
    let mut key_to_alias: HashMap<String, String> = HashMap::new();
    let mut hidden_i = 0u32;
    let mut import_keys: Vec<String> = Vec::new();
    for s in &p.stmts {
        match &s.node {
            StmtKind::ImportModule { path, alias } => {
                let key = path.join(".");
                if !key_to_alias.contains_key(&key) {
                    key_to_alias.insert(key.clone(), alias.clone());
                    import_keys.push(key);
                }
            }
            StmtKind::ImportFrom { from, .. } => {
                let key = from.join(".");
                if !key_to_alias.contains_key(&key) {
                    let hid = format!("__import{}", hidden_i);
                    hidden_i += 1;
                    key_to_alias.insert(key.clone(), hid);
                    import_keys.push(key);
                }
            }
            _ => {}
        }
    }

    let mut type_ctx = TypeCtx::new_program_base();
    let mut module_alias_exports: HashMap<String, HashMap<String, TypeId>> = HashMap::new();
    for key in &import_keys {
        let alias = key_to_alias.get(key).expect("alias");
        let ex = import_exports.get(key).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Name,
                Span::point(0),
                format!("missing import interface for '{}'", key),
            )
        })?;
        let mut map: HashMap<String, TypeId> = HashMap::new();
        for (name, tr) in ex {
            let tid = intern_type_repr(&mut type_ctx, tr, Span::point(0))?;
            map.insert(name.clone(), tid);
        }
        module_alias_exports.insert(alias.clone(), map);
    }

    // Bind `import {x as y} from mod` items into the initial environment using the module interface.
    let mut prelude_env: HashMap<String, TypeId> = HashMap::new();
    for s in &p.stmts {
        if let StmtKind::ImportFrom { items, from, .. } = &s.node {
            let key = from.join(".");
            let alias = key_to_alias.get(&key).expect("alias for import-from");
            let exports = module_alias_exports.get(alias).ok_or_else(|| {
                CompileError::new(ErrorKind::Name, s.span, format!("unknown module '{}'", key))
            })?;
            for (name, as_name) in items {
                let bind = as_name.as_deref().unwrap_or(name.as_str()).to_string();
                let tid = exports.get(name).copied().ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        format!("unknown export '{}.{}'", key, name),
                    )
                })?;
                prelude_env.insert(bind, tid);
            }
        }
    }

    let expected_program_expr_type = if is_repl { T_DYNAMIC } else { T_BYTES };
    let mut tc = TypeChecker::new(TypecheckInputs {
        module_alias_exports,
        prelude_env,
        expected_program_expr_type,
    });
    // Seed the typechecker with the module's type context (so import types are present).
    tc.seed_type_ctx(type_ctx);
    tc.check_program(p)?;
    Ok(tc.finish())
}

fn intern_type_repr(tc: &mut TypeCtx, tr: &TypeRepr, span: Span) -> Result<TypeId, CompileError> {
    match tr {
        TypeRepr::Bytes => Ok(T_BYTES),
        TypeRepr::Bool => Ok(T_BOOL),
        TypeRepr::I8 => Ok(T_I8),
        TypeRepr::I16 => Ok(T_I16),
        TypeRepr::I32 => Ok(T_I32),
        TypeRepr::I64 => Ok(T_I64),
        TypeRepr::F16 => Ok(T_F16),
        TypeRepr::F32 => Ok(T_F32),
        TypeRepr::F64 => Ok(T_F64),
        TypeRepr::Dynamic => Ok(T_DYNAMIC),
        TypeRepr::Object => Ok(T_OBJECT),
        TypeRepr::Atom => Ok(T_ATOM),
        TypeRepr::Array(elem) => {
            let e = intern_type_repr(tc, elem, span)?;
            match e {
                T_I32 => Ok(T_ARRAY_I32),
                T_BYTES => Ok(T_ARRAY_BYTES),
                _ => Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "only Array<I32> and Array<Bytes> supported for now",
                )),
            }
        }
        TypeRepr::List(elem) => {
            let e = intern_type_repr(tc, elem, span)?;
            match e {
                T_I32 => Ok(T_LIST_I32),
                T_BYTES => Ok(T_LIST_BYTES),
                _ => Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "only List<I32> and List<Bytes> supported for now",
                )),
            }
        }
        TypeRepr::Tuple(elems) => {
            let mut elem_tids: Vec<TypeId> = Vec::with_capacity(elems.len());
            for e in elems {
                elem_tids.push(intern_type_repr(tc, e, span)?);
            }
            Ok(tc.intern_tuple_type(&elem_tids))
        }
        TypeRepr::Fun { args, ret } => {
            let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
            for a in args {
                arg_tids.push(intern_type_repr(tc, a, span)?);
            }
            let ret_tid = intern_type_repr(tc, ret, span)?;
            Ok(tc.intern_fun_type(ret_tid, &arg_tids))
        }
    }
}
