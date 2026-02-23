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

use std::collections::{HashMap, HashSet};

use crate::ast::{Program, Span, StmtKind};
use crate::error::{CompileError, CompileWarning, ErrorKind};
use crate::hir::SemanticInfo;
use crate::ir::{IrBuilder, IrModule, IrTerminator, TypeId, VRegId};
use crate::typectx::{TypeCtx, TypeRepr};

use super::{bind_local, ensure_open_block, intern_atom, lower_expr, lower_stmt, LowerCtx, T_BYTES, T_OBJECT};

fn intern_type_repr(ctx: &mut TypeCtx, tr: &TypeRepr, span: Span) -> Result<TypeId, CompileError> {
    match tr {
        TypeRepr::Bytes => Ok(super::T_BYTES),
        TypeRepr::Bool => Ok(super::T_BOOL),
        TypeRepr::I8 => Ok(super::T_I8),
        TypeRepr::I16 => Ok(super::T_I16),
        TypeRepr::I32 => Ok(super::T_I32),
        TypeRepr::I64 => Ok(super::T_I64),
        TypeRepr::F16 => Ok(super::T_F16),
        TypeRepr::F32 => Ok(super::T_F32),
        TypeRepr::F64 => Ok(super::T_F64),
        TypeRepr::Dynamic => Ok(super::T_DYNAMIC),
        TypeRepr::Object => Ok(super::T_OBJECT),
        TypeRepr::Atom => Ok(super::T_ATOM),
        TypeRepr::Array(elem) => {
            let e = intern_type_repr(ctx, elem, span)?;
            match e {
                super::T_I32 => Ok(super::T_ARRAY_I32),
                super::T_BYTES => Ok(super::T_ARRAY_BYTES),
                _ => Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "only Array<I32> and Array<Bytes> supported for now",
                )),
            }
        }
        TypeRepr::List(elem) => {
            let e = intern_type_repr(ctx, elem, span)?;
            match e {
                super::T_I32 => Ok(super::T_LIST_I32),
                super::T_BYTES => Ok(super::T_LIST_BYTES),
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
                elem_tids.push(intern_type_repr(ctx, e, span)?);
            }
            Ok(ctx.intern_tuple_type(&elem_tids))
        }
        TypeRepr::Fun { args, ret } => {
            let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
            for a in args {
                arg_tids.push(intern_type_repr(ctx, a, span)?);
            }
            let ret_tid = intern_type_repr(ctx, ret, span)?;
            Ok(ctx.intern_fun_type(ret_tid, &arg_tids))
        }
    }
}

#[cfg_attr(not(test), allow(dead_code))]
pub fn lower_program_to_ir(p: &Program, sem: &SemanticInfo) -> Result<IrModule, CompileError> {
    let mut b = IrBuilder::new(Some("main".to_string()));
    let mut ctx = LowerCtx {
        type_ctx: sem.type_ctx.clone(),
        const_bytes: Vec::new(),
        const_i64: Vec::new(),
        const_f64: Vec::new(),
        atoms: vec![b"__proto__".to_vec(), b"init".to_vec()],
        atom_ids: HashMap::from([
            ("__proto__".to_string(), crate::jlyb::ATOM___PROTO__),
            ("init".to_string(), crate::jlyb::ATOM_INIT),
        ]),
        env_stack: vec![HashMap::new()],
        loop_stack: Vec::new(),
        fn_stack: Vec::new(),
        nested_funcs: Vec::new(),
        next_nested_fun_index: 0,
        pending_fn_self: None,
        user_top_level_fun_count: 1,
        exports_obj: None,
        module_alias_exports: HashMap::new(),
        module_key_to_alias: HashMap::new(),
        sem_binding_types: sem.binding_types.clone(),
        sem_expr_types: sem.expr_types.clone(),
        sem_const_inits: sem.const_inits.clone(),
        warnings: Vec::new(),
        method_table: HashMap::new(),
        proto_for_nominal: None,
        recording_method: None,
        last_const_fun_index: None,
        export_all_lets: false,
        exported_bindings: HashSet::new(),
        session_obj: None,
        session_imported_names: HashSet::new(),
    };

    // Concrete "global object" so `this` always has a tangible binding
    // (even when compiling a standalone program rather than a module).
    //
    // We bind it under a reserved name so later compiler stages can reference/capture it.
    let v_global = b.new_vreg(T_OBJECT);
    b.emit(Span::point(0), crate::ir::IrOp::ObjNew { dst: v_global });
    bind_local(&mut ctx, "__global", v_global, T_OBJECT);

    for s in &p.stmts {
        lower_stmt(s, &mut ctx, &mut b)?;
    }
    let (out, out_tid) = lower_expr(&p.expr, &mut ctx, &mut b)?;
    if out_tid != T_BYTES {
        return Err(CompileError::new(
            ErrorKind::Type,
            p.expr.span,
            "program must evaluate to bytes",
        ));
    }
    b.term(IrTerminator::Ret { value: out });

    let mut funcs = vec![b.func];
    funcs.extend(ctx.nested_funcs);

    Ok(IrModule {
        types: ctx.type_ctx.types,
        sigs: ctx.type_ctx.sigs,
        const_i64: ctx.const_i64,
        const_f64: ctx.const_f64,
        const_bytes: ctx.const_bytes,
        atoms: ctx.atoms,
        funcs,
        entry: 0,
    })
}

pub struct LoweredModuleInit {
    pub ir: IrModule,
    pub exports: HashMap<String, u32>,
    pub import_keys: Vec<String>,
    pub warnings: Vec<CompileWarning>,
}

pub fn lower_module_init_to_ir(
    module_name: &str,
    prog: &Program,
    sem: &SemanticInfo,
    is_entry: bool,
    is_repl: bool,
    import_exports: &HashMap<String, HashMap<String, TypeRepr>>,
) -> Result<LoweredModuleInit, CompileError> {
    let mut import_keys: Vec<String> = Vec::new();
    let mut key_to_alias: HashMap<String, String> = HashMap::new();
    let mut hidden_i = 0u32;

    for s in &prog.stmts {
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

    let mut type_ctx = sem.type_ctx.clone();
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

    let mut exports: HashMap<String, u32> = HashMap::new();
    for s in &prog.stmts {
        if let StmtKind::Let {
            exported,
            name,
            ty,
            expr,
            ..
        } = &s.node
        {
            if name == "_" {
                continue;
            }
            let should_export = *exported || is_repl;
            if !should_export {
                continue;
            }
            let tid = if let Some(ann) = ty {
                type_ctx.resolve_ty(ann)?
            } else if let Some(&tid) = sem.expr_types.get(&crate::hir::NodeId(expr.span)) {
                tid
            } else if *exported {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "exported let requires type annotation",
                ));
            } else {
                continue;
            };
            exports.insert(name.clone(), tid);
        }
    }

    let mut b = IrBuilder::new(Some(format!("module:{module_name}")));
    let mut ctx = LowerCtx {
        type_ctx,
        const_bytes: Vec::new(),
        const_i64: Vec::new(),
        const_f64: Vec::new(),
        atoms: vec![b"__proto__".to_vec(), b"init".to_vec()],
        atom_ids: HashMap::from([
            ("__proto__".to_string(), crate::jlyb::ATOM___PROTO__),
            ("init".to_string(), crate::jlyb::ATOM_INIT),
        ]),
        env_stack: vec![HashMap::new()],
        loop_stack: Vec::new(),
        fn_stack: Vec::new(),
        nested_funcs: Vec::new(),
        next_nested_fun_index: 0,
        pending_fn_self: None,
        user_top_level_fun_count: 1,
        exports_obj: Some(VRegId(0)),
        module_alias_exports,
        module_key_to_alias: key_to_alias,
        sem_binding_types: sem.binding_types.clone(),
        sem_expr_types: sem.expr_types.clone(),
        sem_const_inits: sem.const_inits.clone(),
        warnings: Vec::new(),
        method_table: HashMap::new(),
        proto_for_nominal: None,
        recording_method: None,
        last_const_fun_index: None,
        export_all_lets: is_repl,
        exported_bindings: HashSet::new(),
        session_obj: None,
        session_imported_names: HashSet::new(),
    };

    let param_count = 1 + import_keys.len();
    if param_count > 255 {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            Span::point(0),
            "too many module imports",
        ));
    }
    b.func.param_count = param_count as u8;

    // Param 0: exports object (also serves as the module "global object").
    let exports_v = b.new_vreg(T_OBJECT);
    bind_local(&mut ctx, "__global", exports_v, T_OBJECT);
    for key in &import_keys {
        let alias = ctx.module_key_to_alias.get(key).expect("alias").clone();
        let v = b.new_vreg(T_OBJECT);
        bind_local(&mut ctx, &alias, v, T_OBJECT);
    }

    for s in &prog.stmts {
        lower_stmt(s, &mut ctx, &mut b)?;
    }

    // REPL: re-export session-imported names so the next module in the chain can access them.
    // Without this, "o.one = 3" would not pass o through, and "o.one" would get null.
    if is_repl
        && !ctx.session_imported_names.is_empty()
        && ctx.session_obj.is_some()
    {
        let session_obj = ctx.session_obj.unwrap();
        let exports_obj = ctx.exports_obj.expect("repl has exports");
        let to_reexport: Vec<(String, u32)> = ctx
            .module_key_to_alias
            .get("__repl_session__")
            .and_then(|alias| ctx.module_alias_exports.get(alias))
            .map(|name_to_tid| {
                ctx.session_imported_names
                    .iter()
                    .filter_map(|name| name_to_tid.get(name).map(|&tid| (name.clone(), tid)))
                    .collect()
            })
            .unwrap_or_default();
        for (name, tid) in to_reexport {
            ensure_open_block(&mut b);
            let atom_id = intern_atom(&name, &mut ctx);
            let val = b.new_vreg(tid);
            b.emit(
                prog.expr.span,
                crate::ir::IrOp::ObjGetAtom {
                    dst: val,
                    obj: session_obj,
                    atom_id,
                },
            );
            b.emit(
                prog.expr.span,
                crate::ir::IrOp::ObjSetAtom {
                    obj: exports_obj,
                    atom_id,
                    value: val,
                },
            );
            exports.insert(name, tid);
        }
    }

    if is_entry {
        let (out, out_tid) = lower_expr(&prog.expr, &mut ctx, &mut b)?;
        if !is_repl && out_tid != T_BYTES {
            return Err(CompileError::new(
                ErrorKind::Type,
                prog.expr.span,
                "program must evaluate to bytes",
            ));
        }
        b.term(IrTerminator::Ret { value: out });
    } else {
        let _ = lower_expr(&prog.expr, &mut ctx, &mut b)?;
        let vnull = b.new_vreg(super::T_DYNAMIC);
        b.emit(prog.expr.span, crate::ir::IrOp::ConstNull { dst: vnull });
        b.term(IrTerminator::Ret { value: vnull });
    }

    let mut funcs = vec![b.func];
    funcs.extend(ctx.nested_funcs);

    Ok(LoweredModuleInit {
        ir: IrModule {
            types: ctx.type_ctx.types,
            sigs: ctx.type_ctx.sigs,
            const_i64: ctx.const_i64,
            const_f64: ctx.const_f64,
            const_bytes: ctx.const_bytes,
            atoms: ctx.atoms,
            funcs,
            entry: 0,
        },
        exports,
        import_keys,
        warnings: ctx.warnings,
    })
}
