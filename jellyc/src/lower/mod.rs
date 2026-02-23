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

 mod expr;
mod stmt;

use std::collections::HashMap;

use crate::ast::{Program, Span, StmtKind, Ty};
use crate::hir::{NodeId, SemanticInfo};

pub(crate) use expr::lower_expr;
pub(crate) use stmt::lower_stmt;
use crate::error::{CompileError, CompileWarning, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrModule, IrTerminator, TypeId, VRegId};
use crate::typectx::{TypeCtx, TypeRepr};

// Base program-module type IDs. Must match `TypeCtx::new_program_base()`.
const T_BOOL: TypeId = crate::typectx::T_BOOL; // 0
const T_ATOM: TypeId = crate::typectx::T_ATOM; // 1
const T_I8: TypeId = crate::typectx::T_I8; // 2
const T_I16: TypeId = crate::typectx::T_I16; // 3
const T_I32: TypeId = crate::typectx::T_I32; // 4
const T_I64: TypeId = crate::typectx::T_I64; // 5
const T_F16: TypeId = crate::typectx::T_F16; // 6
const T_F32: TypeId = crate::typectx::T_F32; // 7
const T_F64: TypeId = crate::typectx::T_F64; // 8
const T_BYTES: TypeId = crate::typectx::T_BYTES; // 9
const T_DYNAMIC: TypeId = crate::typectx::T_DYNAMIC; // 10
const T_OBJECT: TypeId = crate::typectx::T_OBJECT; // 11
const T_ARRAY_I32: TypeId = crate::typectx::T_ARRAY_I32; // 12
const T_ARRAY_BYTES: TypeId = crate::typectx::T_ARRAY_BYTES; // 13
const T_LIST_I32: TypeId = crate::typectx::T_LIST_I32; // 14
const T_LIST_BYTES: TypeId = crate::typectx::T_LIST_BYTES; // 15

pub(crate) fn is_object_kind(tc: &TypeCtx, tid: TypeId) -> bool {
    tc.types
        .get(tid as usize)
        .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
}

#[derive(Clone, Copy)]
pub(crate) struct Binding {
    pub v: VRegId,
    pub tid: TypeId,
}

pub(crate) struct LowerCtx {
    pub type_ctx: TypeCtx,
    pub const_bytes: Vec<Vec<u8>>,
    pub const_i64: Vec<i64>,
    pub const_f64: Vec<f64>,
    pub atoms: Vec<Vec<u8>>,
    pub atom_ids: HashMap<String, u32>,
    pub env_stack: Vec<HashMap<String, Binding>>,
    pub loop_stack: Vec<LoopTargets>,
    pub fn_stack: Vec<FnCtx>,
    pub nested_funcs: Vec<crate::ir::IrFunction>,
    pub pending_fn_self: Option<(String, TypeId)>,
    pub user_top_level_fun_count: u32,
    pub exports_obj: Option<VRegId>,
    pub module_alias_exports: HashMap<String, HashMap<String, TypeId>>,
    pub module_key_to_alias: HashMap<String, String>,
    pub sem_binding_types: HashMap<NodeId, TypeId>,
    pub warnings: Vec<CompileWarning>,
}

#[derive(Clone)]
pub(crate) struct FnCtx {
    pub ret_tid: TypeId,
    pub outer_env: Option<Vec<HashMap<String, Binding>>>,
    pub captures: HashMap<String, (Binding, VRegId)>,
    pub capture_order: Vec<String>,
    pub self_name: Option<String>,
    pub self_func_index: Option<u32>,
    pub self_fun_tid: Option<TypeId>,
    pub self_loop_head: BlockId,
}

#[derive(Clone, Copy)]
pub(crate) struct LoopTargets {
    pub break_tgt: crate::ir::BlockId,
    pub continue_tgt: crate::ir::BlockId,
}

fn intern_type_repr(ctx: &mut TypeCtx, tr: &TypeRepr, span: Span) -> Result<TypeId, CompileError> {
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
            let e = intern_type_repr(ctx, elem, span)?;
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
            let e = intern_type_repr(ctx, elem, span)?;
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
        pending_fn_self: None,
        user_top_level_fun_count: 1,
        exports_obj: None,
        module_alias_exports: HashMap::new(),
        module_key_to_alias: HashMap::new(),
        sem_binding_types: sem.binding_types.clone(),
        warnings: Vec::new(),
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
            exported: true,
            name,
            ty: Some(ty),
            ..
        } = &s.node
        {
            let tid = type_ctx.resolve_ty(ty)?;
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
        pending_fn_self: None,
        user_top_level_fun_count: 1,
        exports_obj: Some(VRegId(0)),
        module_alias_exports,
        module_key_to_alias: key_to_alias,
        sem_binding_types: sem.binding_types.clone(),
        warnings: Vec::new(),
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

    if is_entry {
        let (out, out_tid) = lower_expr(&prog.expr, &mut ctx, &mut b)?;
        if out_tid != T_BYTES {
            return Err(CompileError::new(ErrorKind::Type, prog.expr.span, "program must evaluate to bytes"));
        }
        b.term(IrTerminator::Ret { value: out });
    } else {
        let _ = lower_expr(&prog.expr, &mut ctx, &mut b)?;
        let vnull = b.new_vreg(T_DYNAMIC);
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

pub(crate) fn intern_atom(name: &str, ctx: &mut LowerCtx) -> u32 {
    if let Some(&id) = ctx.atom_ids.get(name) {
        return id;
    }
    let id = ctx.atoms.len() as u32;
    ctx.atoms.push(name.as_bytes().to_vec());
    ctx.atom_ids.insert(name.to_string(), id);
    id
}

pub(crate) fn lookup_var(ctx: &LowerCtx, name: &str, span: Span) -> Result<Binding, CompileError> {
    ctx.env_stack
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .copied()
        .ok_or_else(|| CompileError::new(ErrorKind::Name, span, format!("unknown variable '{}'", name)))
}

pub(crate) fn ensure_capture_binding(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    name: &str,
    span: Span,
) -> Result<Option<Binding>, CompileError> {
    let Some(fc) = ctx.fn_stack.last_mut() else {
        return Ok(None);
    };
    let Some(outer_env) = fc.outer_env.as_ref() else {
        return Ok(None);
    };

    if ctx.env_stack.iter().rev().any(|m| m.contains_key(name)) {
        return Ok(None);
    }

    let outer = outer_env
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .copied();
    let Some(ob) = outer else {
        return Ok(None);
    };

    if let Some((_, _slot)) = fc.captures.get(name) {
        let bd = lookup_var(ctx, name, span)?;
        return Ok(Some(bd));
    }

    let slot = b.new_vreg(if ctx.type_ctx.types.get(ob.tid as usize).map_or(false, |te| te.kind == crate::jlyb::TypeKind::Function) {
        ob.tid
    } else {
        T_DYNAMIC
    });
    fc.captures.insert(name.to_string(), (ob, slot));
    fc.capture_order.push(name.to_string());

    let out = b.new_vreg(ob.tid);
    match ob.tid {
        T_I8 => b.emit(span, crate::ir::IrOp::FromDynI8 { dst: out, src: slot }),
        T_I16 => b.emit(span, crate::ir::IrOp::FromDynI16 { dst: out, src: slot }),
        T_I32 => b.emit(span, crate::ir::IrOp::FromDynI32 { dst: out, src: slot }),
        T_I64 => b.emit(span, crate::ir::IrOp::FromDynI64 { dst: out, src: slot }),
        T_F16 => b.emit(span, crate::ir::IrOp::FromDynF16 { dst: out, src: slot }),
        T_F32 => b.emit(span, crate::ir::IrOp::FromDynF32 { dst: out, src: slot }),
        T_F64 => b.emit(span, crate::ir::IrOp::FromDynF64 { dst: out, src: slot }),
        T_BOOL => b.emit(span, crate::ir::IrOp::FromDynBool { dst: out, src: slot }),
        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(span, crate::ir::IrOp::FromDynPtr { dst: out, src: slot }),
        T_DYNAMIC => b.emit(span, crate::ir::IrOp::Mov { dst: out, src: slot }),
        _ if ctx.type_ctx.types.get(ob.tid as usize).map_or(false, |te| te.kind == crate::jlyb::TypeKind::Function) => {
            bind_local(ctx, name, slot, ob.tid);
            return Ok(Some(Binding { v: slot, tid: ob.tid }));
        }
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "unsupported capture type",
            ))
        }
    }
    bind_local(ctx, name, out, ob.tid);
    Ok(Some(Binding { v: out, tid: ob.tid }))
}

pub(crate) fn bind_local(ctx: &mut LowerCtx, name: &str, v: VRegId, tid: TypeId) {
    ctx.env_stack
        .last_mut()
        .expect("env stack")
        .insert(name.to_string(), Binding { v, tid });
}

pub(crate) fn resolve_opt_ty(t: &Option<Ty>, ctx: &mut LowerCtx) -> Result<Option<TypeId>, CompileError> {
    match t {
        Some(ty) => Ok(Some(ctx.type_ctx.resolve_ty(ty)?)),
        None => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExprKind, Spanned, StmtKind, TyKind};

    #[test]
    fn lower_module_named_imports_binds_locals() {
        let sp = Span::new(0, 1);
        let import_add = Spanned::new(
            StmtKind::ImportFrom {
                type_only: false,
                items: vec![("add".to_string(), None)],
                from: vec!["math".to_string()],
            },
            sp,
        );
        let import_y = Spanned::new(
            StmtKind::ImportFrom {
                type_only: false,
                items: vec![("x".to_string(), Some("y".to_string()))],
                from: vec!["consts".to_string()],
            },
            sp,
        );
        let call_add = Spanned::new(
            ExprKind::Call {
                callee: Box::new(Spanned::new(ExprKind::Var("add".to_string()), sp)),
                type_args: vec![],
                args: vec![
                    Spanned::new(ExprKind::Var("y".to_string()), sp),
                    Spanned::new(ExprKind::I32Lit(3), sp),
                ],
            },
            sp,
        );
        let let_r = Spanned::new(
            StmtKind::Let {
                exported: false,
                name: "r".to_string(),
                type_params: Vec::new(),
                ty: Some(Spanned::new(TyKind::Named("I32".to_string()), sp)),
                expr: call_add,
            },
            sp,
        );
        let prog = Program {
            stmts: vec![import_add, import_y, let_r],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), sp),
        };

        let mut import_exports: HashMap<String, HashMap<String, TypeRepr>> = HashMap::new();
        import_exports.insert(
            "math".to_string(),
            HashMap::from([(
                "add".to_string(),
                TypeRepr::Fun {
                    args: vec![TypeRepr::I32, TypeRepr::I32],
                    ret: Box::new(TypeRepr::I32),
                },
            )]),
        );
        import_exports.insert(
            "consts".to_string(),
            HashMap::from([("x".to_string(), TypeRepr::I32)]),
        );

        let (hir, info) = crate::semantic::analyze_module_init("entry", &prog, true, &import_exports).unwrap();
        let lowered = lower_module_init_to_ir("entry", &hir.program, &info, true, &import_exports).unwrap();
        assert!(lowered.ir.funcs.len() >= 1);
    }

    #[test]
    fn lower_minimal_bytes_program() {
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(0, 2)),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let m = lower_program_to_ir(&hir.program, &info).unwrap();
        assert_eq!(m.entry, 0);
        assert_eq!(m.funcs.len(), 1);
        assert_eq!(m.const_bytes.len(), 1);
    }

    #[test]
    fn lower_if_expression_builds_blocks_and_join() {
        let src_span = Span::new(0, 1);
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(
                ExprKind::If {
                    cond: Box::new(Spanned::new(ExprKind::BoolLit(true), src_span)),
                    then_br: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), src_span)),
                    else_br: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), src_span)),
                },
                src_span,
            ),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let m = lower_program_to_ir(&hir.program, &info).unwrap();
        let f = &m.funcs[0];
        assert!(f.blocks.len() >= 4, "expected entry+then+else+join");
    }

    #[test]
    fn lower_while_statement_builds_cond_body_exit_blocks() {
        let sp = Span::new(0, 1);
        let prog = Program {
            stmts: vec![Spanned::new(
                StmtKind::While {
                    cond: Spanned::new(ExprKind::BoolLit(false), sp),
                    body: vec![],
                },
                sp,
            )],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(2, 4)),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let m = lower_program_to_ir(&hir.program, &info).unwrap();
        let f = &m.funcs[0];
        assert!(
            f.blocks.iter().any(|b| b.label.as_deref() == Some("while_cond")),
            "missing while_cond block"
        );
        assert!(
            f.blocks.iter().any(|b| b.label.as_deref() == Some("while_body")),
            "missing while_body block"
        );
        assert!(
            f.blocks.iter().any(|b| b.label.as_deref() == Some("while_exit")),
            "missing while_exit block"
        );
    }

    #[test]
    fn lower_try_expression_builds_catch_and_join_blocks() {
        let sp = Span::new(0, 1);
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(
                ExprKind::Try {
                    body: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), sp)),
                    catch_name: Some("e".to_string()),
                    catch_body: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), sp)),
                },
                sp,
            ),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let m = lower_program_to_ir(&hir.program, &info).unwrap();
        let f = &m.funcs[0];
        assert!(
            f.blocks.iter().any(|b| b.label.as_deref() == Some("catch")),
            "missing catch block"
        );
        assert!(
            f.blocks.iter().any(|b| b.label.as_deref() == Some("try_join")),
            "missing try_join block"
        );
    }

    #[test]
    fn lower_self_recursive_fn_literal() {
        let sp = Span::new(0, 1);
        let ty_i32 = Spanned::new(TyKind::Named("I32".to_string()), sp);
        let ty_fun = Spanned::new(
            TyKind::Fun {
                args: vec![ty_i32.clone()],
                ret: Box::new(ty_i32.clone()),
            },
            sp,
        );

        let call_fib = Spanned::new(
            ExprKind::Call {
                callee: Box::new(Spanned::new(ExprKind::Var("fib".to_string()), sp)),
                type_args: vec![],
                args: vec![Spanned::new(ExprKind::Var("n".to_string()), sp)],
            },
            sp,
        );
        let fn_lit = Spanned::new(
            ExprKind::Fn {
                params: vec![("n".to_string(), None)],
                body: vec![Spanned::new(StmtKind::Return { expr: Some(call_fib) }, sp)],
                tail: None,
            },
            sp,
        );
        let st_let = Spanned::new(
            StmtKind::Let {
                exported: false,
                name: "fib".to_string(),
                type_params: Vec::new(),
                ty: Some(ty_fun),
                expr: fn_lit,
            },
            sp,
        );

        let prog = Program {
            stmts: vec![st_let],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), sp),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn lower_self_recursive_fn_literal_through_if() {
        let sp = Span::new(0, 1);
        let ty_i32 = Spanned::new(TyKind::Named("I32".to_string()), sp);
        let ty_fun = Spanned::new(
            TyKind::Fun {
                args: vec![ty_i32.clone()],
                ret: Box::new(ty_i32.clone()),
            },
            sp,
        );

        let call_fib_1 = Spanned::new(
            ExprKind::Call {
                callee: Box::new(Spanned::new(ExprKind::Var("fib".to_string()), sp)),
                type_args: vec![],
                args: vec![Spanned::new(
                    ExprKind::Sub(
                        Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp)),
                        Box::new(Spanned::new(ExprKind::I32Lit(1), sp)),
                    ),
                    sp,
                )],
            },
            sp,
        );
        let call_fib_2 = Spanned::new(
            ExprKind::Call {
                callee: Box::new(Spanned::new(ExprKind::Var("fib".to_string()), sp)),
                type_args: vec![],
                args: vec![Spanned::new(
                    ExprKind::Sub(
                        Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp)),
                        Box::new(Spanned::new(ExprKind::I32Lit(2), sp)),
                    ),
                    sp,
                )],
            },
            sp,
        );
        let else_expr = Spanned::new(ExprKind::Add(Box::new(call_fib_1), Box::new(call_fib_2)), sp);
        let if_expr = Spanned::new(
            ExprKind::If {
                cond: Box::new(Spanned::new(
                    ExprKind::Lt(
                        Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp)),
                        Box::new(Spanned::new(ExprKind::I32Lit(2), sp)),
                    ),
                    sp,
                )),
                then_br: Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp)),
                else_br: Box::new(else_expr),
            },
            sp,
        );
        let fn_lit = Spanned::new(
            ExprKind::Fn {
                params: vec![("n".to_string(), None)],
                body: vec![Spanned::new(StmtKind::Return { expr: Some(if_expr) }, sp)],
                tail: None,
            },
            sp,
        );
        let st_let = Spanned::new(
            StmtKind::Let {
                exported: false,
                name: "fib".to_string(),
                type_params: Vec::new(),
                ty: Some(ty_fun),
                expr: fn_lit,
            },
            sp,
        );

        let prog = Program {
            stmts: vec![st_let],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), sp),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        lower_program_to_ir(&hir.program, &info).unwrap();
    }
}
