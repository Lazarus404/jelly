use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Program, Span, Stmt, StmtKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::typectx::TypeCtx;

const T_BYTES: TypeId = 0;
const T_BOOL: TypeId = 1;
const T_I32: TypeId = 2;
const T_DYNAMIC: TypeId = 3;
const T_ARRAY_I32: TypeId = 4;
const T_ARRAY_BYTES: TypeId = 5;
const T_OBJECT: TypeId = 6;
const T_LIST_I32: TypeId = 7;
const T_LIST_BYTES: TypeId = 8;
const T_ATOM: TypeId = 9;

fn is_object_kind(tc: &TypeCtx, tid: TypeId) -> bool {
    tc.types
        .get(tid as usize)
        .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
}

#[derive(Clone, Debug)]
pub enum TypeRepr {
    Bytes,
    Bool,
    I32,
    Dynamic,
    Object,
    Atom,
    Array(Box<TypeRepr>),
    List(Box<TypeRepr>),
    Fun { args: Vec<TypeRepr>, ret: Box<TypeRepr> },
    Tuple(Vec<TypeRepr>),
}

pub fn type_repr_from_ty(t: &Ty) -> Result<TypeRepr, CompileError> {
    fn go(t: &Ty) -> Result<TypeRepr, CompileError> {
        match &t.node {
            crate::ast::TyKind::Named(n) => match n.as_str() {
                "Bytes" | "bytes" => Ok(TypeRepr::Bytes),
                "Bool" | "bool" => Ok(TypeRepr::Bool),
                "I32" | "i32" => Ok(TypeRepr::I32),
                "Any" | "Dynamic" | "dynamic" => Ok(TypeRepr::Dynamic),
                "Object" | "object" => Ok(TypeRepr::Object),
                "Atom" | "atom" => Ok(TypeRepr::Atom),
                _ => Err(CompileError::new(
                    ErrorKind::Type,
                    t.span,
                    format!("unknown type '{}'", n),
                )),
            },
            crate::ast::TyKind::Generic { base, args } => {
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
            crate::ast::TyKind::Tuple(elems) => {
                let mut out: Vec<TypeRepr> = Vec::with_capacity(elems.len());
                for e in elems {
                    out.push(go(e)?);
                }
                Ok(TypeRepr::Tuple(out))
            }
            crate::ast::TyKind::Fun { args, ret } => {
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

pub fn type_repr_from_jlyb(m: &crate::jlyb::Module, tid: u32) -> Result<TypeRepr, CompileError> {
    const TUPLE_TAG: u32 = 0x8000_0000;
    fn go(m: &crate::jlyb::Module, tid: u32) -> Result<TypeRepr, CompileError> {
        let te = m.types.get(tid as usize).ok_or_else(|| {
            CompileError::new(ErrorKind::Internal, Span::point(0), "bad type id in module ABI")
        })?;
        match te.kind {
            crate::jlyb::TypeKind::Bytes => Ok(TypeRepr::Bytes),
            crate::jlyb::TypeKind::Bool => Ok(TypeRepr::Bool),
            crate::jlyb::TypeKind::I32 => Ok(TypeRepr::I32),
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

#[derive(Clone, Copy)]
struct Binding {
    v: VRegId,
    tid: TypeId,
}

struct LowerCtx {
    type_ctx: TypeCtx,
    const_bytes: Vec<Vec<u8>>,
    atoms: Vec<Vec<u8>>,
    atom_ids: HashMap<String, u32>,
    env_stack: Vec<HashMap<String, Binding>>,
    loop_stack: Vec<LoopTargets>,
    fn_stack: Vec<FnCtx>,
    nested_funcs: Vec<crate::ir::IrFunction>,
    pending_fn_self: Option<(String, TypeId)>,
    user_top_level_fun_count: u32,
    exports_obj: Option<VRegId>,
    // Module support (IR backend): aliases bound to imported module namespace objects.
    // If an alias is present here, member calls must NOT bind `this`.
    module_alias_exports: HashMap<String, HashMap<String, TypeId>>, // alias -> export name -> type id
    module_key_to_alias: HashMap<String, String>,                  // module key (dotted) -> alias name
}

#[derive(Clone)]
struct FnCtx {
    ret_tid: TypeId,
    // When lowering a fn literal, this holds the outer lexical environment for capture lookup,
    // and records the captures in the order we first see them.
    outer_env: Option<Vec<HashMap<String, Binding>>>,
    captures: HashMap<String, (Binding, VRegId)>, // name -> (outer binding, capture-slot vreg (Dynamic))
    capture_order: Vec<String>,
    self_name: Option<String>,
    self_func_index: Option<u32>,
    self_fun_tid: Option<TypeId>,
    self_loop_head: BlockId,
}

#[derive(Clone, Copy)]
struct LoopTargets {
    break_tgt: crate::ir::BlockId,
    continue_tgt: crate::ir::BlockId,
}

pub fn lower_program_to_ir(p: &Program) -> Result<IrModule, CompileError> {
    let mut b = IrBuilder::new(Some("main".to_string()));
    let mut ctx = LowerCtx {
        type_ctx: TypeCtx::new_program_base(),
        const_bytes: Vec::new(),
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
    };

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
        const_bytes: ctx.const_bytes,
        atoms: ctx.atoms,
        funcs,
        entry: 0,
    })
}

fn intern_type_repr(ctx: &mut TypeCtx, tr: &TypeRepr, span: Span) -> Result<TypeId, CompileError> {
    match tr {
        TypeRepr::Bytes => Ok(T_BYTES),
        TypeRepr::Bool => Ok(T_BOOL),
        TypeRepr::I32 => Ok(T_I32),
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

pub struct LoweredModuleInit {
    pub ir: IrModule,
    pub exports: HashMap<String, u32>, // export name -> type id (in `ir.types`)
    pub import_keys: Vec<String>,      // module keys, in init param order
}

pub fn lower_module_init_to_ir(
    module_name: &str,
    prog: &Program,
    is_entry: bool,
    import_exports: &HashMap<String, HashMap<String, TypeRepr>>, // module key -> exports
) -> Result<LoweredModuleInit, CompileError> {
    // Determine import parameter order and assign a canonical param alias per module key.
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

    // Build module-alias export typing tables.
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

    // Collect exported bindings (requires explicit types).
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

    // Param 0: exports object (implicit; not user-visible).
    let _exports_v = b.new_vreg(T_OBJECT);
    // Params 1..: imported module namespace objects, bound to their canonical aliases.
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
        b.emit(prog.expr.span, IrOp::ConstNull { dst: vnull });
        b.term(IrTerminator::Ret { value: vnull });
    }

    let mut funcs = vec![b.func];
    funcs.extend(ctx.nested_funcs);

    Ok(LoweredModuleInit {
        ir: IrModule {
            types: ctx.type_ctx.types,
            sigs: ctx.type_ctx.sigs,
            const_bytes: ctx.const_bytes,
            atoms: ctx.atoms,
            funcs,
            entry: 0,
        },
        exports,
        import_keys,
    })
}

fn intern_atom(name: &str, ctx: &mut LowerCtx) -> u32 {
    if let Some(&id) = ctx.atom_ids.get(name) {
        return id;
    }
    let id = ctx.atoms.len() as u32;
    ctx.atoms.push(name.as_bytes().to_vec());
    ctx.atom_ids.insert(name.to_string(), id);
    id
}

fn lookup_var(ctx: &LowerCtx, name: &str, span: Span) -> Result<Binding, CompileError> {
    ctx.env_stack
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .copied()
        .ok_or_else(|| CompileError::new(ErrorKind::Name, span, format!("unknown variable '{}'", name)))
}

fn ensure_capture_binding(
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

    // If it's already a local, nothing to do.
    if ctx.env_stack.iter().rev().any(|m| m.contains_key(name)) {
        return Ok(None);
    }

    // Find in outer env.
    let outer = outer_env
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .copied();
    let Some(ob) = outer else {
        return Ok(None);
    };

    // If already captured, bind to the unboxed local created previously.
    if let Some((_, _slot)) = fc.captures.get(name) {
        let bd = lookup_var(ctx, name, span)?;
        return Ok(Some(bd));
    }

    // Create a Dynamic capture-slot vreg (pinned to the end of the callee frame by codegen).
    let slot = b.new_vreg(T_DYNAMIC);
    fc.captures.insert(name.to_string(), (ob, slot));
    fc.capture_order.push(name.to_string());

    // Unbox into a typed local at first use and bind the name to that typed vreg.
    let out = b.new_vreg(ob.tid);
    match ob.tid {
        T_I32 => b.emit(span, IrOp::FromDynI32 { dst: out, src: slot }),
        T_BOOL => b.emit(span, IrOp::FromDynBool { dst: out, src: slot }),
        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(span, IrOp::FromDynPtr { dst: out, src: slot }),
        T_DYNAMIC => b.emit(span, IrOp::Mov { dst: out, src: slot }),
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

fn bind_local(ctx: &mut LowerCtx, name: &str, v: VRegId, tid: TypeId) {
    ctx.env_stack
        .last_mut()
        .expect("env stack")
        .insert(name.to_string(), Binding { v, tid });
}

fn elem_tid_for_array(arr_tid: TypeId) -> Option<TypeId> {
    match arr_tid {
        T_ARRAY_I32 => Some(T_I32),
        T_ARRAY_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn elem_tid_for_list(list_tid: TypeId) -> Option<TypeId> {
    match list_tid {
        T_LIST_I32 => Some(T_I32),
        T_LIST_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn lower_pin_as(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    name: &str,
    expect_tid: TypeId,
    span: Span,
) -> Result<(VRegId, TypeId), CompileError> {
    let bd = lookup_var(ctx, name, span)?;
    if bd.tid == expect_tid {
        return Ok((bd.v, bd.tid));
    }
    if bd.tid != T_DYNAMIC {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "pinned name has incompatible type",
        ));
    }
    let out = b.new_vreg(expect_tid);
    match expect_tid {
        T_I32 => b.emit(span, IrOp::FromDynI32 { dst: out, src: bd.v }),
        T_BOOL => b.emit(span, IrOp::FromDynBool { dst: out, src: bd.v }),
        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(span, IrOp::FromDynPtr { dst: out, src: bd.v }),
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "unsupported pinned conversion",
            ))
        }
    }
    Ok((out, expect_tid))
}

fn chain_check(
    b: &mut IrBuilder,
    next_check: BlockId,
    cond_v: VRegId,
    label: String,
) -> BlockId {
    let pass_b = b.new_block(Some(label));
    b.term(IrTerminator::JmpIf {
        cond: cond_v,
        then_tgt: pass_b,
        else_tgt: next_check,
    });
    b.set_block(pass_b);
    pass_b
}

fn emit_pin_check_dynamic_subject(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    subj_dyn: VRegId,
    span: Span,
    name: &str,
    next_check: crate::ir::BlockId,
    bind_b: crate::ir::BlockId,
) -> Result<(), CompileError> {
    let bd = lookup_var(ctx, name, span)?;
    let v_rhs_dyn = if bd.tid == T_DYNAMIC {
        bd.v
    } else {
        let out = b.new_vreg(T_DYNAMIC);
        b.emit(span, IrOp::ToDyn { dst: out, src: bd.v });
        out
    };
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(span, IrOp::Physeq { dst: v_cond, a: subj_dyn, b: v_rhs_dyn });
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}

fn resolve_opt_ty(t: &Option<Ty>, ctx: &mut LowerCtx) -> Result<Option<TypeId>, CompileError> {
    match t {
        Some(ty) => Ok(Some(ctx.type_ctx.resolve_ty(ty)?)),
        None => Ok(None),
    }
}

fn lower_stmt(s: &Stmt, ctx: &mut LowerCtx, b: &mut IrBuilder) -> Result<(), CompileError> {
    fn ensure_open_block(b: &mut IrBuilder) {
        if !b.is_open() {
            let nb = b.new_block(Some("cont".to_string()));
            b.set_block(nb);
        }
    }

    match &s.node {
        StmtKind::ImportModule { path, alias } => {
            // Bind the user-visible alias to the module namespace object parameter.
            let key = path.join(".");
            let canon = ctx.module_key_to_alias.get(&key).cloned().ok_or_else(|| {
                CompileError::new(ErrorKind::Name, s.span, format!("unknown module '{}'", key))
            })?;
            let bd = lookup_var(ctx, &canon, s.span)?;
            if canon.as_str() != alias {
                bind_local(ctx, alias, bd.v, bd.tid);
            }
            Ok(())
        }
        StmtKind::ImportFrom {
            type_only: _,
            items,
            from,
        } => {
            ensure_open_block(b);
            let key = from.join(".");
            let canon = ctx.module_key_to_alias.get(&key).cloned().ok_or_else(|| {
                CompileError::new(ErrorKind::Name, s.span, format!("unknown module '{}'", key))
            })?;
            let mod_bd = lookup_var(ctx, &canon, s.span)?;

            for (name, alias) in items {
                let bind = alias.as_ref().unwrap_or(name);
                let tid = ctx
                    .module_alias_exports
                    .get(&canon)
                    .and_then(|m| m.get(name))
                    .copied()
                    .ok_or_else(|| {
                        CompileError::new(
                            ErrorKind::Name,
                            s.span,
                            format!("unknown export '{}.{}'", canon, name),
                        )
                    })?;
                let atom_id = intern_atom(name, ctx);
                let out = b.new_vreg(tid);
                b.emit(s.span, IrOp::ObjGetAtom { dst: out, obj: mod_bd.v, atom_id });
                bind_local(ctx, bind, out, tid);
            }
            Ok(())
        }
        StmtKind::Prototype { .. } => Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "prototype must be expanded before lowering",
        )),
        StmtKind::Let {
            exported,
            name,
            type_params,
            ty,
            expr,
        } => {
            ensure_open_block(b);
            if !type_params.is_empty() {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "template lets must be expanded before lowering",
                ));
            }
            if ctx.env_stack.iter().any(|m| m.contains_key(name)) {
                return Err(CompileError::new(
                    ErrorKind::Name,
                    s.span,
                    format!("duplicate let binding '{}'", name),
                ));
            }
            let expect = resolve_opt_ty(ty, ctx)?;
            // Recursion sugar for function literals: pre-bind the name so the initializer can
            // reference itself (lowered as `CONST_FUN` in the function body).
            if let (Some(et), true) = (expect, matches!(&expr.node, ExprKind::Fn { .. })) {
                ctx.pending_fn_self = Some((name.clone(), et));
                let dst = b.new_vreg(et);
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(name.clone(), Binding { v: dst, tid: et });
                let (v, tid) = lower_expr_expect(expr, Some(et), ctx, b)?;
                ctx.pending_fn_self = None;
                if tid != et {
                    return Err(CompileError::new(ErrorKind::Type, s.span, "let initializer type mismatch"));
                }
                if v != dst {
                    b.emit(s.span, IrOp::Mov { dst, src: v });
                }
                if *exported {
                    let exports_obj = ctx.exports_obj.ok_or_else(|| {
                        CompileError::new(
                            ErrorKind::Name,
                            s.span,
                            "export requires module compilation (use --backend ir with imports/exports)",
                        )
                    })?;
                    let atom_id = intern_atom(name, ctx);
                    b.emit(s.span, IrOp::ObjSetAtom { obj: exports_obj, atom_id, value: dst });
                }
                Ok(())
            } else {
                let (v, tid) = lower_expr_expect(expr, expect, ctx, b)?;
                ctx.pending_fn_self = None;
                if let Some(et) = expect {
                    if tid != et {
                        return Err(CompileError::new(ErrorKind::Type, s.span, "let initializer type mismatch"));
                    }
                }
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(name.clone(), Binding { v, tid });
                if *exported {
                    let exports_obj = ctx.exports_obj.ok_or_else(|| {
                        CompileError::new(
                            ErrorKind::Name,
                            s.span,
                            "export requires module compilation (use --backend ir with imports/exports)",
                        )
                    })?;
                    let atom_id = intern_atom(name, ctx);
                    b.emit(s.span, IrOp::ObjSetAtom { obj: exports_obj, atom_id, value: v });
                }
                Ok(())
            }
        }
        StmtKind::Assign { name, expr } => {
            ensure_open_block(b);
            let dst = *ctx.env_stack.iter().rev().find_map(|m| m.get(name)).ok_or_else(|| {
                CompileError::new(ErrorKind::Name, s.span, format!("unknown variable '{}'", name))
            })?;
            let (rhs, rt) = lower_expr(expr, ctx, b)?;
            if rt != dst.tid {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    format!("assignment to '{}' changes type", name),
                ));
            }
            b.emit(s.span, IrOp::Mov { dst: dst.v, src: rhs });
            Ok(())
        }
        StmtKind::MemberAssign { base, name, expr } => {
            ensure_open_block(b);
            let (v_obj, t_obj) = lower_expr(base, ctx, b)?;
            let te = ctx
                .type_ctx
                .types
                .get(t_obj as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, base.span, "bad member assignment base type id"))?;
            if te.kind != crate::jlyb::TypeKind::Object {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    base.span,
                    "member assignment target must be Object",
                ));
            }
            if ctx.type_ctx.is_tuple_type(t_obj) {
                return Err(CompileError::new(ErrorKind::Type, base.span, "tuples are immutable"));
            }
            let (v_val, _t_val) = lower_expr(expr, ctx, b)?;
            let atom_id = intern_atom(name.as_str(), ctx);
            b.emit(s.span, IrOp::ObjSetAtom { obj: v_obj, atom_id, value: v_val });
            Ok(())
        }
        StmtKind::While { cond, body } => {
            ensure_open_block(b);
            let from = b.cur_block();
            let cond_b = b.new_block(Some("while_cond".to_string()));
            let body_b = b.new_block(Some("while_body".to_string()));
            let exit_b = b.new_block(Some("while_exit".to_string()));

            // jump into cond
            b.set_block(from);
            b.term(IrTerminator::Jmp { target: cond_b });

            // cond block
            b.set_block(cond_b);
            let (v_cond, t_cond) = lower_expr(cond, ctx, b)?;
            if t_cond != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, cond.span, "while condition must be bool"));
            }
            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: body_b,
                else_tgt: exit_b,
            });

            // body block
            b.set_block(body_b);
            ctx.loop_stack.push(LoopTargets {
                break_tgt: exit_b,
                continue_tgt: cond_b,
            });
            ctx.env_stack.push(HashMap::new());
            for st in body {
                lower_stmt(st, ctx, b)?;
            }
            ctx.env_stack.pop();
            ctx.loop_stack.pop();

            ensure_open_block(b);
            let tail = b.cur_block();
            b.set_block(tail);
            b.term(IrTerminator::Jmp { target: cond_b });

            // continue after loop
            b.set_block(exit_b);
            Ok(())
        }
        StmtKind::Break => {
            ensure_open_block(b);
            let tgt = ctx
                .loop_stack
                .last()
                .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "break used outside of while"))?
                .break_tgt;
            b.term(IrTerminator::Jmp { target: tgt });
            // continue lowering into a dead block (unreachable)
            let nb = b.new_block(Some("after_break".to_string()));
            b.set_block(nb);
            Ok(())
        }
        StmtKind::Continue => {
            ensure_open_block(b);
            let tgt = ctx
                .loop_stack
                .last()
                .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "continue used outside of while"))?
                .continue_tgt;
            b.term(IrTerminator::Jmp { target: tgt });
            let nb = b.new_block(Some("after_continue".to_string()));
            b.set_block(nb);
            Ok(())
        }
        StmtKind::Throw { expr } => {
            ensure_open_block(b);
            let (v, tid) = lower_expr(expr, ctx, b)?;
            let payload = if tid == T_DYNAMIC {
                v
            } else {
                let out = b.new_vreg(T_DYNAMIC);
                b.emit(s.span, IrOp::ToDyn { dst: out, src: v });
                out
            };
            b.emit(s.span, IrOp::Throw { payload });
            Ok(())
        }
        StmtKind::Return { expr } => {
            ensure_open_block(b);
            let fc = ctx
                .fn_stack
                .last()
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, s.span, "return outside of function lowering"))?
                .clone();
            let ret_tid = fc.ret_tid;

            fn emit_tail_self_call(
                e: &Expr,
                callee_name: &str,
                args: &[Expr],
                fc: &FnCtx,
                ret_tid: TypeId,
                ctx: &mut LowerCtx,
                b: &mut IrBuilder,
            ) -> Result<(), CompileError> {
                if fc.self_name.as_deref() != Some(callee_name) {
                    return Err(CompileError::new(ErrorKind::Internal, e.span, "not a self tail call"));
                }
                let self_tid = fc
                    .self_fun_tid
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing self fun tid"))?;
                let te = ctx
                    .type_ctx
                    .types
                    .get(self_tid as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad self fun tid"))?;
                if te.kind != crate::jlyb::TypeKind::Function {
                    return Err(CompileError::new(ErrorKind::Internal, e.span, "self is not a function"));
                }
                let sig_id = te.p0;
                let (sig_args, sig_ret) = ctx
                    .type_ctx
                    .sigs
                    .get(sig_id as usize)
                    .map(|s| (s.args.clone(), s.ret_type))
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad self fun sig id"))?;
                if sig_ret != ret_tid {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "tail call return type mismatch"));
                }
                if sig_args.len() != args.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "tail call arity mismatch"));
                }

                // Compute all args into temporaries first (prevents clobbering).
                let mut tmps: Vec<VRegId> = Vec::with_capacity(args.len());
                for (i, a) in args.iter().enumerate() {
                    let want = sig_args[i];
                    let (va, ta) = lower_expr_expect(a, Some(want), ctx, b)?;
                    if ta != want {
                        return Err(CompileError::new(ErrorKind::Type, a.span, "tail call argument type mismatch"));
                    }
                    let tmp = b.new_vreg(want);
                    b.emit(a.span, IrOp::Mov { dst: tmp, src: va });
                    tmps.push(tmp);
                }
                // Assign into parameter vregs 0..n-1.
                for (i, tmp) in tmps.iter().enumerate() {
                    b.emit(
                        e.span,
                        IrOp::Mov {
                            dst: VRegId(i as u32),
                            src: *tmp,
                        },
                    );
                }
                b.term(IrTerminator::Jmp {
                    target: fc.self_loop_head,
                });
                Ok(())
            }

            fn lower_return_expr(
                e: &Expr,
                fc: &FnCtx,
                ret_tid: TypeId,
                ctx: &mut LowerCtx,
                b: &mut IrBuilder,
            ) -> Result<(), CompileError> {
                // Tail recursion through `if`: compile the if as control flow, and let each
                // branch either return a value or become a tail self-call loop.
                if let ExprKind::If { cond, then_br, else_br } = &e.node {
                    let (v_cond, t_cond) = lower_expr(cond, ctx, b)?;
                    if t_cond != T_BOOL {
                        return Err(CompileError::new(ErrorKind::Type, cond.span, "if condition must be bool"));
                    }
                    let then_b = b.new_block(Some("ret_if_then".to_string()));
                    let else_b = b.new_block(Some("ret_if_else".to_string()));
                    b.term(IrTerminator::JmpIf {
                        cond: v_cond,
                        then_tgt: then_b,
                        else_tgt: else_b,
                    });
                    b.set_block(then_b);
                    lower_return_expr(then_br, fc, ret_tid, ctx, b)?;
                    b.set_block(else_b);
                    lower_return_expr(else_br, fc, ret_tid, ctx, b)?;
                    return Ok(());
                }

                // Direct self tail-call.
                if let ExprKind::Call { callee, type_args, args } = &e.node {
                    if type_args.is_empty() {
                        if let ExprKind::Var(n) = &callee.node {
                            if fc.self_name.as_deref() == Some(n.as_str()) {
                                return emit_tail_self_call(e, n, args, fc, ret_tid, ctx, b);
                            }
                        }
                    }
                }

                // Normal return.
                let (v, tid) = lower_expr_expect(e, Some(ret_tid), ctx, b)?;
                if tid != ret_tid {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "return type mismatch"));
                }
                b.term(IrTerminator::Ret { value: v });
                Ok(())
            }

            match expr {
                Some(e) => lower_return_expr(e, &fc, ret_tid, ctx, b),
                None => {
                    if ret_tid != T_DYNAMIC {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            s.span,
                            "return without value only allowed in functions returning Any/Dynamic",
                        ));
                    }
                    let v = b.new_vreg(T_DYNAMIC);
                    b.emit(s.span, IrOp::ConstNull { dst: v });
                    b.term(IrTerminator::Ret { value: v });
                    Ok(())
                }
            }
        }
        // Control-flow + container ops will move here as IR lowering becomes primary.
        _ => Err(CompileError::new(
            ErrorKind::Codegen,
            s.span,
            "IR lowering: statement not implemented yet",
        )),
    }
}

fn emit_bytes_eq(
    span: Span,
    dst: VRegId,
    a: VRegId,
    b2: VRegId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    let sig_args = [T_BYTES, T_BYTES];
    let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);
    let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args);

    let vcallee = b.new_vreg(fun_tid);
    b.emit(
        span,
        IrOp::ConstFun {
            dst: vcallee,
            func_index: crate::jlyb::PRELUDE_BYTES_EQ,
        },
    );

    // Marshal args into a contiguous vreg window.
    let arg0 = b.new_vreg(T_BYTES);
    b.emit(span, IrOp::Mov { dst: arg0, src: a });
    let arg1 = b.new_vreg(T_BYTES);
    b.emit(span, IrOp::Mov { dst: arg1, src: b2 });

    b.emit(
        span,
        IrOp::Call {
            dst,
            callee: vcallee,
            sig_id,
            arg_base: arg0,
            nargs: 2,
        },
    );
    Ok(())
}

fn lower_tuple_eq(
    span: Span,
    a: VRegId,
    b2: VRegId,
    tup_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    // Ensure we are in an open block.
    if !b.is_open() {
        let nb = b.new_block(Some("cont".to_string()));
        b.set_block(nb);
    }

    let elems = ctx
        .type_ctx
        .tuple_elems(tup_tid)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad tuple type"))?
        .to_vec();

    let out = b.new_vreg(T_BOOL);
    let fail_b = b.new_block(Some("tup_eq_fail".to_string()));
    let ok_b = b.new_block(Some("tup_eq_ok".to_string()));
    let join_b = b.new_block(Some("tup_eq_join".to_string()));

    if elems.is_empty() {
        // () == () is true.
        b.emit(span, IrOp::ConstBool { dst: out, imm: true });
        return Ok(out);
    } else {
        // Compare each element with early-exit on mismatch.
        for (i, &et) in elems.iter().enumerate() {
            let atom_id = intern_atom(&i.to_string(), ctx);
            let va = b.new_vreg(et);
            let vb = b.new_vreg(et);
            b.emit(span, IrOp::ObjGetAtom { dst: va, obj: a, atom_id });
            b.emit(span, IrOp::ObjGetAtom { dst: vb, obj: b2, atom_id });

            let v_eq = b.new_vreg(T_BOOL);
            match et {
                T_I32 => b.emit(span, IrOp::EqI32 { dst: v_eq, a: va, b: vb }),
                T_BOOL => b.emit(span, IrOp::Physeq { dst: v_eq, a: va, b: vb }),
                T_BYTES => emit_bytes_eq(span, v_eq, va, vb, ctx, b)?,
                _ if ctx.type_ctx.is_tuple_type(et) => {
                    // Nested tuples.
                    let nested = lower_tuple_eq(span, va, vb, et, ctx, b)?;
                    b.emit(span, IrOp::Mov { dst: v_eq, src: nested });
                }
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        span,
                        "tuple element equality not supported for this type yet",
                    ))
                }
            }

            let then_tgt = if i + 1 == elems.len() {
                ok_b
            } else {
                let nb = b.new_block(Some(format!("tup_eq_next{}", i)));
                nb
            };
            b.term(IrTerminator::JmpIf {
                cond: v_eq,
                then_tgt,
                else_tgt: fail_b,
            });
            b.set_block(then_tgt);
        }
    }

    // ok: true
    b.set_block(ok_b);
    b.emit(span, IrOp::ConstBool { dst: out, imm: true });
    b.term(IrTerminator::Jmp { target: join_b });

    // fail: false
    b.set_block(fail_b);
    b.emit(span, IrOp::ConstBool { dst: out, imm: false });
    b.term(IrTerminator::Jmp { target: join_b });

    // join: value in `out`
    b.set_block(join_b);
    Ok(out)
}

fn lower_expr(e: &Expr, ctx: &mut LowerCtx, b: &mut IrBuilder) -> Result<(VRegId, TypeId), CompileError> {
    lower_expr_expect(e, None, ctx, b)
}

fn lower_expr_expect(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    #[allow(unreachable_patterns)]
    match &e.node {
        ExprKind::BytesLit(bytes) => {
            let idx = ctx.const_bytes.len() as u32;
            ctx.const_bytes.push(bytes.clone());
            let v = b.new_vreg(T_BYTES);
            b.emit(e.span, IrOp::ConstBytes { dst: v, pool_index: idx });
            Ok((v, T_BYTES))
        }
        ExprKind::BoolLit(x) => {
            let v = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::ConstBool { dst: v, imm: *x });
            Ok((v, T_BOOL))
        }
        ExprKind::I32Lit(x) => {
            let v = b.new_vreg(T_I32);
            b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
            Ok((v, T_I32))
        }
        ExprKind::Null => {
            let v = b.new_vreg(T_DYNAMIC);
            b.emit(e.span, IrOp::ConstNull { dst: v });
            Ok((v, T_DYNAMIC))
        }
        ExprKind::Var(name) => {
            if let Some(fc) = ctx.fn_stack.last() {
                if fc.self_name.as_deref() == Some(name.as_str()) {
                    let tid = fc
                        .self_fun_tid
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing self function type"))?;
                    let func_index = fc
                        .self_func_index
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing self func index"))?;
                    let v = b.new_vreg(tid);
                    b.emit(e.span, IrOp::ConstFun { dst: v, func_index });
                    return Ok((v, tid));
                }
            }
            if ctx.env_stack.iter().rev().find_map(|m| m.get(name)).is_none() {
                let _ = ensure_capture_binding(ctx, b, name, e.span)?;
            }
            let bd = lookup_var(ctx, name, e.span)?;
            if let Some(et) = expect {
                if bd.tid == et {
                    return Ok((bd.v, bd.tid));
                }
                // Nominal object upcast: allow `Box<I32>` where plain `Object` is expected.
                if et == T_OBJECT && is_object_kind(&ctx.type_ctx, bd.tid) && bd.tid != T_OBJECT {
                    let out = b.new_vreg(T_OBJECT);
                    b.emit(e.span, IrOp::Mov { dst: out, src: bd.v });
                    return Ok((out, T_OBJECT));
                }
                // Implicit boxing/unboxing at Dynamic boundaries.
                if et == T_DYNAMIC {
                    let out = b.new_vreg(T_DYNAMIC);
                    b.emit(e.span, IrOp::ToDyn { dst: out, src: bd.v });
                    return Ok((out, T_DYNAMIC));
                }
                if bd.tid == T_DYNAMIC {
                    let out = b.new_vreg(et);
                    match et {
                        T_I32 => b.emit(e.span, IrOp::FromDynI32 { dst: out, src: bd.v }),
                        T_BOOL => b.emit(e.span, IrOp::FromDynBool { dst: out, src: bd.v }),
                        // bytes/object/array ptr kinds
                        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(e.span, IrOp::FromDynPtr { dst: out, src: bd.v }),
                        _ => {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "unsupported Dynamic->typed conversion",
                            ))
                        }
                    }
                    return Ok((out, et));
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "type mismatch (no implicit conversion)",
                ));
            }
            Ok((bd.v, bd.tid))
        }
        ExprKind::Member { base, name } => {
            // Namespaces must be used as call targets (handled in ExprKind::Call builtin logic).
            if let ExprKind::Var(ns) = &base.node {
                if matches!(
                    ns.as_str(),
                    "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
                ) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "namespace members must be called (e.g. Bytes.len(x))",
                    ));
                }
            }

            let (v_obj, t_obj) = lower_expr(base, ctx, b)?;
            let te = ctx
                .type_ctx
                .types
                .get(t_obj as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad member base type id"))?;
            if te.kind != crate::jlyb::TypeKind::Object {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "member access currently only supported for Object (obj.field)",
                ));
            }

            let atom_id = intern_atom(name.as_str(), ctx);

            // If this is a module namespace object, prefer the module's declared export type.
            if let ExprKind::Var(alias) = &base.node {
                if let Some(exports) = ctx.module_alias_exports.get(alias) {
                    let out_tid = exports.get(name).copied().or(expect).unwrap_or(T_DYNAMIC);
                    let out = b.new_vreg(out_tid);
                    b.emit(e.span, IrOp::ObjGetAtom { dst: out, obj: v_obj, atom_id });
                    if let Some(et) = expect {
                        if et == out_tid {
                            return Ok((out, out_tid));
                        }
                        if et == T_DYNAMIC && out_tid != T_DYNAMIC {
                            let vd = b.new_vreg(T_DYNAMIC);
                            b.emit(e.span, IrOp::ToDyn { dst: vd, src: out });
                            return Ok((vd, T_DYNAMIC));
                        }
                        if out_tid == T_DYNAMIC {
                            return Ok((out, T_DYNAMIC));
                        }
                        return Err(CompileError::new(ErrorKind::Type, e.span, "module export type mismatch"));
                    }
                    return Ok((out, out_tid));
                }
            }

            if ctx.type_ctx.is_tuple_type(t_obj) {
                // Tuple element access: `t.0`, `t.1`, ...
                let idx: usize = name
                    .parse()
                    .map_err(|_| CompileError::new(ErrorKind::Type, e.span, "tuple element access must be .<index>"))?;
                let elems = ctx
                    .type_ctx
                    .tuple_elems(t_obj)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad tuple type"))?;
                if idx >= elems.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "tuple index out of range"));
                }
                let elem_tid = elems[idx];
                let out = b.new_vreg(elem_tid);
                b.emit(e.span, IrOp::ObjGetAtom { dst: out, obj: v_obj, atom_id });
                if let Some(et) = expect {
                    if et == elem_tid {
                        return Ok((out, elem_tid));
                    }
                    if et == T_DYNAMIC {
                        let vd = b.new_vreg(T_DYNAMIC);
                        b.emit(e.span, IrOp::ToDyn { dst: vd, src: out });
                        return Ok((vd, T_DYNAMIC));
                    }
                    return Err(CompileError::new(ErrorKind::Type, e.span, "tuple element type mismatch"));
                }
                Ok((out, elem_tid))
            } else {
                let out_tid = expect.unwrap_or(T_DYNAMIC);
                let out = b.new_vreg(out_tid);
                b.emit(e.span, IrOp::ObjGetAtom { dst: out, obj: v_obj, atom_id });
                Ok((out, out_tid))
            }
        }
        ExprKind::ArrayLit(elems) => {
            if elems.is_empty() {
                let et = expect.ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "empty array literal requires a type annotation",
                    )
                })?;
                if et != T_ARRAY_I32 && et != T_ARRAY_BYTES {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "empty array literal requires Array<i32> or Array<bytes>",
                    ));
                }
                let v0 = b.new_vreg(T_I32);
                b.emit(e.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                let out = b.new_vreg(et);
                b.emit(e.span, IrOp::ArrayNew { dst: out, len: v0 });
                return Ok((out, et));
            }

            let mut vs: Vec<(VRegId, TypeId)> = Vec::with_capacity(elems.len());
            for el in elems {
                vs.push(lower_expr(el, ctx, b)?);
            }
            let t0 = vs[0].1;
            for &(_, tt) in &vs[1..] {
                if tt != t0 {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "array literal elements must have same type",
                    ));
                }
            }
            let arr_tid = match t0 {
                T_I32 => T_ARRAY_I32,
                T_BYTES => T_ARRAY_BYTES,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "array literal only supports i32/bytes elements for now",
                    ))
                }
            };

            let v_n = b.new_vreg(T_I32);
            b.emit(e.span, IrOp::ConstI32 { dst: v_n, imm: vs.len() as i32 });
            let v_arr = b.new_vreg(arr_tid);
            b.emit(e.span, IrOp::ArrayNew { dst: v_arr, len: v_n });
            for (i, (vp, _)) in vs.iter().enumerate() {
                let v_i = b.new_vreg(T_I32);
                b.emit(e.span, IrOp::ConstI32 { dst: v_i, imm: i as i32 });
                b.emit(e.span, IrOp::ArraySet { arr: v_arr, index: v_i, value: *vp });
            }
            Ok((v_arr, arr_tid))
        }
        ExprKind::TupleLit(elems) => {
            let mut vs: Vec<(VRegId, TypeId)> = Vec::with_capacity(elems.len());
            let mut elem_tids: Vec<TypeId> = Vec::with_capacity(elems.len());
            for el in elems {
                let (v, t) = lower_expr(el, ctx, b)?;
                vs.push((v, t));
                elem_tids.push(t);
            }

            let tup_tid = ctx.type_ctx.intern_tuple_type(&elem_tids);
            let out = b.new_vreg(tup_tid);
            b.emit(e.span, IrOp::ObjNew { dst: out });

            for (i, (v, _t)) in vs.iter().enumerate() {
                let key = i.to_string();
                let atom_id = intern_atom(&key, ctx);
                b.emit(
                    e.span,
                    IrOp::ObjSetAtom {
                        obj: out,
                        atom_id,
                        value: *v,
                    },
                );
            }
            Ok((out, tup_tid))
        }
        ExprKind::ObjLit(fields) => {
            let out_tid = expect.filter(|&et| is_object_kind(&ctx.type_ctx, et)).unwrap_or(T_OBJECT);
            let out = b.new_vreg(out_tid);
            b.emit(e.span, IrOp::ObjNew { dst: out });
            for (k, ve) in fields {
                let (v_val, _t_val) = lower_expr(ve, ctx, b)?;
                let atom_id = intern_atom(k.as_str(), ctx);
                b.emit(
                    e.span,
                    IrOp::ObjSetAtom {
                        obj: out,
                        atom_id,
                        value: v_val,
                    },
                );
            }
            Ok((out, out_tid))
        }
        ExprKind::Index { base, index } => {
            let (vb, tb) = lower_expr(base, ctx, b)?;
            let (vi, ti) = lower_expr(index, ctx, b)?;
            if ti != T_I32 {
                return Err(CompileError::new(ErrorKind::Type, index.span, "index must be i32"));
            }
            match tb {
                T_ARRAY_I32 => {
                    let out = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ArrayGet { dst: out, arr: vb, index: vi });
                    Ok((out, T_I32))
                }
                T_ARRAY_BYTES => {
                    let out = b.new_vreg(T_BYTES);
                    b.emit(e.span, IrOp::ArrayGet { dst: out, arr: vb, index: vi });
                    Ok((out, T_BYTES))
                }
                _ => Err(CompileError::new(ErrorKind::Type, e.span, "indexing not supported for this type yet")),
            }
        }
        ExprKind::Add(_a, _b) => {
            // Special-case bytes concatenation: flatten long chains into concat_many(Array<bytes>)
            // to match the AST backend behavior.
            fn collect_add_terms<'e>(e: &'e Expr, out: &mut Vec<&'e Expr>) {
                match &e.node {
                    ExprKind::Add(x, y) => {
                        collect_add_terms(x, out);
                        collect_add_terms(y, out);
                    }
                    _ => out.push(e),
                }
            }

            let mut terms: Vec<&Expr> = Vec::new();
            collect_add_terms(e, &mut terms);
            if terms.len() == 2 {
                let (va, ta) = lower_expr(terms[0], ctx, b)?;
                let (vb, tb) = lower_expr(terms[1], ctx, b)?;
                let (va, ta, vb, tb) = if ta != tb {
                    match (&terms[0].node, &terms[1].node) {
                        (ExprKind::Member { .. }, _) => {
                            let (va, ta) = lower_expr_expect(terms[0], Some(tb), ctx, b)?;
                            (va, ta, vb, tb)
                        }
                        (_, ExprKind::Member { .. }) => {
                            let (vb, tb) = lower_expr_expect(terms[1], Some(ta), ctx, b)?;
                            (va, ta, vb, tb)
                        }
                        _ => (va, ta, vb, tb),
                    }
                } else {
                    (va, ta, vb, tb)
                };
                if ta != tb {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects operands of same type"));
                }
                return match ta {
                    T_BYTES => {
                        let out = b.new_vreg(T_BYTES);
                        b.emit(e.span, IrOp::BytesConcat2 { dst: out, a: va, b: vb });
                        Ok((out, T_BYTES))
                    }
                    T_I32 => {
                        let out = b.new_vreg(T_I32);
                        b.emit(e.span, IrOp::AddI32 { dst: out, a: va, b: vb });
                        Ok((out, T_I32))
                    }
                    _ => Err(CompileError::new(ErrorKind::Type, e.span, "'+' not supported for this type yet")),
                };
            }

            // Compile terms left-to-right, but allow member gets to be typed by a previously
            // inferred add-chain type (so `o.s + x` can treat `o.s` as bytes/i32).
            let mut compiled_opt: Vec<Option<(VRegId, TypeId)>> = vec![None; terms.len()];
            let mut deferred_member_idxs: Vec<usize> = Vec::new();
            let mut t0: Option<TypeId> = None;
            for (i, t) in terms.iter().enumerate() {
                if matches!(t.node, ExprKind::Member { .. }) {
                    deferred_member_idxs.push(i);
                    continue;
                }
                let (v, tt) = lower_expr(t, ctx, b)?;
                if let Some(t0v) = t0 {
                    if tt != t0v {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "'+' expects operands of same type",
                        ));
                    }
                } else {
                    t0 = Some(tt);
                }
                compiled_opt[i] = Some((v, tt));
            }
            let t0 = t0.or(expect).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "cannot infer '+' operand type (needs a non-member term or type context)",
                )
            })?;
            for i in deferred_member_idxs {
                let (v, tt) = lower_expr_expect(terms[i], Some(t0), ctx, b)?;
                if tt != t0 {
                    return Err(CompileError::new(ErrorKind::Type, terms[i].span, "member type mismatch"));
                }
                compiled_opt[i] = Some((v, tt));
            }
            let compiled: Vec<(VRegId, TypeId)> = compiled_opt
                .into_iter()
                .map(|x| x.expect("compiled term"))
                .collect();

            match t0 {
                T_BYTES => {
                    // Build Array<bytes> and concat_many.
                    let n = compiled.len() as i32;
                    let v_n = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v_n, imm: n });

                    let v_arr = b.new_vreg(T_ARRAY_BYTES);
                    b.emit(e.span, IrOp::ArrayNew { dst: v_arr, len: v_n });

                    for (i, (v_part, _)) in compiled.iter().enumerate() {
                        let v_i = b.new_vreg(T_I32);
                        b.emit(e.span, IrOp::ConstI32 { dst: v_i, imm: i as i32 });
                        b.emit(e.span, IrOp::ArraySet { arr: v_arr, index: v_i, value: *v_part });
                    }

                    let out = b.new_vreg(T_BYTES);
                    b.emit(e.span, IrOp::BytesConcatMany { dst: out, parts: v_arr });
                    Ok((out, T_BYTES))
                }
                T_I32 => {
                    // Fold i32 addition left-to-right (keeps semantics identical).
                    let mut acc = compiled[0].0;
                    for (rhs, _) in &compiled[1..] {
                        let out = b.new_vreg(T_I32);
                        b.emit(e.span, IrOp::AddI32 { dst: out, a: acc, b: *rhs });
                        acc = out;
                    }
                    Ok((acc, T_I32))
                }
                _ => Err(CompileError::new(ErrorKind::Type, e.span, "'+' not supported for this type yet")),
            }
        }
        ExprKind::Sub(a, bb) => {
            let (va, ta) = lower_expr(a, ctx, b)?;
            let (vb, tb) = lower_expr(bb, ctx, b)?;
            if ta != tb {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'-' expects operands of same type"));
            }
            if ta != T_I32 {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'-' currently only supported for i32"));
            }
            let out = b.new_vreg(T_I32);
            b.emit(e.span, IrOp::SubI32 { dst: out, a: va, b: vb });
            Ok((out, T_I32))
        }
        ExprKind::Neg(inner) => {
            let (v, t) = lower_expr(inner, ctx, b)?;
            if t != T_I32 {
                return Err(CompileError::new(ErrorKind::Type, e.span, "unary '-' currently only supported for i32"));
            }
            let out = b.new_vreg(T_I32);
            b.emit(e.span, IrOp::NegI32 { dst: out, src: v });
            Ok((out, T_I32))
        }
        ExprKind::Not(inner) => {
            let (v, t) = lower_expr(inner, ctx, b)?;
            if t != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'!' expects bool"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: v });
            Ok((out, T_BOOL))
        }
        ExprKind::Eq(a, bb) => {
            // If one side is a member access, allow it to take its type from the other side.
            let (va, ta, vb, tb) = match (&a.node, &bb.node) {
                (ExprKind::Member { .. }, _) => {
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                    (va, ta, vb, tb)
                }
                (_, ExprKind::Member { .. }) => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                    (va, ta, vb, tb)
                }
                _ => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            };
            if ta != tb {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'==' expects operands of same type"));
            }

            // Tuple structural equality (element-wise).
            if ctx.type_ctx.is_tuple_type(ta) {
                let out = lower_tuple_eq(e.span, va, vb, ta, ctx, b)?;
                return Ok((out, T_BOOL));
            }

            let out = b.new_vreg(T_BOOL);
            match ta {
                T_BOOL => {
                    b.emit(e.span, IrOp::Physeq { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_I32 => {
                    b.emit(e.span, IrOp::EqI32 { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_BYTES => {
                    emit_bytes_eq(e.span, out, va, vb, ctx, b)?;
                    Ok((out, T_BOOL))
                }
                _ => Err(CompileError::new(ErrorKind::Type, e.span, "'==' not supported for this type yet")),
            }
        }
        ExprKind::Ne(a, bb) => {
            // a != b  ==  !(a == b)
            let (t, tt) = lower_expr(
                &crate::ast::Spanned::new(ExprKind::Eq(a.clone(), bb.clone()), e.span),
                ctx,
                b,
            )?;
            if tt != T_BOOL {
                return Err(CompileError::new(ErrorKind::Internal, e.span, "bad ne lowering"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: t });
            Ok((out, T_BOOL))
        }
        ExprKind::Lt(a, bb) => {
            let (va, ta, vb, tb) = match (&a.node, &bb.node) {
                (ExprKind::Member { .. }, _) => {
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                    (va, ta, vb, tb)
                }
                (_, ExprKind::Member { .. }) => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                    (va, ta, vb, tb)
                }
                _ => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            };
            if ta != T_I32 || tb != T_I32 {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'<' currently only supported for i32"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::LtI32 { dst: out, a: va, b: vb });
            Ok((out, T_BOOL))
        }
        ExprKind::Gt(a, bb) => {
            // a > b  ==  b < a
            lower_expr(
                &crate::ast::Spanned::new(ExprKind::Lt(bb.clone(), a.clone()), e.span),
                ctx,
                b,
            )
        }
        ExprKind::Le(a, bb) => {
            // a <= b  ==  !(b < a)
            let (t, tt) = lower_expr(
                &crate::ast::Spanned::new(ExprKind::Lt(bb.clone(), a.clone()), e.span),
                ctx,
                b,
            )?;
            if tt != T_BOOL {
                return Err(CompileError::new(ErrorKind::Internal, e.span, "bad le lowering"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: t });
            Ok((out, T_BOOL))
        }
        ExprKind::Ge(a, bb) => {
            // a >= b  ==  !(a < b)
            let (t, tt) = lower_expr(
                &crate::ast::Spanned::new(ExprKind::Lt(a.clone(), bb.clone()), e.span),
                ctx,
                b,
            )?;
            if tt != T_BOOL {
                return Err(CompileError::new(ErrorKind::Internal, e.span, "bad ge lowering"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: t });
            Ok((out, T_BOOL))
        }
        ExprKind::And(a, bb) => {
            // Short-circuit:
            //   if(!a) res=a(false) else res=b
            let (va, ta) = lower_expr(a, ctx, b)?;
            if ta != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
            }

            let rhs_b = b.new_block(Some("and_rhs".to_string()));
            let short_b = b.new_block(Some("and_short".to_string()));
            let join_b = b.new_block(Some("and_join".to_string()));

            b.term(IrTerminator::JmpIf {
                cond: va,
                then_tgt: rhs_b,
                else_tgt: short_b,
            });

            b.set_block(short_b);
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(rhs_b);
            let (vb, tb) = lower_expr(bb, ctx, b)?;
            if tb != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
            }
            let rhs_end = b.cur_block();
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(join_b);
            let v_res = b.new_vreg(T_BOOL);
            b.emit(
                e.span,
                IrOp::Phi {
                    dst: v_res,
                    incomings: vec![(short_b, va), (rhs_end, vb)],
                },
            );
            Ok((v_res, T_BOOL))
        }
        ExprKind::Or(a, bb) => {
            // Short-circuit:
            //   if(a) res=a(true) else res=b
            let (va, ta) = lower_expr(a, ctx, b)?;
            if ta != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
            }

            let short_b = b.new_block(Some("or_short".to_string()));
            let rhs_b = b.new_block(Some("or_rhs".to_string()));
            let join_b = b.new_block(Some("or_join".to_string()));

            b.term(IrTerminator::JmpIf {
                cond: va,
                then_tgt: short_b,
                else_tgt: rhs_b,
            });

            b.set_block(short_b);
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(rhs_b);
            let (vb, tb) = lower_expr(bb, ctx, b)?;
            if tb != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
            }
            let rhs_end = b.cur_block();
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(join_b);
            let v_res = b.new_vreg(T_BOOL);
            b.emit(
                e.span,
                IrOp::Phi {
                    dst: v_res,
                    incomings: vec![(short_b, va), (rhs_end, vb)],
                },
            );
            Ok((v_res, T_BOOL))
        }
        ExprKind::TypeApp { .. } => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "unexpected type application (templates must be expanded before lowering)",
        )),
        ExprKind::Call { callee, type_args, args } => {
            let builtin = match &callee.node {
                ExprKind::Var(n) => Some(("".to_string(), n.clone())),
                ExprKind::Member { base, name } => match &base.node {
                    ExprKind::Var(ns) => Some((ns.clone(), name.clone())),
                    _ => None,
                },
                _ => None,
            };

            if let Some((ns, name)) = builtin {
                let ns = ns.as_str();
                let name = name.as_str();

                // Bytes.*
                if ns == "Bytes" && name == "new" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.new expects 1 arg"));
                    }
                    let (vlen, tlen) = lower_expr_expect(&args[0], Some(T_I32), ctx, b)?;
                    if tlen != T_I32 {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "Bytes.new(i32)"));
                    }
                    let out = b.new_vreg(T_BYTES);
                    b.emit(e.span, IrOp::BytesNew { dst: out, len: vlen });
                    return Ok((out, T_BYTES));
                }
                if ns == "Bytes" && name == "len" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.len expects 1 arg"));
                    }
                    let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
                    if tb != T_BYTES {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.len(bytes)"));
                    }
                    let out = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::BytesLen { dst: out, bytes: vb });
                    return Ok((out, T_I32));
                }
                if ns == "Bytes" && name == "get_u8" {
                    if args.len() != 2 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.get_u8 expects 2 args"));
                    }
                    let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
                    let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
                    if tb != T_BYTES || ti != T_I32 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.get_u8(bytes, i32)"));
                    }
                    let out = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::BytesGetU8 { dst: out, bytes: vb, index: vi });
                    return Ok((out, T_I32));
                }
                if ns == "Bytes" && name == "set_u8" {
                    if args.len() != 3 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.set_u8 expects 3 args"));
                    }
                    let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
                    let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
                    let (vv, tv) = lower_expr_expect(&args[2], Some(T_I32), ctx, b)?;
                    if tb != T_BYTES || ti != T_I32 || tv != T_I32 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.set_u8(bytes, i32, i32)"));
                    }
                    b.emit(e.span, IrOp::BytesSetU8 { bytes: vb, index: vi, value: vv });
                    return Ok((vb, T_BYTES));
                }
                if ns == "Bytes" && name == "slice" {
                    if args.len() != 3 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.slice expects 3 args"));
                    }
                    let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
                    let (v_start, t_start) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
                    let (v_len, t_len) = lower_expr_expect(&args[2], Some(T_I32), ctx, b)?;
                    if tb != T_BYTES || t_start != T_I32 || t_len != T_I32 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.slice(bytes, i32, i32)"));
                    }

                    let sig_args = [T_BYTES, T_I32, T_I32];
                    let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
                    let fun_tid = ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args);

                    let vcallee = b.new_vreg(fun_tid);
                    b.emit(
                        e.span,
                        IrOp::ConstFun {
                            dst: vcallee,
                            func_index: crate::jlyb::PRELUDE_BYTES_SLICE,
                        },
                    );

                    // Marshal args into a contiguous vreg window.
                    let arg0 = b.new_vreg(T_BYTES);
                    b.emit(args[0].span, IrOp::Mov { dst: arg0, src: vb });
                    let arg1 = b.new_vreg(T_I32);
                    b.emit(args[1].span, IrOp::Mov { dst: arg1, src: v_start });
                    let arg2 = b.new_vreg(T_I32);
                    b.emit(args[2].span, IrOp::Mov { dst: arg2, src: v_len });

                    let out = b.new_vreg(T_BYTES);
                    b.emit(
                        e.span,
                        IrOp::Call {
                            dst: out,
                            callee: vcallee,
                            sig_id,
                            arg_base: arg0,
                            nargs: 3,
                        },
                    );
                    return Ok((out, T_BYTES));
                }
                if ns == "Bytes" && name == "eq" {
                    if !type_args.is_empty() {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "Bytes.eq does not take type arguments",
                        ));
                    }
                    if args.len() != 2 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.eq expects 2 args"));
                    }
                    let (va, ta) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
                    let (vb, tb) = lower_expr_expect(&args[1], Some(T_BYTES), ctx, b)?;
                    if ta != T_BYTES || tb != T_BYTES {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.eq(bytes, bytes)"));
                    }

                    let sig_args = [T_BYTES, T_BYTES];
                    let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);
                    let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args);

                    let vcallee = b.new_vreg(fun_tid);
                    b.emit(
                        e.span,
                        IrOp::ConstFun {
                            dst: vcallee,
                            func_index: crate::jlyb::PRELUDE_BYTES_EQ,
                        },
                    );

                    // Marshal args into a contiguous vreg window.
                    let arg0 = b.new_vreg(T_BYTES);
                    b.emit(args[0].span, IrOp::Mov { dst: arg0, src: va });
                    let arg1 = b.new_vreg(T_BYTES);
                    b.emit(args[1].span, IrOp::Mov { dst: arg1, src: vb });

                    let out = b.new_vreg(T_BOOL);
                    b.emit(
                        e.span,
                        IrOp::Call {
                            dst: out,
                            callee: vcallee,
                            sig_id,
                            arg_base: arg0,
                            nargs: 2,
                        },
                    );
                    return Ok((out, T_BOOL));
                }

                // Atom.*
                if ns == "Atom" && name == "intern" {
                    if !type_args.is_empty() {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "Atom.intern does not take type arguments",
                        ));
                    }
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Atom.intern expects 1 arg"));
                    }
                    let s = match &args[0].node {
                        ExprKind::BytesLit(b) => b,
                        _ => {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                args[0].span,
                                "Atom.intern expects a bytes literal",
                            ))
                        }
                    };
                    let name = std::str::from_utf8(s).map_err(|_| {
                        CompileError::new(ErrorKind::Type, args[0].span, "Atom.intern expects UTF-8 bytes")
                    })?;
                    let atom_id = intern_atom(name, ctx);
                    let out = b.new_vreg(T_ATOM);
                    b.emit(e.span, IrOp::ConstAtom { dst: out, atom_id });
                    return Ok((out, T_ATOM));
                }

                // Object.*
                if ns == "Object" && name == "get" {
                    if args.len() != 2 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Object.get expects 2 args"));
                    }
                    let out_tid = if type_args.is_empty() {
                        T_DYNAMIC
                    } else {
                        if type_args.len() != 1 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "Object.get<T>(obj, key): expects 1 type arg",
                            ));
                        }
                        ctx.type_ctx.resolve_ty(&type_args[0])?
                    };
                    let (v_obj, t_obj) = lower_expr_expect(&args[0], Some(T_OBJECT), ctx, b)?;
                    if t_obj != T_OBJECT {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "Object.get expects Object"));
                    }
                    let (v_key, t_key) = lower_expr_expect(&args[1], Some(T_ATOM), ctx, b)?;
                    if t_key != T_ATOM {
                        return Err(CompileError::new(ErrorKind::Type, args[1].span, "Object.get expects Atom key"));
                    }
                    let out = b.new_vreg(out_tid);
                    b.emit(e.span, IrOp::ObjGet { dst: out, obj: v_obj, atom: v_key });
                    return Ok((out, out_tid));
                }
                if ns == "Object" && name == "set" {
                    if !type_args.is_empty() {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "Object.set does not take type arguments",
                        ));
                    }
                    if args.len() != 3 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Object.set expects 3 args"));
                    }
                    let (v_obj, t_obj) = lower_expr_expect(&args[0], Some(T_OBJECT), ctx, b)?;
                    if t_obj != T_OBJECT {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "Object.set expects Object"));
                    }
                    let (v_key, t_key) = lower_expr_expect(&args[1], Some(T_ATOM), ctx, b)?;
                    if t_key != T_ATOM {
                        return Err(CompileError::new(ErrorKind::Type, args[1].span, "Object.set expects Atom key"));
                    }
                    let (v_val, _t_val) = lower_expr(&args[2], ctx, b)?;
                    b.emit(e.span, IrOp::ObjSet { obj: v_obj, atom: v_key, value: v_val });
                    // Return the object for convenience.
                    return Ok((v_obj, T_OBJECT));
                }

                // Array.*
                if ns == "Array" && name == "new" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.new expects 1 arg"));
                    }
                    let (vlen, tlen) = lower_expr_expect(&args[0], Some(T_I32), ctx, b)?;
                    if tlen != T_I32 {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "Array.new(i32)"));
                    }
                    let arr_tid = if ns == "Array" {
                        if type_args.is_empty() {
                            match expect {
                                Some(t) if t == T_ARRAY_I32 || t == T_ARRAY_BYTES => t,
                                _ => {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        "Array.new<T>(len): missing type argument",
                                    ))
                                }
                            }
                        } else if type_args.len() == 1 {
                            let elem = ctx.type_ctx.resolve_ty(&type_args[0])?;
                            match elem {
                                T_I32 => T_ARRAY_I32,
                                T_BYTES => T_ARRAY_BYTES,
                                _ => {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        "only Array<I32> and Array<Bytes> supported for now",
                                    ))
                                }
                            }
                        } else {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.new expects 1 type arg"));
                        }
                    } else {
                        T_ARRAY_I32
                    };
                    let out = b.new_vreg(arr_tid);
                    b.emit(e.span, IrOp::ArrayNew { dst: out, len: vlen });
                    return Ok((out, arr_tid));
                }

                if ns == "Array" && name == "len" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.len expects 1 arg"));
                    }
                    let (va, ta) = lower_expr(&args[0], ctx, b)?;
                    if ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "Array.len(Array<T>)"));
                    }
                    let out = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ArrayLen { dst: out, arr: va });
                    return Ok((out, T_I32));
                }
                if ns == "Array" && name == "get" {
                    if args.len() != 2 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get expects 2 args"));
                    }
                    let (va, ta) = lower_expr(&args[0], ctx, b)?;
                    let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
                    if (ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES) || ti != T_I32 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get(Array<T>, i32)"));
                    }
                    let out_tid = if ta == T_ARRAY_I32 { T_I32 } else { T_BYTES };
                    let out = b.new_vreg(out_tid);
                    b.emit(e.span, IrOp::ArrayGet { dst: out, arr: va, index: vi });
                    return Ok((out, out_tid));
                }
                if ns == "Array" && name == "set" {
                    if args.len() != 3 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set expects 3 args"));
                    }
                    let (va, ta) = lower_expr(&args[0], ctx, b)?;
                    let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
                    let want = if ta == T_ARRAY_I32 {
                        T_I32
                    } else if ta == T_ARRAY_BYTES {
                        T_BYTES
                    } else {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set(Array<T>, i32, T)"));
                    };
                    let (vv, tv) = lower_expr_expect(&args[2], Some(want), ctx, b)?;
                    if ti != T_I32 || tv != want {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set(Array<T>, i32, T)"));
                    }
                    b.emit(e.span, IrOp::ArraySet { arr: va, index: vi, value: vv });
                    return Ok((va, ta));
                }

                // List.*
                if ns == "List" && name == "nil" {
                    if !args.is_empty() {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.nil expects 0 args"));
                    }
                    let list_tid = if type_args.is_empty() {
                        match expect {
                            Some(t) if t == T_LIST_I32 || t == T_LIST_BYTES => t,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "List.nil<T>(): missing type argument",
                                ))
                            }
                        }
                    } else if type_args.len() == 1 {
                        let elem = ctx.type_ctx.resolve_ty(&type_args[0])?;
                        match elem {
                            T_I32 => T_LIST_I32,
                            T_BYTES => T_LIST_BYTES,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "only List<I32> and List<Bytes> supported for now",
                                ))
                            }
                        }
                    } else {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.nil expects 1 type arg"));
                    };
                    let out = b.new_vreg(list_tid);
                    b.emit(e.span, IrOp::ListNil { dst: out });
                    return Ok((out, list_tid));
                }

                if ns == "List" && name == "cons" {
                    if args.len() != 2 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons expects 2 args"));
                    }
                    // Determine list type from explicit type arg or expected type.
                    let list_tid = if type_args.is_empty() {
                        match expect {
                            Some(t) if t == T_LIST_I32 || t == T_LIST_BYTES => t,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "List.cons<T>(head, tail): missing type argument",
                                ))
                            }
                        }
                    } else if type_args.len() == 1 {
                        let elem = ctx.type_ctx.resolve_ty(&type_args[0])?;
                        match elem {
                            T_I32 => T_LIST_I32,
                            T_BYTES => T_LIST_BYTES,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "only List<I32> and List<Bytes> supported for now",
                                ))
                            }
                        }
                    } else {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons expects 1 type arg"));
                    };
                    let elem_tid = elem_tid_for_list(list_tid).expect("list tid");
                    let (vhead, thead) = lower_expr_expect(&args[0], Some(elem_tid), ctx, b)?;
                    let (vtail, ttail) = lower_expr_expect(&args[1], Some(list_tid), ctx, b)?;
                    if thead != elem_tid || ttail != list_tid {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons<T>(T, List<T>)"));
                    }
                    let out = b.new_vreg(list_tid);
                    b.emit(e.span, IrOp::ListCons { dst: out, head: vhead, tail: vtail });
                    return Ok((out, list_tid));
                }

                if ns == "List" && name == "head" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.head expects 1 arg"));
                    }
                    let (vl, tl) = lower_expr(&args[0], ctx, b)?;
                    let elem_tid = elem_tid_for_list(tl)
                        .ok_or_else(|| CompileError::new(ErrorKind::Type, args[0].span, "List.head(List<T>)"))?;
                    let out = b.new_vreg(elem_tid);
                    b.emit(e.span, IrOp::ListHead { dst: out, list: vl });
                    return Ok((out, elem_tid));
                }

                if ns == "List" && name == "tail" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.tail expects 1 arg"));
                    }
                    let (vl, tl) = lower_expr(&args[0], ctx, b)?;
                    if tl != T_LIST_I32 && tl != T_LIST_BYTES {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "List.tail(List<T>)"));
                    }
                    let out = b.new_vreg(tl);
                    b.emit(e.span, IrOp::ListTail { dst: out, list: vl });
                    return Ok((out, tl));
                }

                if ns == "List" && name == "is_nil" {
                    if args.len() != 1 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "List.is_nil expects 1 arg"));
                    }
                    let (vl, tl) = lower_expr(&args[0], ctx, b)?;
                    if tl != T_LIST_I32 && tl != T_LIST_BYTES {
                        return Err(CompileError::new(ErrorKind::Type, args[0].span, "List.is_nil(List<T>)"));
                    }
                    let out = b.new_vreg(T_BOOL);
                    b.emit(e.span, IrOp::ListIsNil { dst: out, list: vl });
                    return Ok((out, T_BOOL));
                }
            }

            if !type_args.is_empty() {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "generic type arguments are only supported for builtins (for now)",
                ));
            }

            // General function call.
            // Method call sugar: `obj.m(args...)` binds `this=obj` using `BIND_THIS`.
            if let ExprKind::Member { base, name } = &callee.node {
                // Module namespace call: `Mod.f(args...)` does NOT bind `this`.
                if let ExprKind::Var(alias) = &base.node {
                    if ctx.module_alias_exports.contains_key(alias) {
                        let (v_obj, t_obj) = lower_expr_expect(base, Some(T_OBJECT), ctx, b)?;
                        if t_obj != T_OBJECT {
                            return Err(CompileError::new(ErrorKind::Type, base.span, "module namespace must be Object"));
                        }

                        // If we know this export's declared function type, enforce it; otherwise,
                        // infer a best-effort function type from the call site.
                        let maybe_tid = ctx
                            .module_alias_exports
                            .get(alias)
                            .and_then(|m| m.get(name))
                            .copied();

                        let mut inferred_args: Option<Vec<(VRegId, TypeId)>> = None;
                        let (fun_tid, sig_id, sig_args, sig_ret) = if let Some(exp_tid) = maybe_tid {
                            let te = ctx
                                .type_ctx
                                .types
                                .get(exp_tid as usize)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad export type id"))?;
                            if te.kind != crate::jlyb::TypeKind::Function {
                                return Err(CompileError::new(ErrorKind::Type, e.span, "module member is not callable"));
                            }
                            let sig_id = te.p0;
                            let sig = ctx
                                .type_ctx
                                .sigs
                                .get(sig_id as usize)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?
                                .clone();
                            (exp_tid, sig_id, sig.args, sig.ret_type)
                        } else {
                            // Infer a signature (Dynamic return by default).
                            let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
                            let mut arg_vals: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
                            for a in args.iter() {
                                let (v, t) = lower_expr(a, ctx, b)?;
                                arg_vals.push((v, t));
                                arg_tids.push(t);
                            }
                            inferred_args = Some(arg_vals);
                            let ret_tid = expect.unwrap_or(T_DYNAMIC);
                            let sig_id = ctx.type_ctx.intern_sig(ret_tid, &arg_tids);
                            let fun_tid = ctx.type_ctx.intern_fun_type(ret_tid, &arg_tids);

                            // Reuse the already-lowered args below.
                            // (We return them via a dummy vector in place of sig_args.)
                            // Caller will ignore sig_args for inferred path.
                            (fun_tid, sig_id, arg_tids, ret_tid)
                        };

                        // Evaluate args left-to-right with expected types when available.
                        let arg_vals: Vec<(VRegId, TypeId)> = if let Some(v) = inferred_args.take() {
                            v
                        } else {
                            let mut out: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
                            for (i, a) in args.iter().enumerate() {
                                let et = sig_args.get(i).copied();
                                out.push(lower_expr_expect(a, et, ctx, b)?);
                            }
                            out
                        };

                        let atom_id = intern_atom(name, ctx);
                        let v_f = b.new_vreg(fun_tid);
                        b.emit(e.span, IrOp::ObjGetAtom { dst: v_f, obj: v_obj, atom_id });

                        // Marshal args into a contiguous vreg window.
                        let nargs = arg_vals.len() as u8;
                        let arg_base = if nargs == 0 {
                            VRegId(0)
                        } else {
                            let base = b.new_vreg(sig_args[0]);
                            b.emit(e.span, IrOp::Mov { dst: base, src: arg_vals[0].0 });
                            let mut prev = base;
                            for i in 1..(nargs as usize) {
                                let v = b.new_vreg(sig_args[i]);
                                debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
                                b.emit(e.span, IrOp::Mov { dst: v, src: arg_vals[i].0 });
                                prev = v;
                            }
                            base
                        };

                        let out = b.new_vreg(sig_ret);
                        b.emit(
                            e.span,
                            IrOp::Call {
                                dst: out,
                                callee: v_f,
                                sig_id,
                                arg_base,
                                nargs,
                            },
                        );
                        return Ok((out, sig_ret));
                    }
                }

                let ret_tid = expect.unwrap_or(T_DYNAMIC);
                let (v_obj, t_obj) = lower_expr_expect(base, Some(T_OBJECT), ctx, b)?;
                if t_obj != T_OBJECT {
                    return Err(CompileError::new(ErrorKind::Type, base.span, "method receiver must be Object"));
                }

                // Evaluate args left-to-right.
                let mut arg_vals: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
                for a in args {
                    arg_vals.push(lower_expr(a, ctx, b)?);
                }

                // Unbound method type: (Object, A...) -> R
                let mut unbound_args: Vec<TypeId> = Vec::with_capacity(1 + arg_vals.len());
                unbound_args.push(T_OBJECT);
                unbound_args.extend(arg_vals.iter().map(|(_, t)| *t));
                let unbound_fun_tid = ctx.type_ctx.intern_fun_type(ret_tid, &unbound_args);

                // Bound method type (after BIND_THIS): (A...) -> R
                let bound_args: Vec<TypeId> = arg_vals.iter().map(|(_, t)| *t).collect();
                let bound_sig_id = ctx.type_ctx.intern_sig(ret_tid, &bound_args);
                let bound_fun_tid = ctx.type_ctx.intern_fun_type(ret_tid, &bound_args);

                let atom_id = intern_atom(name, ctx);
                let v_unbound = b.new_vreg(unbound_fun_tid);
                b.emit(
                    e.span,
                    IrOp::ObjGetAtom {
                        dst: v_unbound,
                        obj: v_obj,
                        atom_id,
                    },
                );

                let v_bound = b.new_vreg(bound_fun_tid);
                b.emit(e.span, IrOp::BindThis { dst: v_bound, func: v_unbound, this: v_obj });

                // Build a contiguous vreg window holding the arguments.
                let nargs = arg_vals.len() as u8;
                let arg_base = if nargs == 0 {
                    VRegId(0)
                } else {
                    let base = b.new_vreg(bound_args[0]);
                    b.emit(e.span, IrOp::Mov { dst: base, src: arg_vals[0].0 });
                    let mut prev = base;
                    for i in 1..(nargs as usize) {
                        let v = b.new_vreg(bound_args[i]);
                        debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
                        b.emit(e.span, IrOp::Mov { dst: v, src: arg_vals[i].0 });
                        prev = v;
                    }
                    base
                };

                let out = b.new_vreg(ret_tid);
                b.emit(
                    e.span,
                    IrOp::Call {
                        dst: out,
                        callee: v_bound,
                        sig_id: bound_sig_id,
                        arg_base,
                        nargs,
                    },
                );
                return Ok((out, ret_tid));
            }

            let (vcallee, callee_tid) = lower_expr(callee, ctx, b)?;
            let te = ctx
                .type_ctx
                .types
                .get(callee_tid as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad callee type id"))?;
            if te.kind != crate::jlyb::TypeKind::Function {
                return Err(CompileError::new(ErrorKind::Type, callee.span, "call target is not a function"));
            }
            let sig_id = te.p0;
            let (sig_args, sig_ret) = {
                let sig = ctx
                    .type_ctx
                    .sigs
                    .get(sig_id as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?;
                (sig.args.clone(), sig.ret_type)
            };
            if sig_args.len() != args.len() {
                return Err(CompileError::new(ErrorKind::Type, e.span, "call arity mismatch"));
            }
            // Build a contiguous vreg window holding the arguments.
            let mut arg_vals: Vec<VRegId> = Vec::with_capacity(args.len());
            for (i, a) in args.iter().enumerate() {
                let (va, ta) = lower_expr(a, ctx, b)?;
                if ta != sig_args[i] {
                    return Err(CompileError::new(ErrorKind::Type, a.span, "call argument type mismatch"));
                }
                arg_vals.push(va);
            }

            let nargs = arg_vals.len() as u8;
            let arg_base = if nargs == 0 {
                VRegId(0)
            } else {
                let base = b.new_vreg(sig_args[0]);
                b.emit(e.span, IrOp::Mov { dst: base, src: arg_vals[0] });
                let mut prev = base;
                for i in 1..(nargs as usize) {
                    let v = b.new_vreg(sig_args[i]);
                    debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
                    b.emit(e.span, IrOp::Mov { dst: v, src: arg_vals[i] });
                    prev = v;
                }
                base
            };
            let out_tid = sig_ret;
            let out = b.new_vreg(out_tid);
            b.emit(
                e.span,
                IrOp::Call {
                    dst: out,
                    callee: vcallee,
                    sig_id,
                    arg_base,
                    nargs,
                },
            );
            Ok((out, out_tid))
        }
        ExprKind::Block { stmts, expr } => {
            ctx.env_stack.push(HashMap::new());
            for st in stmts {
                lower_stmt(st, ctx, b)?;
            }
            let out = lower_expr(expr, ctx, b)?;
            ctx.env_stack.pop();
            Ok(out)
        }
        ExprKind::If { cond, then_br, else_br } => {
            let (v_cond, t_cond) = lower_expr(cond, ctx, b)?;
            if t_cond != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, cond.span, "if condition must be bool"));
            }

            let then_b = b.new_block(Some("if_then".to_string()));
            let else_b = b.new_block(Some("if_else".to_string()));
            let join_b = b.new_block(Some("if_join".to_string()));

            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: then_b,
                else_tgt: else_b,
            });

            // then
            b.set_block(then_b);
            let (v_then, t_then) = lower_expr(then_br, ctx, b)?;
            b.term(IrTerminator::Jmp { target: join_b });

            // else
            b.set_block(else_b);
            let (v_else, t_else) = lower_expr(else_br, ctx, b)?;
            b.term(IrTerminator::Jmp { target: join_b });

            if t_then != t_else {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "if branches must return same type",
                ));
            }

            b.set_block(join_b);
            let v_res = b.new_vreg(t_then);
            b.emit(
                e.span,
                IrOp::Phi {
                    dst: v_res,
                    incomings: vec![(then_b, v_then), (else_b, v_else)],
                },
            );
            Ok((v_res, t_then))
        }
        ExprKind::Try { body, catch_name, catch_body } => {
            // Layout to match bytecode structure:
            //   TRY -> catch
            //   <body>
            //   END_TRY
            //   JMP join
            // catch:
            //   <catch_body>
            // join:
            let catch_b = b.new_block(Some("catch".to_string()));
            let join_b = b.new_block(Some("try_join".to_string()));

            let v_exc = b.new_vreg(T_DYNAMIC);
            b.emit(
                e.span,
                IrOp::Try {
                    catch_dst: v_exc,
                    catch_block: catch_b,
                },
            );

            let (v_body, t_body) = lower_expr(body, ctx, b)?;
            b.emit(e.span, IrOp::EndTry);
            let body_end = b.cur_block();
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(catch_b);
            ctx.env_stack.push(HashMap::new());
            if let Some(n) = catch_name {
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(n.clone(), Binding { v: v_exc, tid: T_DYNAMIC });
            }
            let (v_catch, t_catch) = lower_expr(catch_body, ctx, b)?;
            ctx.env_stack.pop();
            if t_catch != t_body {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "try and catch must return same type",
                ));
            }
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(join_b);
            let v_res = b.new_vreg(t_body);
            b.emit(
                e.span,
                IrOp::Phi {
                    dst: v_res,
                    incomings: vec![(body_end, v_body), (catch_b, v_catch)],
                },
            );
            Ok((v_res, t_body))
        }
        ExprKind::Match { subject, arms } => {
            if arms.is_empty() {
                return Err(CompileError::new(ErrorKind::Type, e.span, "match must have at least one arm"));
            }
            // For now, require the last arm to be `_ => { ...expr... }` so the match is exhaustive
            // and produces a value.
            if !matches!(arms.last().unwrap().pat.node, crate::ast::PatternKind::Wildcard) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "non-exhaustive match (last arm must be '_')",
                ));
            }
            if arms.last().unwrap().when.is_some() {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "last '_' match arm cannot have a when-guard",
                ));
            }
            if arms.last().unwrap().tail.is_none() {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "match must produce a value (last arm must have a value)",
                ));
            }

            if !b.is_open() {
                let nb = b.new_block(Some("cont".to_string()));
                b.set_block(nb);
            }

            let (v_subj, t_subj) = lower_expr(subject, ctx, b)?;

            // Special case: matching on a Dynamic subject with scalar-only patterns.
            // This is the first place where `KINDOF` + `SWITCH_KIND` is a clear win.
            if t_subj == T_DYNAMIC {
                let scalar_ok = arms.iter().all(|a| {
                    matches!(
                        a.pat.node,
                        crate::ast::PatternKind::Wildcard
                            | crate::ast::PatternKind::Bind(_)
                            | crate::ast::PatternKind::Pin(_)
                            | crate::ast::PatternKind::BoolLit(_)
                            | crate::ast::PatternKind::I32Lit(_)
                    )
                });
                if scalar_ok {
                    return lower_match_dynamic_scalar(e, v_subj, arms, expect, ctx, b);
                }
            }

            let mut out_tid: Option<TypeId> = expect;
            let mut v_out: Option<VRegId> = out_tid.map(|t| b.new_vreg(t));

            // Pre-create check/bind/guard/body blocks.
            let mut check_bs = Vec::with_capacity(arms.len());
            let mut bind_bs = Vec::with_capacity(arms.len());
            let mut guard_bs: Vec<Option<crate::ir::BlockId>> = Vec::with_capacity(arms.len());
            let mut body_bs = Vec::with_capacity(arms.len());
            for i in 0..arms.len() {
                check_bs.push(b.new_block(Some(format!("match_check{}", i))));
                bind_bs.push(b.new_block(Some(format!("match_bind{}", i))));
                guard_bs.push(if arms[i].when.is_some() {
                    Some(b.new_block(Some(format!("match_when{}", i))))
                } else {
                    None
                });
                body_bs.push(b.new_block(Some(format!("match_body{}", i))));
            }

            // Create join last, so its uses can't precede defs.
            let join_b = b.new_block(Some("match_join".to_string()));

            // Jump into the first check.
            b.term(IrTerminator::Jmp { target: check_bs[0] });

            for i in 0..arms.len() {
                let next_check = if i + 1 < arms.len() { check_bs[i + 1] } else { bind_bs[i] };
                let body_b = body_bs[i];
                let guard_b = guard_bs[i];
                let bind_b = bind_bs[i];

                // --- check
                b.set_block(check_bs[i]);

                match &arms[i].pat.node {
                    crate::ast::PatternKind::Wildcard => {
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::BoolLit(p) => {
                        if t_subj != T_BOOL {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "match bool pattern requires bool subject",
                            ));
                        }
                        let v_pat = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::ConstBool { dst: v_pat, imm: *p });
                        let v_cond = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj, b: v_pat });
                        b.term(IrTerminator::JmpIf {
                            cond: v_cond,
                            then_tgt: bind_b,
                            else_tgt: next_check,
                        });
                    }
                    crate::ast::PatternKind::I32Lit(p) => {
                        if t_subj != T_I32 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "match i32 pattern requires i32 subject",
                            ));
                        }
                        let v_pat = b.new_vreg(T_I32);
                        b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_pat, imm: *p });
                        let v_cond = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj, b: v_pat });
                        b.term(IrTerminator::JmpIf {
                            cond: v_cond,
                            then_tgt: bind_b,
                            else_tgt: next_check,
                        });
                    }
                    crate::ast::PatternKind::Bind(_) => {
                        // Always matches; binding is established in the bind block.
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::Pin(name) => {
                        // Match subject against an existing name.
                        let bd = lookup_var(ctx, name.as_str(), arms[i].pat.span)?;
                        if bd.tid == T_DYNAMIC {
                            let v_subj_dyn = b.new_vreg(T_DYNAMIC);
                            b.emit(arms[i].pat.span, IrOp::ToDyn { dst: v_subj_dyn, src: v_subj });
                            let v_cond = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj_dyn, b: bd.v });
                            b.term(IrTerminator::JmpIf {
                                cond: v_cond,
                                then_tgt: bind_b,
                                else_tgt: next_check,
                            });
                        } else {
                            if bd.tid != t_subj {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    arms[i].pat.span,
                                    "pinned name has incompatible type",
                                ));
                            }
                            let v_cond = b.new_vreg(T_BOOL);
                            match t_subj {
                                T_I32 => b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj, b: bd.v }),
                                _ => b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj, b: bd.v }),
                            }
                            b.term(IrTerminator::JmpIf {
                                cond: v_cond,
                                then_tgt: bind_b,
                                else_tgt: next_check,
                            });
                        }
                    }
                    crate::ast::PatternKind::Obj(fields) => {
                        if t_subj != T_OBJECT {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "object pattern requires Object subject",
                            ));
                        }
                        for (k, vp) in fields {
                            let atom_id = intern_atom(k.as_str(), ctx);
                            let v_has = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::ObjHasAtom { dst: v_has, obj: v_subj, atom_id });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_has,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );

                            match &vp.node {
                                crate::ast::PatternKind::I32Lit(imm) => {
                                    let v_f = b.new_vreg(T_I32);
                                    b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_I32);
                                    b.emit(vp.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::EqI32 { dst: v_eq, a: v_f, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::BoolLit(imm) => {
                                    let v_f = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::ConstBool { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::Physeq { dst: v_eq, a: v_f, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::Pin(name) => {
                                    let bd = lookup_var(ctx, name.as_str(), vp.span)?;
                                    if bd.tid == T_DYNAMIC {
                                        let v_f = b.new_vreg(T_DYNAMIC);
                                        b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(vp.span, IrOp::Physeq { dst: v_eq, a: v_f, b: bd.v });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    } else {
                                        let v_f = b.new_vreg(bd.tid);
                                        b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match bd.tid {
                                            T_I32 => b.emit(vp.span, IrOp::EqI32 { dst: v_eq, a: v_f, b: bd.v }),
                                            _ => b.emit(vp.span, IrOp::Physeq { dst: v_eq, a: v_f, b: bd.v }),
                                        }
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                }
                                crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                                _ => {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        vp.span,
                                        "nested patterns in object fields not supported yet",
                                    ))
                                }
                            }
                        }
                        // All checks passed.
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::TupleExact(elems) => {
                        if !ctx.type_ctx.is_tuple_type(t_subj) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "tuple pattern requires tuple subject",
                            ));
                        }
                        let elem_tids = ctx
                            .type_ctx
                            .tuple_elems(t_subj)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, arms[i].pat.span, "bad tuple type"))?
                            .to_vec();
                        if elem_tids.len() != elems.len() {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "tuple pattern arity mismatch",
                            ));
                        }

                        for (idx, p) in elems.iter().enumerate() {
                            let atom_id = intern_atom(idx.to_string().as_str(), ctx);
                            let elem_tid = elem_tids[idx];
                            match &p.node {
                                crate::ast::PatternKind::I32Lit(imm) => {
                                    if elem_tid != T_I32 {
                                        return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires I32 element"));
                                    }
                                    let v_el = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::BoolLit(imm) => {
                                    if elem_tid != T_BOOL {
                                        return Err(CompileError::new(ErrorKind::Type, p.span, "bool element pattern requires Bool element"));
                                    }
                                    let v_el = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::ConstBool { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::Pin(name) => {
                                    let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                    let v_el = b.new_vreg(elem_tid);
                                    b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    match t_pin {
                                        T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                        _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
                                    }
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                                _ => {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        p.span,
                                        "nested patterns in tuples not supported yet",
                                    ))
                                }
                            }
                        }
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::ArrayExact(elems) => {
                        if let Some(elem_tid) = elem_tid_for_array(t_subj) {
                            // --- Array exact match: len == elems.len()
                            let v_len = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });
                            let v_n = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_n, imm: elems.len() as i32 });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_ok, a: v_len, b: v_n });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );

                            for (idx, p) in elems.iter().enumerate() {
                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires Array<i32>"));
                                        }
                                        let v_i = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Pin(name) => {
                                        let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                        let v_i = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
                                        }
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            p.span,
                                            "nested patterns in arrays not supported yet",
                                        ))
                                    }
                                }
                            }
                            b.term(IrTerminator::Jmp { target: bind_b });
                        } else if let Some(elem_tid) = elem_tid_for_list(t_subj) {
                            // --- List exact match: walk elems and ensure tail is nil at end.
                            let mut v_cur = v_subj;
                            for p in elems {
                                // require non-nil
                                let v_is_nil = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::ListIsNil { dst: v_is_nil, list: v_cur });
                                let v_ok = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::NotBool { dst: v_ok, src: v_is_nil });
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_ok,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );

                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires List<i32>"));
                                        }
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Pin(name) => {
                                        let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
                                        }
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            p.span,
                                            "nested patterns in lists not supported yet",
                                        ))
                                    }
                                }

                                let v_next = b.new_vreg(t_subj);
                                b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                v_cur = v_next;
                            }
                            // ensure nil at end
                            let v_is_nil = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::ListIsNil { dst: v_is_nil, list: v_cur });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_is_nil,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );
                            b.term(IrTerminator::Jmp { target: bind_b });
                        } else {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list exact pattern requires Array<T> or List<T> subject",
                            ));
                        }
                    }
                    crate::ast::PatternKind::ArrayHeadTail { head, .. } => {
                        let elem_tid = elem_tid_for_array(t_subj)
                            .or_else(|| elem_tid_for_list(t_subj))
                            .ok_or_else(|| {
                                CompileError::new(
                                    ErrorKind::Type,
                                    arms[i].pat.span,
                                    "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                                )
                            })?;
                        // len >= 1  (fail if len < 1)
                        if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                            let v_is_nil = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::ListIsNil { dst: v_is_nil, list: v_subj });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::NotBool { dst: v_ok, src: v_is_nil });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );
                        } else {
                            let v_len = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });
                            let v_one = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_one, imm: 1 });
                            let v_too_short = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::LtI32 { dst: v_too_short, a: v_len, b: v_one });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::NotBool { dst: v_ok, src: v_too_short });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );
                        }

                        match &head.node {
                            crate::ast::PatternKind::I32Lit(imm) => {
                                if elem_tid != T_I32 {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        head.span,
                                        "i32 element pattern requires Array<i32> or List<i32>",
                                    ));
                                }
                                let v_el = b.new_vreg(T_I32);
                                if t_subj == T_LIST_I32 {
                                    b.emit(head.span, IrOp::ListHead { dst: v_el, list: v_subj });
                                } else {
                                    let v0 = b.new_vreg(T_I32);
                                    b.emit(head.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                                    b.emit(head.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v0 });
                                }
                                let v_pat = b.new_vreg(T_I32);
                                b.emit(head.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                let v_eq = b.new_vreg(T_BOOL);
                                b.emit(head.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_eq,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );
                            }
                            crate::ast::PatternKind::Pin(name) => {
                                let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, head.span)?;
                                let v_el = b.new_vreg(elem_tid);
                                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                    b.emit(head.span, IrOp::ListHead { dst: v_el, list: v_subj });
                                } else {
                                    let v0 = b.new_vreg(T_I32);
                                    b.emit(head.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                                    b.emit(head.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v0 });
                                }
                                let v_eq = b.new_vreg(T_BOOL);
                                match t_pin {
                                    T_I32 => b.emit(head.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                    _ => b.emit(head.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
                                }
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_eq,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );
                            }
                            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    head.span,
                                    "nested patterns in arrays not supported yet",
                                ))
                            }
                        }

                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::ArrayPrefixRest { prefix, .. } => {
                        if let Some(elem_tid) = elem_tid_for_array(t_subj) {
                            // --- Array prefix/rest: len >= prefix.len()
                            let v_len = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });
                            let v_n = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_n, imm: prefix.len() as i32 });
                            let v_too_short = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::LtI32 { dst: v_too_short, a: v_len, b: v_n });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::NotBool { dst: v_ok, src: v_too_short });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );

                            for (idx, p) in prefix.iter().enumerate() {
                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires Array<i32>"));
                                        }
                                        let v_i = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Pin(name) => {
                                        let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                        let v_i = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
                                        }
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            p.span,
                                            "nested patterns in arrays not supported yet",
                                        ))
                                    }
                                }
                            }
                            b.term(IrTerminator::Jmp { target: bind_b });
                        } else if let Some(elem_tid) = elem_tid_for_list(t_subj) {
                            // --- List prefix/rest: ensure prefix elements exist and match; rest is tail after dropping prefix.len().
                            let mut v_cur = v_subj;
                            for p in prefix {
                                let v_is_nil = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::ListIsNil { dst: v_is_nil, list: v_cur });
                                let v_ok = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::NotBool { dst: v_ok, src: v_is_nil });
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_ok,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );

                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires List<i32>"));
                                        }
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Pin(name) => {
                                        let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
                                        }
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            p.span,
                                            "nested patterns in lists not supported yet",
                                        ))
                                    }
                                }

                                let v_next = b.new_vreg(t_subj);
                                b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                v_cur = v_next;
                            }
                            b.term(IrTerminator::Jmp { target: bind_b });
                        } else {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "prefix/rest pattern requires Array<T> or List<T> subject",
                            ));
                        }
                    }
                }

                // --- bind block (introduce bindings, then jump to when/body)
                b.set_block(bind_b);
                ctx.env_stack.push(HashMap::new());

                // If present, compute `rest` as a fresh array slice starting at `start_idx`.
                let mut need_slice: Option<(usize, String)> = None;

                match &arms[i].pat.node {
                    crate::ast::PatternKind::Bind(name) => {
                        bind_local(ctx, name.as_str(), v_subj, t_subj);
                    }
                    crate::ast::PatternKind::Obj(fields) => {
                        for (k, vp) in fields {
                            if let crate::ast::PatternKind::Bind(name) = &vp.node {
                                let atom_id = intern_atom(k.as_str(), ctx);
                                let v = b.new_vreg(T_DYNAMIC);
                                b.emit(vp.span, IrOp::ObjGetAtom { dst: v, obj: v_subj, atom_id });
                                bind_local(ctx, name.as_str(), v, T_DYNAMIC);
                            }
                        }
                    }
                    crate::ast::PatternKind::TupleExact(elems) => {
                        let elem_tids = ctx.type_ctx.tuple_elems(t_subj).ok_or_else(|| {
                            CompileError::new(ErrorKind::Type, arms[i].pat.span, "tuple pattern requires tuple subject")
                        })?.to_vec();
                        if elem_tids.len() != elems.len() {
                            return Err(CompileError::new(ErrorKind::Type, arms[i].pat.span, "tuple pattern arity mismatch"));
                        }
                        for (idx, p) in elems.iter().enumerate() {
                            if let crate::ast::PatternKind::Bind(name) = &p.node {
                                let idx_s = idx.to_string();
                                let atom_id = intern_atom(idx_s.as_str(), ctx);
                                let tid = elem_tids[idx];
                                let v_el = b.new_vreg(tid);
                                b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                bind_local(ctx, name.as_str(), v_el, tid);
                            }
                        }
                    }
                    crate::ast::PatternKind::ArrayExact(elems) => {
                        let elem_tid = elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                            )
                        })?;
                        for (idx, p) in elems.iter().enumerate() {
                            if let crate::ast::PatternKind::Bind(name) = &p.node {
                                let v_el = b.new_vreg(elem_tid);
                                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                    // Walk idx steps then take head.
                                    let mut v_cur = v_subj;
                                    for _ in 0..idx {
                                        let v_next = b.new_vreg(t_subj);
                                        b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                        v_cur = v_next;
                                    }
                                    b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                } else {
                                    let v_i = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                    b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                }
                                bind_local(ctx, name.as_str(), v_el, elem_tid);
                            }
                        }
                    }
                    crate::ast::PatternKind::ArrayHeadTail { head, rest } => {
                        let elem_tid = elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                            )
                        })?;
                        if let crate::ast::PatternKind::Bind(name) = &head.node {
                            let v_el = b.new_vreg(elem_tid);
                            if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                b.emit(head.span, IrOp::ListHead { dst: v_el, list: v_subj });
                            } else {
                                let v0 = b.new_vreg(T_I32);
                                b.emit(head.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                                b.emit(head.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v0 });
                            }
                            bind_local(ctx, name.as_str(), v_el, elem_tid);
                        }
                        if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                            let v_tail = b.new_vreg(t_subj);
                            b.emit(arms[i].pat.span, IrOp::ListTail { dst: v_tail, list: v_subj });
                            bind_local(ctx, rest.as_str(), v_tail, t_subj);
                        } else {
                            // rest is indexes 1..len
                            need_slice = Some((1, rest.clone()));
                        }
                    }
                    crate::ast::PatternKind::ArrayPrefixRest { prefix, rest } => {
                        let elem_tid = elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                            )
                        })?;
                        for (idx, p) in prefix.iter().enumerate() {
                            if let crate::ast::PatternKind::Bind(name) = &p.node {
                                let v_el = b.new_vreg(elem_tid);
                                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                    let mut v_cur = v_subj;
                                    for _ in 0..idx {
                                        let v_next = b.new_vreg(t_subj);
                                        b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                        v_cur = v_next;
                                    }
                                    b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                } else {
                                    let v_i = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                    b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                }
                                bind_local(ctx, name.as_str(), v_el, elem_tid);
                            }
                        }
                        if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                            let mut v_cur = v_subj;
                            for _ in 0..prefix.len() {
                                let v_next = b.new_vreg(t_subj);
                                b.emit(arms[i].pat.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                v_cur = v_next;
                            }
                            bind_local(ctx, rest.as_str(), v_cur, t_subj);
                        } else {
                            need_slice = Some((prefix.len(), rest.clone()));
                        }
                    }
                    _ => {}
                }

                // Compute rest slice if needed.
                let mut bind_cont_block = bind_b;
                if let Some((start_idx, rest_name)) = need_slice {
                    let elem_tid = elem_tid_for_array(t_subj).ok_or_else(|| {
                        CompileError::new(ErrorKind::Type, arms[i].pat.span, "array pattern requires Array<i32> or Array<bytes> subject")
                    })?;

                    let v_len = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });

                    let v_start = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_start, imm: start_idx as i32 });

                    let v_rest_len = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::SubI32 { dst: v_rest_len, a: v_len, b: v_start });

                    let v_rest = b.new_vreg(t_subj);
                    b.emit(arms[i].pat.span, IrOp::ArrayNew { dst: v_rest, len: v_rest_len });
                    bind_local(ctx, rest_name.as_str(), v_rest, t_subj);

                    // Two counters:
                    // - `dst_i`: index into the new `rest` array (0..rest_len-1)
                    // - `src_i`: index into the source array (start..len-1)
                    let v_zero = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_zero, imm: 0 });
                    let v_one = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_one, imm: 1 });

                    let v_dst_i = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_dst_i, src: v_zero });

                    let v_src_i = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_src_i, src: v_start });

                    let loop_cond = b.new_block(Some(format!("match_slice{}_cond", i)));
                    let loop_body = b.new_block(Some(format!("match_slice{}_body", i)));
                    let loop_exit = b.new_block(Some(format!("match_slice{}_exit", i)));

                    b.term(IrTerminator::Jmp { target: loop_cond });

                    b.set_block(loop_cond);
                    let v_more = b.new_vreg(T_BOOL);
                    b.emit(arms[i].pat.span, IrOp::LtI32 { dst: v_more, a: v_dst_i, b: v_rest_len });
                    b.term(IrTerminator::JmpIf {
                        cond: v_more,
                        then_tgt: loop_body,
                        else_tgt: loop_exit,
                    });

                    b.set_block(loop_body);
                    let v_val = b.new_vreg(elem_tid);
                    b.emit(arms[i].pat.span, IrOp::ArrayGet { dst: v_val, arr: v_subj, index: v_src_i });
                    b.emit(arms[i].pat.span, IrOp::ArraySet { arr: v_rest, index: v_dst_i, value: v_val });

                    let v_src_i2 = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::AddI32 { dst: v_src_i2, a: v_src_i, b: v_one });
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_src_i, src: v_src_i2 });

                    let v_dst_i2 = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::AddI32 { dst: v_dst_i2, a: v_dst_i, b: v_one });
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_dst_i, src: v_dst_i2 });
                    b.term(IrTerminator::Jmp { target: loop_cond });

                    b.set_block(loop_exit);
                    bind_cont_block = loop_exit;
                }

                // --- when guard (optional)
                if let Some(gb) = guard_b {
                    b.set_block(bind_cont_block);
                    b.term(IrTerminator::Jmp { target: gb });

                    b.set_block(gb);
                    let w = arms[i].when.as_ref().expect("guard block implies when");
                    let (v_w, t_w) = lower_expr(w, ctx, b)?;
                    if t_w != T_BOOL {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            w.span,
                            "match when guard must be bool",
                        ));
                    }
                    b.term(IrTerminator::JmpIf {
                        cond: v_w,
                        then_tgt: body_b,
                        else_tgt: next_check,
                    });
                } else {
                    b.set_block(bind_cont_block);
                    b.term(IrTerminator::Jmp { target: body_b });
                }

                // --- body
                b.set_block(body_b);
                for st in &arms[i].body {
                    lower_stmt(st, ctx, b)?;
                }

                // If the arm already terminated control flow (e.g. `return`), don't try to
                // fall through / join.
                if !b.is_open() {
                    ctx.env_stack.pop();
                    continue;
                }

                if let Some(tail) = &arms[i].tail {
                    let (v_tail, t_tail) = match out_tid {
                        Some(t) => lower_expr_expect(tail, Some(t), ctx, b)?,
                        None => {
                            let (v, t) = lower_expr(tail, ctx, b)?;
                            out_tid = Some(t);
                            if v_out.is_none() {
                                v_out = Some(b.new_vreg(t));
                            }
                            (v, t)
                        }
                    };
                    let ot = out_tid.expect("set above");
                    if t_tail != ot {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            tail.span,
                            "match arm value type mismatch",
                        ));
                    }
                    let dst = v_out.expect("match output");
                    b.emit(e.span, IrOp::Mov { dst, src: v_tail });
                    ctx.env_stack.pop();
                    b.term(IrTerminator::Jmp { target: join_b });
                } else {
                    // Fall through to next arm.
                    ctx.env_stack.pop();
                    b.term(IrTerminator::Jmp { target: next_check });
                }
            }

            let ot = out_tid.ok_or_else(|| {
                CompileError::new(ErrorKind::Type, e.span, "match has no value-producing arms")
            })?;
            let dst = v_out.expect("allocated when type known");
            b.set_block(join_b);
            Ok((dst, ot))
        }
        ExprKind::New { proto, args } => {
            // MVP: `new P(args...)` allocates a fresh object, sets `__proto__ = P`,
            // and if `P` has an `init` field, calls `init(self, ...args)` (explicit self).
            let (v_proto, t_proto) = lower_expr(proto, ctx, b)?;
            if !is_object_kind(&ctx.type_ctx, t_proto) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    proto.span,
                    "new expects an Object prototype",
                ));
            }

            // Type of the allocated instance:
            // - default: use the prototype's (possibly nominal) type
            // - allow erasure to plain Object if context expects Object
            // - otherwise, require the expected nominal type to match the prototype's nominal type
            let self_tid = match expect {
                Some(et) if et == T_OBJECT => T_OBJECT,
                Some(et) if is_object_kind(&ctx.type_ctx, et) => {
                    if et != t_proto {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "new: expected object type does not match prototype type",
                        ));
                    }
                    et
                }
                _ => t_proto,
            };
            let v_self = b.new_vreg(self_tid);
            b.emit(e.span, IrOp::ObjNew { dst: v_self });
            b.emit(
                e.span,
                IrOp::ObjSetAtom {
                    obj: v_self,
                    atom_id: crate::jlyb::ATOM___PROTO__,
                    value: v_proto,
                },
            );

            // If proto has init, call it.
            let v_has = b.new_vreg(T_BOOL);
            b.emit(
                e.span,
                IrOp::ObjHasAtom {
                    dst: v_has,
                    obj: v_proto,
                    atom_id: crate::jlyb::ATOM_INIT,
                },
            );

            let call_b = b.new_block(Some("new_init".to_string()));
            let join_b = b.new_block(Some("new_join".to_string()));
            b.term(IrTerminator::JmpIf {
                cond: v_has,
                then_tgt: call_b,
                else_tgt: join_b,
            });

            // call_b
            b.set_block(call_b);

            // Type the init call by the actual argument types.
            //
            // This keeps `new` compatible with a typed VM: if `init` expects `(Object, I32)`,
            // the call passes an `I32` slot (not a boxed `Dynamic`).
            let mut lowered_args: Vec<(VRegId, TypeId, Span)> = Vec::with_capacity(args.len());
            for a in args {
                let (v, t) = lower_expr(a, ctx, b)?;
                lowered_args.push((v, t, a.span));
            }

            let sig_args: Vec<TypeId> = std::iter::once(T_OBJECT)
                .chain(lowered_args.iter().map(|(_v, t, _sp)| *t))
                .collect();
            let sig_id = ctx.type_ctx.intern_sig(T_OBJECT, &sig_args);
            let fun_tid = ctx.type_ctx.intern_fun_type(T_OBJECT, &sig_args);

            let v_init = b.new_vreg(fun_tid);
            b.emit(
                e.span,
                IrOp::ObjGetAtom {
                    dst: v_init,
                    obj: v_proto,
                    atom_id: crate::jlyb::ATOM_INIT,
                },
            );

            // Marshal args into a contiguous vreg window.
            //
            // The call ABI expects args in vregs `[arg_base .. arg_base+nargs)`.
            // We satisfy this by allocating fresh vregs for the window and moving each arg into it.
            let arg_base = b.new_vreg(T_OBJECT);
            b.emit(e.span, IrOp::Mov { dst: arg_base, src: v_self });
            for (v, t, sp) in lowered_args {
                let slot = b.new_vreg(t);
                b.emit(sp, IrOp::Mov { dst: slot, src: v });
            }

            let v_tmp_ret = b.new_vreg(T_OBJECT);
            b.emit(
                e.span,
                IrOp::Call {
                    dst: v_tmp_ret,
                    callee: v_init,
                    sig_id,
                    arg_base,
                    nargs: (1 + sig_args.len() - 1) as u8,
                },
            );
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(join_b);
            Ok((v_self, self_tid))
        }
        ExprKind::Fn { params, body, tail } => {
            let expect_tid = expect.ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "function literal requires a type annotation",
                )
            })?;

            let te = ctx
                .type_ctx
                .types
                .get(expect_tid as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad function type id"))?;
            if te.kind != crate::jlyb::TypeKind::Function {
                return Err(CompileError::new(ErrorKind::Type, e.span, "expected a function type annotation"));
            }
            let (sig_args, sig_ret) = {
                let sig = ctx
                    .type_ctx
                    .sigs
                    .get(te.p0 as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?;
                (sig.args.clone(), sig.ret_type)
            };
            if sig_args.len() != params.len() {
                return Err(CompileError::new(ErrorKind::Type, e.span, "function param arity mismatch"));
            }

            let func_index =
                crate::jlyb::PRELUDE_FUN_COUNT + ctx.user_top_level_fun_count + (ctx.nested_funcs.len() as u32);
            let mut fb = IrBuilder::new(Some(format!("lambda{}", func_index)));
            fb.func.param_count = params.len() as u8;

            let saved_env = std::mem::replace(&mut ctx.env_stack, vec![HashMap::new()]);
            let saved_loops = std::mem::take(&mut ctx.loop_stack);
            let saved_fn = std::mem::take(&mut ctx.fn_stack);

            ctx.loop_stack = Vec::new();
            ctx.fn_stack = vec![FnCtx {
                ret_tid: sig_ret,
                outer_env: Some(saved_env.clone()),
                captures: HashMap::new(),
                capture_order: Vec::new(),
                self_name: ctx.pending_fn_self.as_ref().map(|(n, _)| n.clone()),
                self_func_index: ctx.pending_fn_self.as_ref().map(|_| func_index),
                self_fun_tid: ctx.pending_fn_self.as_ref().map(|(_, tid)| *tid),
                self_loop_head: BlockId(0),
            }];

            // Bind params to vregs 0..n-1
            for (i, (name, _ann)) in params.iter().enumerate() {
                let tid = sig_args[i];
                let v = fb.new_vreg(tid);
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(name.clone(), Binding { v, tid });
            }

            // Recursion sugar for `let f: T = fn(...) { ... f(...) ... }`:
            // inside the function body, bind `f` to a `CONST_FUN` of the function itself.
            if let Some((self_name, self_tid)) = ctx.pending_fn_self.clone() {
                if self_tid == expect_tid {
                    let env = ctx.env_stack.last_mut().expect("env stack");
                    if !env.contains_key(&self_name) {
                        let vself = fb.new_vreg(expect_tid);
                        fb.emit(e.span, IrOp::ConstFun { dst: vself, func_index });
                        env.insert(self_name, Binding { v: vself, tid: expect_tid });
                    }
                }
            }

            // If this function has a self-binding, keep prologue in block0 and lower the body
            // in a fresh loop header block so tail-self-calls can jump there without re-running
            // the prologue (avoids allocating a fresh function object per iteration).
            if ctx.pending_fn_self.is_some() {
                let head = fb.new_block(Some("fn_body".to_string()));
                if fb.is_open() {
                    fb.term(IrTerminator::Jmp { target: head });
                }
                fb.set_block(head);
                if let Some(top) = ctx.fn_stack.last_mut() {
                    top.self_loop_head = head;
                }
            }

            for st in body {
                lower_stmt(st, ctx, &mut fb)?;
            }

            if fb.is_open() {
                if let Some(t) = tail {
                    let (v, tid) = lower_expr(t, ctx, &mut fb)?;
                    if tid != sig_ret {
                        return Err(CompileError::new(ErrorKind::Type, t.span, "function tail type mismatch"));
                    }
                    fb.term(IrTerminator::Ret { value: v });
                } else {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "missing return"));
                }
            }

            // Extract capture info from the fn lowering context.
            let fcx = ctx
                .fn_stack
                .pop()
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing fn ctx"))?;
            let mut cap_slots: Vec<VRegId> = Vec::new();
            let mut cap_srcs: Vec<Binding> = Vec::new();
            for name in &fcx.capture_order {
                let (ob, slot) = *fcx
                    .captures
                    .get(name)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing capture entry"))?;
                cap_slots.push(slot);
                cap_srcs.push(ob);
            }
            fb.func.cap_vregs = cap_slots;

            // Restore outer context
            ctx.env_stack = saved_env;
            ctx.loop_stack = saved_loops;
            ctx.fn_stack = saved_fn;
            ctx.pending_fn_self = None;

            // Emit the function value in the outer function.
            let out = b.new_vreg(expect_tid);
            if cap_srcs.is_empty() {
                b.emit(e.span, IrOp::ConstFun { dst: out, func_index });
            } else {
                // Marshal captures into a contiguous vreg window so `CLOSURE` can box them.
                let cap_sig_args: Vec<TypeId> = cap_srcs.iter().map(|bd| bd.tid).collect();
                let cap_sig_id = ctx.type_ctx.intern_sig(T_DYNAMIC, &cap_sig_args);

                let cap_base = b.new_vreg(cap_srcs[0].tid);
                b.emit(e.span, IrOp::Mov { dst: cap_base, src: cap_srcs[0].v });
                for bd in &cap_srcs[1..] {
                    let slot = b.new_vreg(bd.tid);
                    b.emit(e.span, IrOp::Mov { dst: slot, src: bd.v });
                }
                b.emit(
                    e.span,
                    IrOp::Closure {
                        dst: out,
                        func_index,
                        cap_sig_id,
                        cap_base,
                        ncaps: cap_srcs.len() as u8,
                    },
                );
            }
            ctx.nested_funcs.push(fb.func);
            Ok((out, expect_tid))
        }
        _ => Err(CompileError::new(
            ErrorKind::Codegen,
            e.span,
            "IR lowering: expression not implemented yet",
        )),
    }
}

fn lower_match_dynamic_scalar(
    e: &Expr,
    v_subj: VRegId,
    arms: &[crate::ast::MatchArm],
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Re-check exhaustiveness / value production (same rules as the general match lowering).
    if arms.is_empty() {
        return Err(CompileError::new(ErrorKind::Type, e.span, "match must have at least one arm"));
    }
    if !matches!(arms.last().unwrap().pat.node, crate::ast::PatternKind::Wildcard) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "non-exhaustive match (last arm must be '_')",
        ));
    }
    if arms.last().unwrap().when.is_some() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "last '_' match arm cannot have a when-guard",
        ));
    }
    if arms.last().unwrap().tail.is_none() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "match must produce a value (last arm must have a value)",
        ));
    }

    // Ensure we are in an open block.
    if !b.is_open() {
        let nb = b.new_block(Some("cont".to_string()));
        b.set_block(nb);
    }

    let mut out_tid: Option<TypeId> = expect;
    let mut v_out: Option<VRegId> = out_tid.map(|t| b.new_vreg(t));

    // Shared blocks per arm (bind/guard/body), and per-kind check chains.
    let mut bind_bs = Vec::with_capacity(arms.len());
    let mut guard_bs: Vec<Option<crate::ir::BlockId>> = Vec::with_capacity(arms.len());
    let mut body_bs = Vec::with_capacity(arms.len());
    let mut resume_bs = Vec::with_capacity(arms.len());
    let mut check_bool_bs = Vec::with_capacity(arms.len());
    let mut check_i32_bs = Vec::with_capacity(arms.len());
    let mut check_other_bs = Vec::with_capacity(arms.len());

    for i in 0..arms.len() {
        bind_bs.push(b.new_block(Some(format!("dmatch_bind{}", i))));
        guard_bs.push(if arms[i].when.is_some() {
            Some(b.new_block(Some(format!("dmatch_when{}", i))))
        } else {
            None
        });
        body_bs.push(b.new_block(Some(format!("dmatch_body{}", i))));
        resume_bs.push(b.new_block(Some(format!("dmatch_resume{}", i))));
        check_bool_bs.push(b.new_block(Some(format!("dmatch_check_bool{}", i))));
        check_i32_bs.push(b.new_block(Some(format!("dmatch_check_i32{}", i))));
        check_other_bs.push(b.new_block(Some(format!("dmatch_check_other{}", i))));
    }

    let join_b = b.new_block(Some("dmatch_join".to_string()));

    // Find chain entry points (first arm that could match the kind).
    let mut first_bool = None::<crate::ir::BlockId>;
    let mut first_i32 = None::<crate::ir::BlockId>;
    let mut first_other = None::<crate::ir::BlockId>;
    for i in 0..arms.len() {
        match &arms[i].pat.node {
            crate::ast::PatternKind::BoolLit(_) | crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) | crate::ast::PatternKind::Pin(_) => {
                if first_bool.is_none() {
                    first_bool = Some(check_bool_bs[i]);
                }
            }
            _ => {}
        }
        match &arms[i].pat.node {
            crate::ast::PatternKind::I32Lit(_) | crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) | crate::ast::PatternKind::Pin(_) => {
                if first_i32.is_none() {
                    first_i32 = Some(check_i32_bs[i]);
                }
            }
            _ => {}
        }
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) | crate::ast::PatternKind::Pin(_) => {
                if first_other.is_none() {
                    first_other = Some(check_other_bs[i]);
                }
            }
            _ => {}
        }
    }
    let entry_bool = first_bool.unwrap_or(check_other_bs[0]);
    let entry_i32 = first_i32.unwrap_or(check_other_bs[0]);
    let entry_other = first_other.unwrap_or(check_other_bs[0]);

    // Dispatch by KINDOF using SWITCH_KIND.
    let v_kind = b.new_vreg(T_I32);
    b.emit(e.span, IrOp::Kindof { dst: v_kind, src: v_subj });
    b.term(IrTerminator::SwitchKind {
        kind: v_kind,
        cases: vec![(1, entry_bool), (2, entry_i32)],
        default: entry_other,
    });

    // Resume dispatch blocks: after an arm fails its `when` guard or falls through,
    // jump to the next arm index by re-dispatching on the (already computed) kind code.
    for i in 0..arms.len() {
        b.set_block(resume_bs[i]);
        let j = i + 1;
        if j >= arms.len() {
            // No next arm; should be unreachable under our exhaustiveness rules.
            b.term(IrTerminator::Jmp { target: join_b });
            continue;
        }
        b.term(IrTerminator::SwitchKind {
            kind: v_kind,
            cases: vec![(1, check_bool_bs[j]), (2, check_i32_bs[j])],
            default: check_other_bs[j],
        });
    }

    for i in 0..arms.len() {
        let bind_b = bind_bs[i];
        let body_b = body_bs[i];
        let guard_b = guard_bs[i];
        let resume_b = resume_bs[i];

        // --- check chain helpers
        let next_bool = if i + 1 < arms.len() { check_bool_bs[i + 1] } else { bind_b };
        let next_i32 = if i + 1 < arms.len() { check_i32_bs[i + 1] } else { bind_b };
        let next_other = if i + 1 < arms.len() { check_other_bs[i + 1] } else { bind_b };

        // --- check_bool
        b.set_block(check_bool_bs[i]);
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
                b.term(IrTerminator::Jmp { target: bind_b });
            }
            crate::ast::PatternKind::Pin(name) => {
                emit_pin_check_dynamic_subject(ctx, b, v_subj, arms[i].pat.span, name.as_str(), next_bool, bind_b)?;
            }
            crate::ast::PatternKind::BoolLit(p) => {
                let v_subj_b = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::FromDynBool { dst: v_subj_b, src: v_subj });
                let v_pat = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::ConstBool { dst: v_pat, imm: *p });
                let v_cond = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj_b, b: v_pat });
                b.term(IrTerminator::JmpIf {
                    cond: v_cond,
                    then_tgt: bind_b,
                    else_tgt: next_bool,
                });
            }
            // Not applicable in bool chain.
            _ => b.term(IrTerminator::Jmp { target: next_bool }),
        }

        // --- check_i32
        b.set_block(check_i32_bs[i]);
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
                b.term(IrTerminator::Jmp { target: bind_b });
            }
            crate::ast::PatternKind::Pin(name) => {
                emit_pin_check_dynamic_subject(ctx, b, v_subj, arms[i].pat.span, name.as_str(), next_i32, bind_b)?;
            }
            crate::ast::PatternKind::I32Lit(p) => {
                let v_subj_i = b.new_vreg(T_I32);
                b.emit(arms[i].pat.span, IrOp::FromDynI32 { dst: v_subj_i, src: v_subj });
                let v_pat = b.new_vreg(T_I32);
                b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_pat, imm: *p });
                let v_cond = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj_i, b: v_pat });
                b.term(IrTerminator::JmpIf {
                    cond: v_cond,
                    then_tgt: bind_b,
                    else_tgt: next_i32,
                });
            }
            _ => b.term(IrTerminator::Jmp { target: next_i32 }),
        }

        // --- check_other (only wildcard/bind/pin apply)
        b.set_block(check_other_bs[i]);
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
                b.term(IrTerminator::Jmp { target: bind_b });
            }
            crate::ast::PatternKind::Pin(name) => {
                emit_pin_check_dynamic_subject(ctx, b, v_subj, arms[i].pat.span, name.as_str(), next_other, bind_b)?;
            }
            _ => b.term(IrTerminator::Jmp { target: next_other }),
        }

        // --- bind block
        b.set_block(bind_b);
        ctx.env_stack.push(HashMap::new());
        if let crate::ast::PatternKind::Bind(name) = &arms[i].pat.node {
            bind_local(ctx, name.as_str(), v_subj, T_DYNAMIC);
        }

        // Route through optional when-guard.
        if let Some(gb) = guard_b {
            b.term(IrTerminator::Jmp { target: gb });
            b.set_block(gb);
            let w = arms[i].when.as_ref().expect("guard block implies when");
            let (v_w, t_w) = lower_expr(w, ctx, b)?;
            if t_w != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, w.span, "match when guard must be bool"));
            }
            b.term(IrTerminator::JmpIf {
                cond: v_w,
                then_tgt: body_b,
                else_tgt: resume_b,
            });
        } else {
            b.term(IrTerminator::Jmp { target: body_b });
        }

        // --- body
        b.set_block(body_b);
        for st in &arms[i].body {
            lower_stmt(st, ctx, b)?;
        }
        if !b.is_open() {
            ctx.env_stack.pop();
            continue;
        }

        if let Some(tail) = &arms[i].tail {
            let (v_tail, t_tail) = match out_tid {
                Some(t) => lower_expr_expect(tail, Some(t), ctx, b)?,
                None => {
                    let (v, t) = lower_expr(tail, ctx, b)?;
                    out_tid = Some(t);
                    if v_out.is_none() {
                        v_out = Some(b.new_vreg(t));
                    }
                    (v, t)
                }
            };
            let ot = out_tid.expect("set above");
            if t_tail != ot {
                return Err(CompileError::new(ErrorKind::Type, tail.span, "match arm value type mismatch"));
            }
            let dst = v_out.expect("match output");
            b.emit(e.span, IrOp::Mov { dst, src: v_tail });
            ctx.env_stack.pop();
            b.term(IrTerminator::Jmp { target: join_b });
        } else {
            ctx.env_stack.pop();
            b.term(IrTerminator::Jmp { target: resume_b });
        }
    }

    let ot = out_tid.ok_or_else(|| CompileError::new(ErrorKind::Type, e.span, "match has no value-producing arms"))?;
    let dst = v_out.expect("allocated when type known");
    b.set_block(join_b);
    Ok((dst, ot))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExprKind, Program, Span, Spanned, StmtKind};

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
                ty: Some(Spanned::new(crate::ast::TyKind::Named("I32".to_string()), sp)),
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

        let lowered = lower_module_init_to_ir("entry", &prog, true, &import_exports).unwrap();
        assert!(lowered.ir.funcs.len() >= 1);
    }

    #[test]
    fn lower_minimal_bytes_program() {
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(0, 2)),
        };
        let m = lower_program_to_ir(&prog).unwrap();
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
        let m = lower_program_to_ir(&prog).unwrap();
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
        let m = lower_program_to_ir(&prog).unwrap();
        let f = &m.funcs[0];
        assert!(
            f.blocks
                .iter()
                .any(|b| b.label.as_deref() == Some("while_cond")),
            "missing while_cond block"
        );
        assert!(
            f.blocks
                .iter()
                .any(|b| b.label.as_deref() == Some("while_body")),
            "missing while_body block"
        );
        assert!(
            f.blocks
                .iter()
                .any(|b| b.label.as_deref() == Some("while_exit")),
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
        let m = lower_program_to_ir(&prog).unwrap();
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
        let ty_i32 = Spanned::new(crate::ast::TyKind::Named("I32".to_string()), sp);
        let ty_fun = Spanned::new(
            crate::ast::TyKind::Fun {
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
        lower_program_to_ir(&prog).unwrap();
    }

    #[test]
    fn lower_self_recursive_fn_literal_through_if() {
        let sp = Span::new(0, 1);
        let ty_i32 = Spanned::new(crate::ast::TyKind::Named("I32".to_string()), sp);
        let ty_fun = Spanned::new(
            crate::ast::TyKind::Fun {
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
        lower_program_to_ir(&prog).unwrap();
    }
}

