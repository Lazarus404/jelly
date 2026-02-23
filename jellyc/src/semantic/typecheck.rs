use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, MatchArm, Pattern, PatternKind, Program, Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, NodeId, SemanticInfo};
use crate::ir::TypeId;
use crate::typectx::{TypeCtx, TypeRepr};
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8, T_LIST_BYTES,
    T_LIST_I32, T_OBJECT,
};

use super::fn_infer;
use crate::builtin_constraints;

pub struct TypecheckInputs<'a> {
    pub module_alias_exports: HashMap<String, HashMap<String, TypeId>>,
    pub prelude_env: HashMap<String, TypeId>,
    pub _import_exports_repr: Option<&'a HashMap<String, HashMap<String, TypeRepr>>>,
}

pub fn typecheck_program(p: &Program) -> Result<SemanticInfo, CompileError> {
    let mut tc = TypeChecker::new(TypecheckInputs {
        module_alias_exports: HashMap::new(),
        prelude_env: HashMap::new(),
        _import_exports_repr: None,
    });
    tc.check_program(p)?;
    Ok(tc.finish())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typecheck_minimal_bytes_program() {
        let src = "\"ok\"";
        let p = crate::parse::parse_program(src).unwrap();
        crate::resolve::resolve_program(&p).unwrap();
        let info = typecheck_program(&p).unwrap();
        assert_eq!(info.expr_types.get(&NodeId(p.expr.span)).copied(), Some(T_BYTES));
    }

    #[test]
    fn const_inits_are_recorded_and_folded() {
        let src = "const x = 1 + 2 * 3; \"ok\"";
        let p = crate::parse::parse_program(src).unwrap();
        crate::resolve::resolve_program(&p).unwrap();
        let info = typecheck_program(&p).unwrap();

        let s0 = &p.stmts[0];
        let init = info.const_inits.get(&NodeId(s0.span)).expect("const init");
        match init {
            ConstInit::Value(crate::hir::ConstValue::Int(7)) => {}
            other => panic!("unexpected const init: {other:?}"),
        }
    }

    #[test]
    fn const_bytes_var_init_records_alias() {
        let src = "const a = \"A\"; const b = a; \"ok\"";
        let p = crate::parse::parse_program(src).unwrap();
        crate::resolve::resolve_program(&p).unwrap();
        let info = typecheck_program(&p).unwrap();

        let s1 = &p.stmts[1];
        let init = info.const_inits.get(&NodeId(s1.span)).expect("const init");
        assert_eq!(init, &ConstInit::Alias("a".to_string()));
    }
}


pub fn typecheck_module_init(
    p: &Program,
    import_exports: &HashMap<String, HashMap<String, TypeRepr>>,
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
                    CompileError::new(ErrorKind::Name, s.span, format!("unknown export '{}.{}'", key, name))
                })?;
                prelude_env.insert(bind, tid);
            }
        }
    }

    let mut tc = TypeChecker::new(TypecheckInputs {
        module_alias_exports,
        prelude_env,
        _import_exports_repr: Some(import_exports),
    });
    // Seed the typechecker with the module's type context (so import types are present).
    tc.info.type_ctx = type_ctx;
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
                _ => Err(CompileError::new(ErrorKind::Type, span, "only Array<I32> and Array<Bytes> supported for now")),
            }
        }
        TypeRepr::List(elem) => {
            let e = intern_type_repr(tc, elem, span)?;
            match e {
                T_I32 => Ok(T_LIST_I32),
                T_BYTES => Ok(T_LIST_BYTES),
                _ => Err(CompileError::new(ErrorKind::Type, span, "only List<I32> and List<Bytes> supported for now")),
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

struct TypeChecker {
    env_stack: Vec<HashMap<String, TypeId>>,
    const_stack: Vec<HashMap<String, ConstInit>>,
    module_alias_exports: HashMap<String, HashMap<String, TypeId>>,
    ret_stack: Vec<TypeId>,
    info: SemanticInfo,
}

impl TypeChecker {
    fn new(inputs: TypecheckInputs<'_>) -> Self {
        Self {
            env_stack: vec![inputs.prelude_env],
            const_stack: vec![HashMap::new()],
            module_alias_exports: inputs.module_alias_exports,
            ret_stack: Vec::new(),
            info: SemanticInfo::default(),
        }
    }

    fn finish(self) -> SemanticInfo {
        self.info
    }

    fn bind_local(&mut self, name: &str, tid: TypeId) {
        self.env_stack
            .last_mut()
            .expect("env stack")
            .insert(name.to_string(), tid);
    }

    fn lookup(&self, name: &str) -> Option<TypeId> {
        self.env_stack.iter().rev().find_map(|m| m.get(name)).copied()
    }

    fn lookup_const(&self, name: &str) -> Option<ConstInit> {
        self.const_stack.iter().rev().find_map(|m| m.get(name)).cloned()
    }

    fn bind_const(&mut self, name: &str, init: ConstInit) {
        self.const_stack
            .last_mut()
            .expect("const stack")
            .insert(name.to_string(), init);
    }

    fn push_scope(&mut self) {
        self.env_stack.push(HashMap::new());
        self.const_stack.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.env_stack.pop();
        if self.env_stack.is_empty() {
            self.env_stack.push(HashMap::new());
        }
        self.const_stack.pop();
        if self.const_stack.is_empty() {
            self.const_stack.push(HashMap::new());
        }
    }

    fn check_program(&mut self, p: &Program) -> Result<(), CompileError> {
        // `__global` exists in lowering; treat it as Object for capture/type consistency.
        self.bind_local("__global", T_OBJECT);

        for s in &p.stmts {
            self.check_stmt(s)?;
        }
        let _ = self.check_expr(&p.expr, Some(T_BYTES))?;
        Ok(())
    }

    fn record_expr(&mut self, span: Span, tid: TypeId) {
        self.info.expr_types.insert(NodeId(span), tid);
    }

    fn record_binding(&mut self, span: Span, tid: TypeId) {
        self.info.binding_types.insert(NodeId(span), tid);
    }

    fn check_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
        match &s.node {
            StmtKind::ImportModule { alias, .. } => {
                self.bind_local(alias, T_OBJECT);
                Ok(())
            }
            StmtKind::ImportFrom { items, .. } => {
                for (n, a) in items {
                    let bind = a.as_deref().unwrap_or(n.as_str());
                    // `typecheck_module_init` pre-binds these using module interfaces.
                    if self.lookup(bind).is_none() {
                        self.bind_local(bind, T_DYNAMIC);
                    }
                }
                Ok(())
            }
            StmtKind::Prototype { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "prototype must be expanded before semantic analysis",
            )),
            StmtKind::Let {
                is_const,
                name,
                type_params,
                ty,
                expr,
                ..
            } => {
                if !type_params.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "template lets must be expanded before semantic analysis",
                    ));
                }
                let is_discard = name == "_";
                if is_discard {
                    let _ = self.check_expr(expr, None)?;
                    if *is_const {
                        let mut lookup = |n: &str| self.lookup_const(n);
                        let _ = super::const_eval::eval_const_expr(expr, &self.info, &mut lookup)?;
                    }
                    return Ok(());
                }

                // Determine binding type.
                let bind_tid = if let Some(ann) = ty {
                    self.info.type_ctx.resolve_ty(ann)?
                } else if let ExprKind::Fn { params, body, tail } = &expr.node {
                    let (fun_tid, _args, _ret) = fn_infer::infer_fn_type_for_let(
                        name.as_str(),
                        params,
                        body,
                        tail,
                        &mut self.info.type_ctx,
                        &self.env_stack,
                        &self.module_alias_exports,
                    )?;
                    fun_tid
                } else {
                    // Default to the initializer's type when no annotation.
                    self.check_expr(expr, None)?
                };

                // Recursion sugar: let f = fn(...) { ... f(...) ... };
                if matches!(&expr.node, ExprKind::Fn { .. }) {
                    self.bind_local(name, bind_tid);
                }

                let init_tid = self.check_expr(expr, Some(bind_tid))?;
                if init_tid != bind_tid {
                    return Err(CompileError::new(ErrorKind::Type, s.span, "let initializer type mismatch"));
                }
                if *is_const {
                    let mut lookup = |n: &str| self.lookup_const(n);
                    let init = super::const_eval::eval_const_expr(expr, &self.info, &mut lookup)?;
                    self.info.const_inits.insert(NodeId(s.span), init.clone());
                    self.bind_const(name.as_str(), init);
                }
                self.record_binding(s.span, bind_tid);
                if !matches!(&expr.node, ExprKind::Fn { .. }) {
                    self.bind_local(name, bind_tid);
                }
                Ok(())
            }
            StmtKind::Assign { name, expr } => {
                if self.lookup_const(name.as_str()).is_some() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        format!("cannot assign to const binding '{}'", name),
                    ));
                }
                let dst_tid = self
                    .lookup(name)
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, format!("unknown variable '{}'", name)))?;
                let rhs_tid = self.check_expr(expr, Some(dst_tid))?;
                if rhs_tid != dst_tid {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        format!("assignment to '{}' changes type", name),
                    ));
                }
                Ok(())
            }
            StmtKind::While { cond, body } => {
                // Jelly truthiness: loop conditions need not be `Bool` (everything except `null`
                // and `false` is truthy). Lowering will insert truthiness conversion if needed.
                let _ = self.check_expr(cond, None)?;
                self.push_scope();
                for st in body {
                    self.check_stmt(st)?;
                }
                self.pop_scope();
                Ok(())
            }
            StmtKind::Break | StmtKind::Continue => Ok(()),
            StmtKind::Throw { expr } => {
                let _ = self.check_expr(expr, None)?;
                Ok(())
            }
            StmtKind::Return { expr } => {
                if let Some(e) = expr {
                    if let Some(&ret_tid) = self.ret_stack.last() {
                        let got = self.check_expr(e, Some(ret_tid))?;
                        let coerced = self.coerce_type(got, ret_tid, e.span)?;
                        // Record the coerced type so lowering can be mechanical.
                        self.record_expr(e.span, coerced);
                    } else {
                        let _ = self.check_expr(e, None)?;
                    }
                }
                Ok(())
            }
            StmtKind::Expr { expr } => {
                let _ = self.check_expr(expr, None)?;
                Ok(())
            }
            StmtKind::MemberAssign { base, expr, .. } => {
                let t0 = self.check_expr(base, None)?;
                let t_obj = if t0 == T_DYNAMIC {
                    self.check_expr(base, Some(T_OBJECT))?
                } else {
                    t0
                };
                if !self.is_object_kind(t_obj) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "member assignment expects an Object",
                    ));
                }
                let _ = self.check_expr(expr, None)?;
                Ok(())
            }
            StmtKind::IndexAssign { base, index, expr } => {
                let tb = self.check_expr(base, None)?;
                let _ = self.check_expr(index, Some(T_I32))?;
                match tb {
                    T_ARRAY_I32 => {
                        let _ = self.check_expr(expr, Some(T_I32))?;
                    }
                    T_ARRAY_BYTES => {
                        let _ = self.check_expr(expr, Some(T_BYTES))?;
                    }
                    T_BYTES => {
                        let _ = self.check_expr(expr, Some(T_I32))?;
                    }
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            s.span,
                            "index assignment not supported for this type yet",
                        ));
                    }
                }
                Ok(())
            }
        }
    }

    fn check_expr(&mut self, e: &Expr, expect: Option<TypeId>) -> Result<TypeId, CompileError> {
        // IMPORTANT: `Dynamic` is a boxing boundary, not an inference hint.
        //
        // If a context expects `Dynamic` (e.g. `let x: Any = <expr>`), we want to infer and
        // typecheck `<expr>` using its natural/static types, then coerce the *result* to Dynamic.
        // Propagating `expect = Dynamic` downward forces all subexpressions to become Dynamic,
        // which leads to unnecessary ToDyn/FromDyn churn in lowering/codegen.
        let impl_expect = if expect == Some(T_DYNAMIC) { None } else { expect };
        let tid0 = self.check_expr_impl(e, impl_expect)?;
        let tid = if let Some(et) = expect {
            self.coerce_type(tid0, et, e.span)?
        } else {
            tid0
        };
        // `expect = Dynamic` should not force the expression node itself to become Dynamic.
        // Record the expression's natural/static type, and let lowering insert boxing at the
        // actual boundary use-site (let/assign/call/return/throw, etc).
        let record_tid = if expect == Some(T_DYNAMIC) { tid0 } else { tid };
        self.record_expr(e.span, record_tid);
        Ok(tid)
    }

    fn check_expr_impl(&mut self, e: &Expr, expect: Option<TypeId>) -> Result<TypeId, CompileError> {
        match &e.node {
            ExprKind::BytesLit(_) => Ok(T_BYTES),
            ExprKind::BoolLit(_) => Ok(T_BOOL),
            ExprKind::I32Lit(x) => {
                if expect == Some(T_I8) && *x >= -128 && *x <= 127 {
                    Ok(T_I8)
                } else if expect == Some(T_I16) && *x >= -32768 && *x <= 32767 {
                    Ok(T_I16)
                } else {
                    Ok(T_I32)
                }
            }
            ExprKind::I8Lit(_) => Ok(T_I8),
            ExprKind::I16Lit(_) => Ok(T_I16),
            ExprKind::I64Lit(_) => Ok(T_I64),
            ExprKind::F16Lit(_) => Ok(T_F16),
            ExprKind::F64Lit(_) => {
                if expect == Some(T_F64) {
                    Ok(T_F64)
                } else if expect == Some(T_F16) {
                    Ok(T_F16)
                } else {
                    Ok(T_F32)
                }
            }
            ExprKind::AtomLit(_) => Ok(T_ATOM),
            ExprKind::Null => {
                // Represented as Dynamic unless expected to flow to an object-kind type.
                if let Some(et) = expect {
                    if et == T_BYTES
                        || et == T_ARRAY_I32
                        || et == T_ARRAY_BYTES
                        || et == T_LIST_I32
                        || et == T_LIST_BYTES
                        || self.is_object_kind(et)
                    {
                        return Ok(et);
                    }
                }
                Ok(T_DYNAMIC)
            }
            ExprKind::Var(name) => {
                let bt = self
                    .lookup(name)
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, e.span, format!("unknown variable '{}'", name)))?;
                if let Some(et) = expect {
                    return self.coerce_type(bt, et, e.span);
                }
                Ok(bt)
            }
            ExprKind::Member { base, name } => {
                // Builtin namespaces must be called, not extracted as values.
                // But allow shadowing (e.g. `import foo as Bytes` or `let Bytes = {...}`).
                if let ExprKind::Var(ns) = &base.node {
                    let shadowed = self.lookup(ns).is_some();
                    if !shadowed
                        && matches!(
                            ns.as_str(),
                            "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
                        )
                    {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "namespace members must be called (e.g. Bytes.len(x))",
                        ));
                    }
                }

                // Module namespace object: use declared export type if available.
                if let ExprKind::Var(alias) = &base.node {
                    if let Some(exports) = self.module_alias_exports.get(alias) {
                        let exp_tid = exports.get(name).copied();
                        // Ensure the module alias itself is typed/recorded (lowering requires it).
                        let _ = self.check_expr(base, Some(T_OBJECT))?;
                        let out_tid = exp_tid.or(expect).unwrap_or(T_DYNAMIC);
                        if let Some(et) = expect {
                            return self.coerce_type(out_tid, et, e.span);
                        }
                        return Ok(out_tid);
                    }
                }

                // Regular object / nominal object kinds.
                // Preserve nominal object types, but allow `dynamic` to be treated as `object` here.
                let t_obj0 = self.check_expr(base, None)?;
                let t_obj = if t_obj0 == T_DYNAMIC {
                    self.check_expr(base, Some(T_OBJECT))?
                } else {
                    t_obj0
                };
                if !self.is_object_kind(t_obj) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "member access currently only supported for Object (obj.field)",
                    ));
                }

                // Tuple element access: `t.0`.
                if self.info.type_ctx.is_tuple_type(t_obj) {
                    let idx: usize = name
                        .parse()
                        .map_err(|_| CompileError::new(ErrorKind::Type, e.span, "tuple element access must be .<index>"))?;
                    let elems = self
                        .info
                        .type_ctx
                        .tuple_elems(t_obj)
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad tuple type"))?;
                    if idx >= elems.len() {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "tuple index out of range"));
                    }
                    let elem_tid = elems[idx];
                    if let Some(et) = expect {
                        return self.coerce_type(elem_tid, et, e.span);
                    }
                    return Ok(elem_tid);
                }

                // Object property access yields Dynamic unless context requires typed unboxing.
                if let Some(et) = expect {
                    if et != T_DYNAMIC {
                        return Ok(et);
                    }
                }
                Ok(T_DYNAMIC)
            }
            ExprKind::Call { callee, type_args, args } => {
                // Builtin namespaces can be shadowed by imports/lets (e.g. `import m as Bytes`).
                // If shadowed, treat this as a normal call; don't apply builtin typing rules.
                let shadowed_ns = match &callee.node {
                    ExprKind::Member { base, .. } => match &base.node {
                        ExprKind::Var(ns) => self.lookup(ns).is_some(),
                        _ => false,
                    },
                    _ => false,
                };

                // Builtins.
                if !shadowed_ns {
                if let ExprKind::Member { base, name } = &callee.node {
                    if let ExprKind::Var(ns) = &base.node {
                        if ns == "Array" {
                            if matches!(name.as_str(), "len" | "get" | "set") {
                                if !type_args.is_empty() {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        format!("Array.{} does not take type arguments", name),
                                    ));
                                }
                                // Determine the array type from arg0, then typecheck the rest using centralized rules.
                                let t_arr = args
                                    .get(0)
                                    .ok_or_else(|| CompileError::new(ErrorKind::Type, e.span, "Array.* expects at least 1 arg"))
                                    .and_then(|a0| self.check_expr(a0, None))?;
                                if let Some(cs) = builtin_constraints::array_constraints_from_arr_tid(
                                    name,
                                    args.len(),
                                    t_arr,
                                    e.span,
                                )? {
                                    for (i, a) in args.iter().enumerate() {
                                        let et = match cs.args.get(i).copied().unwrap_or(builtin_constraints::ArgConstraint::Any) {
                                            builtin_constraints::ArgConstraint::Exact(tid) => Some(tid),
                                            _ => None,
                                        };
                                        let _ = self.check_expr(a, et)?;
                                    }
                                    let arg_tids: Vec<TypeId> = cs
                                        .args
                                        .iter()
                                        .map(|c| match c {
                                            builtin_constraints::ArgConstraint::Exact(tid) => *tid,
                                            _ => T_DYNAMIC,
                                        })
                                        .collect();
                                    let fun_tid = self.info.type_ctx.intern_fun_type(cs.ret, &arg_tids);
                                    self.record_expr(callee.span, fun_tid);
                                    return Ok(cs.ret);
                                }
                            }
                        }
                        if ns == "List" && matches!(name.as_str(), "head" | "tail" | "is_nil") {
                            if !type_args.is_empty() {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    format!("List.{} does not take type arguments", name),
                                ));
                            }
                            let t_list = args
                                .get(0)
                                .ok_or_else(|| CompileError::new(ErrorKind::Type, e.span, "List.* expects 1 arg"))
                                .and_then(|a0| self.check_expr(a0, None))?;
                            if let Some(cs) = builtin_constraints::list_constraints_from_list_tid(
                                name,
                                args.len(),
                                t_list,
                                e.span,
                            )? {
                                for (i, a) in args.iter().enumerate() {
                                    let et = match cs
                                        .args
                                        .get(i)
                                        .copied()
                                        .unwrap_or(builtin_constraints::ArgConstraint::Any)
                                    {
                                        builtin_constraints::ArgConstraint::Exact(tid) => Some(tid),
                                        _ => None,
                                    };
                                    let _ = self.check_expr(a, et)?;
                                }
                                let arg_tids: Vec<TypeId> = cs
                                    .args
                                    .iter()
                                    .map(|c| match c {
                                        builtin_constraints::ArgConstraint::Exact(tid) => *tid,
                                        _ => T_DYNAMIC,
                                    })
                                    .collect();
                                let fun_tid = self.info.type_ctx.intern_fun_type(cs.ret, &arg_tids);
                                self.record_expr(callee.span, fun_tid);
                                return Ok(cs.ret);
                            }
                        }
                    }
                }
                }

                // Object.set(obj, key, value) returns the receiver's (possibly nominal) object kind.
                if !shadowed_ns {
                if let ExprKind::Member { base, name } = &callee.node {
                    if let ExprKind::Var(ns) = &base.node {
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

                            let recv_expect = expect.filter(|&et| self.is_object_kind(et) && !self.info.type_ctx.is_tuple_type(et));
                            let t_obj = if let Some(et) = recv_expect {
                                let t = self.check_expr(&args[0], Some(et))?;
                                if t != et {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        args[0].span,
                                        "Object.set receiver type mismatch",
                                    ));
                                }
                                et
                            } else {
                                let t0 = self.check_expr(&args[0], None)?;
                                if t0 == T_DYNAMIC {
                                    self.check_expr(&args[0], Some(T_OBJECT))?
                                } else {
                                    t0
                                }
                            };

                            if !self.is_object_kind(t_obj) {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    args[0].span,
                                    "Object.set expects Object",
                                ));
                            }

                            let _ = self.check_expr(&args[1], Some(T_ATOM))?;
                            let _ = self.check_expr(&args[2], None)?;

                            // Best-effort callee type for dumps.
                            let fun_tid =
                                self.info
                                    .type_ctx
                                    .intern_fun_type(t_obj, &[t_obj, T_ATOM, T_DYNAMIC]);
                            self.record_expr(callee.span, fun_tid);
                            return Ok(t_obj);
                        }
                    }
                }
                }

                if !shadowed_ns {
                    if let Some(cs) = builtin_constraints::builtin_constraints(
                        callee,
                        type_args,
                        args.len(),
                        expect,
                        &mut self.info.type_ctx,
                        true,
                        e.span,
                    )? {
                        for (i, a) in args.iter().enumerate() {
                            match cs
                                .args
                                .get(i)
                                .copied()
                                .unwrap_or(builtin_constraints::ArgConstraint::Any)
                            {
                                builtin_constraints::ArgConstraint::Exact(tid) => {
                                    let _ = self.check_expr(a, Some(tid))?;
                                }
                                builtin_constraints::ArgConstraint::ObjectKind => {
                                    let t0 = self.check_expr(a, None)?;
                                    let t = if t0 == T_DYNAMIC {
                                        self.check_expr(a, Some(T_OBJECT))?
                                    } else {
                                        t0
                                    };
                                    if !self.is_object_kind(t) {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            a.span,
                                            "builtin expects an Object",
                                        ));
                                    }
                                }
                                builtin_constraints::ArgConstraint::Numeric => {
                                    let t = self.check_expr(a, None)?;
                                    if !is_numeric(t) {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            a.span,
                                            "builtin expects numeric",
                                        ));
                                    }
                                }
                                builtin_constraints::ArgConstraint::Any => {
                                    let _ = self.check_expr(a, None)?;
                                }
                            }
                        }
                        // Callee type (for dumps) is a best-effort fun type.
                        let arg_tids: Vec<TypeId> = cs
                            .args
                            .iter()
                            .map(|c| match c {
                                builtin_constraints::ArgConstraint::Exact(tid) => *tid,
                                builtin_constraints::ArgConstraint::ObjectKind => T_OBJECT,
                                _ => T_DYNAMIC,
                            })
                            .collect();
                        let fun_tid = self.info.type_ctx.intern_fun_type(cs.ret, &arg_tids);
                        self.record_expr(callee.span, fun_tid);
                        return Ok(cs.ret);
                    }
                }

                // Method call sugar and module namespace calls are handled by typing the callee span.
                //
                // Method call sugar: `obj.m(args...)` where `obj` is an Object value.
                // We treat `obj.m` as a bound function value `(A...) -> R` for the purpose of typing the call.
                // Lowering is responsible for emitting `ObjGetAtom` + `BindThis` + `Call`.
                if let ExprKind::Member { base, name: _ } = &callee.node {
                    // Don't steal module namespace calls (`Mod.f(...)`), which are typed via exports.
                    if let ExprKind::Var(alias) = &base.node {
                        if self.module_alias_exports.contains_key(alias.as_str()) {
                            // fall through to callee typing via `check_expr(callee, None)`
                        } else {
                            let tb0 = self.check_expr(base, None)?;
                            let tb = if tb0 == T_DYNAMIC {
                                self.check_expr(base, Some(T_OBJECT))?
                            } else {
                                tb0
                            };
                            if !self.is_object_kind(tb) {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    base.span,
                                    "method receiver must be Object",
                                ));
                            }

                            let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
                            for a in args {
                                arg_tids.push(self.check_expr(a, None)?);
                            }

                            // Return type comes from context when available; otherwise Dynamic.
                            let ret_tid = expect.unwrap_or(T_DYNAMIC);
                            let fun_tid = self.info.type_ctx.intern_fun_type(ret_tid, &arg_tids);
                            self.record_expr(callee.span, fun_tid);
                            return Ok(ret_tid);
                        }
                    } else {
                        let tb0 = self.check_expr(base, None)?;
                        let tb = if tb0 == T_DYNAMIC {
                            self.check_expr(base, Some(T_OBJECT))?
                        } else {
                            tb0
                        };
                        if !self.is_object_kind(tb) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                base.span,
                                "method receiver must be Object",
                            ));
                        }
                        let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
                        for a in args {
                            arg_tids.push(self.check_expr(a, None)?);
                        }
                        let ret_tid = expect.unwrap_or(T_DYNAMIC);
                        let fun_tid = self.info.type_ctx.intern_fun_type(ret_tid, &arg_tids);
                        self.record_expr(callee.span, fun_tid);
                        return Ok(ret_tid);
                    }
                }

                // Otherwise, typecheck callee to discover a function type (possibly via module export).
                let callee_tid = self.check_expr(callee, None)?;
                let (sig_args, sig_ret) = self.fun_sig(callee_tid, e.span)?;
                if sig_args.len() != args.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "call arity mismatch"));
                }
                for (i, a) in args.iter().enumerate() {
                    let _ = self.check_expr(a, Some(sig_args[i]))?;
                }
                Ok(sig_ret)
            }
            ExprKind::TypeApp { .. } => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "type application must be expanded before semantic analysis",
            )),
            ExprKind::ArrayLit(elems) => {
                if elems.is_empty() {
                    let Some(et) = expect else {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "empty array literal requires a type annotation",
                        ));
                    };
                    if et != T_ARRAY_I32 && et != T_ARRAY_BYTES {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "empty array literal requires Array<I32> or Array<Bytes>",
                        ));
                    }
                    return Ok(et);
                }
                let t0 = self.check_expr(&elems[0], None)?;
                for el in &elems[1..] {
                    let t = self.check_expr(el, Some(t0))?;
                    if t != t0 {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "array literal elements must have same type"));
                    }
                }
                match t0 {
                    T_I32 => Ok(T_ARRAY_I32),
                    T_BYTES => Ok(T_ARRAY_BYTES),
                    _ => Ok(T_DYNAMIC),
                }
            }
            ExprKind::TupleLit(elems) => {
                let mut ts: Vec<TypeId> = Vec::with_capacity(elems.len());
                for el in elems {
                    ts.push(self.check_expr(el, None)?);
                }
                Ok(self.info.type_ctx.intern_tuple_type(&ts))
            }
            ExprKind::ObjLit(fields) => {
                for (_k, v) in fields {
                    let _ = self.check_expr(v, None)?;
                }
                if let Some(et) = expect {
                    if self.is_object_kind(et) && !self.info.type_ctx.is_tuple_type(et) {
                        return Ok(et);
                    }
                }
                Ok(T_OBJECT)
            }
            ExprKind::Index { base, index } => {
                let tb = self.check_expr(base, None)?;
                let ti = self.check_expr(index, Some(T_I32))?;
                let ti = self.coerce_type(ti, T_I32, index.span)?;
                // Record the coerced type so lowering doesn't need contextual guessing.
                self.record_expr(index.span, ti);
                match tb {
                    T_ARRAY_I32 => Ok(T_I32),
                    T_ARRAY_BYTES => Ok(T_BYTES),
                    T_BYTES => Ok(T_I32),
                    _ => Err(CompileError::new(ErrorKind::Type, e.span, "indexing not supported for this type yet")),
                }
            }
            ExprKind::Fn { params, body, tail } => {
                let fun_tid = if let Some(et) = expect {
                    et
                } else {
                    let arg_tids = vec![T_DYNAMIC; params.len()];
                    self.info.type_ctx.intern_fun_type(T_DYNAMIC, &arg_tids)
                };
                let (sig_args, sig_ret) = self.fun_sig(fun_tid, e.span)?;
                if sig_args.len() != params.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "fn param count does not match expected function type"));
                }

                // Function literals see outer bindings (captures, self-recursion sugar).
                self.push_scope();
                self.ret_stack.push(sig_ret);
                for (i, (pn, ann)) in params.iter().enumerate() {
                    let tid = if let Some(t) = ann {
                        self.info.type_ctx.resolve_ty(t)?
                    } else {
                        sig_args[i]
                    };
                    self.bind_local(pn, tid);
                }
                for st in body {
                    self.check_stmt(st)?;
                }
                if let Some(t) = tail {
                    let got = self.check_expr(t, Some(sig_ret))?;
                    let coerced = self.coerce_type(got, sig_ret, t.span)?;
                    self.record_expr(t.span, coerced);
                }
                self.ret_stack.pop();
                self.pop_scope();
                Ok(fun_tid)
            }
            ExprKind::Truthy(x) => {
                let _ = self.check_expr(x, None)?;
                Ok(T_BOOL)
            }
            ExprKind::Not(x) => {
                let t = self.check_expr(x, Some(T_BOOL))?;
                if t != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'!' expects bool"));
                }
                Ok(T_BOOL)
            }
            ExprKind::Neg(x) => {
                let t = self.check_expr(x, None)?;
                if !is_numeric(t) {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "unary '-' expects numeric"));
                }
                Ok(t)
            }
            ExprKind::Add(a, b) => {
                if let Some(et) = expect {
                    if et == T_BYTES {
                        let ta = self.check_expr(a, Some(T_BYTES))?;
                        let tb = self.check_expr(b, Some(T_BYTES))?;
                        if ta != T_BYTES || tb != T_BYTES {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects bytes operands"));
                        }
                        return Ok(T_BYTES);
                    }
                    if is_numeric(et) {
                        let ta = self.check_expr(a, Some(et))?;
                        let tb = self.check_expr(b, Some(et))?;
                        if !is_numeric(ta) || !is_numeric(tb) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "'+' expects numeric operands",
                            ));
                        }
                        return Ok(et);
                    }
                }

                let ta = self.check_expr(a, None)?;
                // If `ta` is Dynamic, avoid passing `expect=Dynamic` to `b`, since that can
                // "downcast" a more precise type (e.g. Bytes/I32) back to Dynamic and break
                // operator typing/coercions.
                let tb = if ta == T_DYNAMIC {
                    self.check_expr(b, None)?
                } else {
                    self.check_expr(b, Some(ta)).or_else(|_| self.check_expr(b, None))?
                };
                if ta == T_BYTES && tb == T_BYTES {
                    Ok(T_BYTES)
                } else if is_numeric(ta) && is_numeric(tb) {
                    Ok(join_numeric(ta, tb))
                } else if ta == T_DYNAMIC && tb == T_BYTES {
                    let ta2 = self.check_expr(a, Some(T_BYTES))?;
                    if ta2 == T_BYTES {
                        Ok(T_BYTES)
                    } else {
                        Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects bytes operands"))
                    }
                } else if tb == T_DYNAMIC && ta == T_BYTES {
                    let tb2 = self.check_expr(b, Some(T_BYTES))?;
                    if tb2 == T_BYTES {
                        Ok(T_BYTES)
                    } else {
                        Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects bytes operands"))
                    }
                } else if ta == T_DYNAMIC && is_numeric(tb) {
                    let ta2 = self.check_expr(a, Some(tb))?;
                    if is_numeric(ta2) {
                        Ok(join_numeric(ta2, tb))
                    } else {
                        Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects numeric operands"))
                    }
                } else if tb == T_DYNAMIC && is_numeric(ta) {
                    let tb2 = self.check_expr(b, Some(ta))?;
                    if is_numeric(tb2) {
                        Ok(join_numeric(ta, tb2))
                    } else {
                        Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects numeric operands"))
                    }
                } else {
                    Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects bytes or numeric operands"))
                }
            }
            ExprKind::Sub(a, b) | ExprKind::Mul(a, b) => {
                if let Some(et) = expect {
                    if is_numeric(et) {
                        let ta = self.check_expr(a, Some(et))?;
                        let tb = self.check_expr(b, Some(et))?;
                        if !is_numeric(ta) || !is_numeric(tb) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "numeric operator expects numeric operands",
                            ));
                        }
                        return Ok(et);
                    }
                }

                let ta = self.check_expr(a, None)?;
                let tb = if ta == T_DYNAMIC {
                    self.check_expr(b, None)?
                } else {
                    self.check_expr(b, Some(ta)).or_else(|_| self.check_expr(b, None))?
                };
                if is_numeric(ta) && is_numeric(tb) {
                    Ok(join_numeric(ta, tb))
                } else if ta == T_DYNAMIC && is_numeric(tb) {
                    let ta2 = self.check_expr(a, Some(tb))?;
                    if is_numeric(ta2) {
                        Ok(join_numeric(ta2, tb))
                    } else {
                        Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "numeric operator expects numeric operands",
                        ))
                    }
                } else if tb == T_DYNAMIC && is_numeric(ta) {
                    let tb2 = self.check_expr(b, Some(ta))?;
                    if is_numeric(tb2) {
                        Ok(join_numeric(ta, tb2))
                    } else {
                        Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "numeric operator expects numeric operands",
                        ))
                    }
                } else {
                    Err(CompileError::new(ErrorKind::Type, e.span, "numeric operator expects numeric operands"))
                }
            }
            ExprKind::Div(a, b) => {
                // If the surrounding context expects a float, push that expectation into both
                // operands so Dynamic object properties can be typed/unboxed mechanically later.
                if expect == Some(T_F64) {
                    let ta = self.check_expr(a, Some(T_F64))?;
                    let tb = self.check_expr(b, Some(T_F64))?;
                    if !is_numeric(ta) || !is_numeric(tb) {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "'/' expects numeric operands"));
                    }
                    return Ok(T_F64);
                }
                if expect == Some(T_F32) {
                    let ta = self.check_expr(a, Some(T_F32))?;
                    let tb = self.check_expr(b, Some(T_F32))?;
                    if !is_numeric(ta) || !is_numeric(tb) {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "'/' expects numeric operands"));
                    }
                    return Ok(T_F32);
                }

                let ta = self.check_expr(a, None)?;
                let tb = self.check_expr(b, Some(ta)).or_else(|_| self.check_expr(b, None))?;
                if !is_numeric(ta) || !is_numeric(tb) {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'/' expects numeric operands"));
                }
                // Division always produces a float.
                Ok(if ta == T_F64 || tb == T_F64 {
                    T_F64
                } else {
                    T_F32
                })
            }
            ExprKind::Eq(a, b) | ExprKind::Ne(a, b) => {
                let mut ta = self.check_expr(a, None)?;
                // Avoid pushing `expect=Dynamic` into the other operand: that would eagerly coerce
                // concrete types (like I32 literals) into Dynamic, which then prevents the
                // Dynamic→numeric pinning logic below from firing.
                let mut tb = if ta == T_DYNAMIC {
                    self.check_expr(b, None)?
                } else {
                    self.check_expr(b, Some(ta)).or_else(|_| self.check_expr(b, None))?
                };

                // If one side is Dynamic and the other is a concrete type, try to pin the Dynamic side
                // to the concrete type so lowering can be mechanical (typed member unboxing, etc).
                //
                // Keep this conservative: do not try to "coerce" a Bool (Bool comparisons are special-cased below).
                if ta == T_DYNAMIC && tb != T_DYNAMIC && tb != T_BOOL {
                    let ta2 = self.check_expr(a, Some(tb))?;
                    if ta2 == tb {
                        ta = tb;
                    }
                } else if tb == T_DYNAMIC && ta != T_DYNAMIC && ta != T_BOOL {
                    let tb2 = self.check_expr(b, Some(ta))?;
                    if tb2 == ta {
                        tb = ta;
                    }
                }

                // Keep current semantics: allow bool comparisons against any type.
                if ta == T_BOOL || tb == T_BOOL {
                    Ok(T_BOOL)
                } else if is_numeric(ta) && is_numeric(tb) {
                    Ok(T_BOOL)
                } else if ta == tb {
                    Ok(T_BOOL)
                } else {
                    Err(CompileError::new(ErrorKind::Type, e.span, "'==' expects operands of same type"))
                }
            }
            ExprKind::Lt(a, b) | ExprKind::Le(a, b) | ExprKind::Gt(a, b) | ExprKind::Ge(a, b) => {
                let mut ta = self.check_expr(a, None)?;
                // Same rule as equality: don't erase concrete types by pushing expect=Dynamic.
                let mut tb = if ta == T_DYNAMIC {
                    self.check_expr(b, None)?
                } else {
                    self.check_expr(b, Some(ta)).or_else(|_| self.check_expr(b, None))?
                };

                // If one side is Dynamic but the other is numeric, push the numeric expectation into
                // the Dynamic side (eg. object property access) so lowering doesn't need heuristics.
                if ta == T_DYNAMIC && is_numeric(tb) {
                    let ta2 = self.check_expr(a, Some(tb))?;
                    if is_numeric(ta2) {
                        ta = ta2;
                    }
                } else if tb == T_DYNAMIC && is_numeric(ta) {
                    let tb2 = self.check_expr(b, Some(ta))?;
                    if is_numeric(tb2) {
                        tb = tb2;
                    }
                }

                if is_numeric(ta) && is_numeric(tb) {
                    Ok(T_BOOL)
                } else {
                    Err(CompileError::new(ErrorKind::Type, e.span, "relational op expects numeric operands"))
                }
            }
            ExprKind::And(a, b) | ExprKind::Or(a, b) => {
                // Jelly truthiness: operands need not be `Bool` (everything except `null` and
                // `false` is truthy). Lowering will insert truthiness conversion if needed.
                let _ = self.check_expr(a, None)?;
                let _ = self.check_expr(b, None)?;
                Ok(T_BOOL)
            }
            ExprKind::If { cond, then_br, else_br } => {
                let _ = self.check_expr(cond, None)?;
                let tt = self.check_expr(then_br, expect)?;
                let te = self.check_expr(else_br, Some(tt)).or_else(|_| self.check_expr(else_br, expect))?;
                if tt != te {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "if branches must have same type"));
                }
                Ok(tt)
            }
            ExprKind::Block { stmts, expr } => {
                self.push_scope();
                for s in stmts {
                    self.check_stmt(s)?;
                }
                let t = self.check_expr(expr, expect)?;
                self.pop_scope();
                Ok(t)
            }
            ExprKind::Try { body, catch_name, catch_body } => {
                let tb = self.check_expr(body, expect)?;
                self.push_scope();
                if let Some(n) = catch_name {
                    self.bind_local(n, T_DYNAMIC);
                }
                let tc = self.check_expr(catch_body, Some(tb)).or_else(|_| self.check_expr(catch_body, expect))?;
                self.pop_scope();
                if tb != tc {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "try/catch branches must have same type"));
                }
                Ok(tb)
            }
            ExprKind::Match { subject, arms } => {
                let subj_tid = self.check_expr(subject, None)?;
                let mut out_t: Option<TypeId> = None;
                for a in arms {
                    let t = self.check_arm(a, subj_tid, expect)?;
                    // Arms without a tail expression are "fallthrough" arms and do not
                    // contribute a value to the match result.
                    let Some(t) = t else { continue };
                    if let Some(prev) = out_t {
                        if t != prev {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "match arms must have same type"));
                        }
                    } else {
                        out_t = Some(t);
                    }
                }
                Ok(out_t.unwrap_or(expect.unwrap_or(T_DYNAMIC)))
            }
            ExprKind::New { proto, args } => {
                let t_proto0 = self.check_expr(proto, None)?;
                let t_proto = if t_proto0 == T_DYNAMIC {
                    self.check_expr(proto, Some(T_OBJECT))?
                } else {
                    t_proto0
                };
                if !self.is_object_kind(t_proto) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        proto.span,
                        "new expects an Object prototype",
                    ));
                }
                for a in args {
                    let _ = self.check_expr(a, None)?;
                }
                // Type of the allocated instance:
                // - default: use the prototype's (possibly nominal) type
                // - allow erasure to plain Object if context expects Object
                // - otherwise, require the expected nominal type to match the prototype's nominal type
                let self_tid = match expect {
                    Some(et) if et == T_OBJECT => T_OBJECT,
                    Some(et) if self.is_object_kind(et) => {
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
                Ok(self_tid)
            }
        }
    }

    fn check_arm(&mut self, a: &MatchArm, subj_tid: TypeId, expect: Option<TypeId>) -> Result<Option<TypeId>, CompileError> {
        self.push_scope();
        self.bind_pattern_typed(&a.pat, subj_tid);
        if let Some(w) = &a.when {
            let t = self.check_expr(w, Some(T_BOOL))?;
            if t != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, w.span, "match when must be bool"));
            }
        }
        for st in &a.body {
            self.check_stmt(st)?;
        }
        let out_t = if let Some(t) = &a.tail {
            Some(self.check_expr(t, expect)?)
        } else {
            None
        };
        self.pop_scope();
        Ok(out_t)
    }

    fn bind_pattern_typed(&mut self, p: &Pattern, subj_tid: TypeId) {
        match &p.node {
            PatternKind::Bind(n) => self.bind_local(n, subj_tid),
            PatternKind::Obj(fields) => {
                for (_k, v) in fields {
                    self.bind_pattern_typed(v, T_DYNAMIC);
                }
            }
            PatternKind::TupleExact(elems) => {
                let elem_tids: Vec<TypeId> = self
                    .info
                    .type_ctx
                    .tuple_elems(subj_tid)
                    .map(|ts| ts.to_vec())
                    .unwrap_or_default();
                for (i, el) in elems.iter().enumerate() {
                    let tid = elem_tids.get(i).copied().unwrap_or(T_DYNAMIC);
                    self.bind_pattern_typed(el, tid);
                }
            }
            PatternKind::ArrayExact(elems) => {
                let elem_tid = match subj_tid {
                    T_ARRAY_I32 | T_LIST_I32 => Some(T_I32),
                    T_ARRAY_BYTES | T_LIST_BYTES => Some(T_BYTES),
                    _ => None,
                };
                for el in elems {
                    self.bind_pattern_typed(el, elem_tid.unwrap_or(T_DYNAMIC));
                }
            }
            PatternKind::ArrayHeadTail { head, rest } => {
                let elem_tid = match subj_tid {
                    T_ARRAY_I32 | T_LIST_I32 => Some(T_I32),
                    T_ARRAY_BYTES | T_LIST_BYTES => Some(T_BYTES),
                    _ => None,
                };
                self.bind_pattern_typed(head, elem_tid.unwrap_or(T_DYNAMIC));
                self.bind_local(rest, subj_tid);
            }
            PatternKind::ArrayPrefixRest { prefix, rest } => {
                let elem_tid = match subj_tid {
                    T_ARRAY_I32 | T_LIST_I32 => Some(T_I32),
                    T_ARRAY_BYTES | T_LIST_BYTES => Some(T_BYTES),
                    _ => None,
                };
                for el in prefix {
                    self.bind_pattern_typed(el, elem_tid.unwrap_or(T_DYNAMIC));
                }
                self.bind_local(rest, subj_tid);
            }
            PatternKind::Wildcard
            | PatternKind::BoolLit(_)
            | PatternKind::I8Lit(_)
            | PatternKind::I16Lit(_)
            | PatternKind::I32Lit(_)
            | PatternKind::Pin(_) => {}
        }
    }

    fn fun_sig(&self, fun_tid: TypeId, span: Span) -> Result<(Vec<TypeId>, TypeId), CompileError> {
        let te = self
            .info
            .type_ctx
            .types
            .get(fun_tid as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad function type id"))?;
        if te.kind != crate::jlyb::TypeKind::Function {
            return Err(CompileError::new(ErrorKind::Type, span, "call target is not a function"));
        }
        let sig = self
            .info
            .type_ctx
            .sigs
            .get(te.p0 as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad fun sig id"))?;
        Ok((sig.args.clone(), sig.ret_type))
    }

    fn is_object_kind(&self, tid: TypeId) -> bool {
        self.info
            .type_ctx
            .types
            .get(tid as usize)
            .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
    }

    fn coerce_type(&self, from: TypeId, to: TypeId, span: Span) -> Result<TypeId, CompileError> {
        if from == to {
            return Ok(to);
        }
        if to == T_DYNAMIC {
            return Ok(T_DYNAMIC);
        }
        if from == T_DYNAMIC {
            return Ok(to);
        }
        if is_numeric(from) && is_numeric(to) {
            return Ok(to);
        }
        Err(CompileError::new(
            ErrorKind::Type,
            span,
            "type mismatch (no implicit conversion)",
        ))
    }
}

fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

fn numeric_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        T_F16 => 4,
        T_F32 => 5,
        T_F64 => 6,
        _ => 255,
    }
}

fn join_numeric(a: TypeId, b: TypeId) -> TypeId {
    if numeric_rank(a) >= numeric_rank(b) {
        a
    } else {
        b
    }
}

