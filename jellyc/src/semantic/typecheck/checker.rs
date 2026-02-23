use std::collections::HashMap;

use crate::ast::{Expr, Program, Span, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, NodeId, SemanticInfo};
use crate::ir::TypeId;
use crate::typectx::TypeCtx;
use crate::typectx::{T_DYNAMIC, T_OBJECT};
use crate::visit::Visitor;

use super::{dispatch, is_numeric, stmt};

pub(super) struct TypecheckInputs {
    pub(super) module_alias_exports: HashMap<String, HashMap<String, TypeId>>,
    pub(super) prelude_env: HashMap<String, TypeId>,
    /// Expected type for the program's final expression. T_BYTES for .jelly files, T_DYNAMIC for REPL.
    pub(super) expected_program_expr_type: TypeId,
}

pub(super) struct TypeChecker {
    env_stack: Vec<HashMap<String, TypeId>>,
    const_stack: Vec<HashMap<String, ConstInit>>,
    module_alias_exports: HashMap<String, HashMap<String, TypeId>>,
    ret_stack: Vec<TypeId>,
    info: SemanticInfo,
    expected_program_expr_type: TypeId,
}

impl TypeChecker {
    pub(super) fn new(inputs: TypecheckInputs) -> Self {
        Self {
            env_stack: vec![inputs.prelude_env],
            const_stack: vec![HashMap::new()],
            module_alias_exports: inputs.module_alias_exports,
            ret_stack: Vec::new(),
            info: SemanticInfo::default(),
            expected_program_expr_type: inputs.expected_program_expr_type,
        }
    }

    pub(super) fn finish(self) -> SemanticInfo {
        self.info
    }

    pub(super) fn seed_type_ctx(&mut self, type_ctx: TypeCtx) {
        self.info.type_ctx = type_ctx;
    }

    pub(super) fn record_const_init(&mut self, span: Span, init: ConstInit) {
        self.info.const_inits.insert(NodeId(span), init);
    }

    pub(super) fn eval_const_expr(&self, expr: &Expr) -> Result<ConstInit, CompileError> {
        let mut lookup = |n: &str| self.lookup_const(n);
        super::super::const_eval::eval_const_expr(expr, &self.info, &mut lookup)
    }

    pub(super) fn bind_local(&mut self, name: &str, tid: TypeId) {
        self.env_stack
            .last_mut()
            .expect("env stack")
            .insert(name.to_string(), tid);
    }

    pub(super) fn lookup(&self, name: &str) -> Option<TypeId> {
        self.env_stack
            .iter()
            .rev()
            .find_map(|m| m.get(name))
            .copied()
    }

    pub(super) fn lookup_const(&self, name: &str) -> Option<ConstInit> {
        self.const_stack
            .iter()
            .rev()
            .find_map(|m| m.get(name))
            .cloned()
    }

    pub(super) fn bind_const(&mut self, name: &str, init: ConstInit) {
        self.const_stack
            .last_mut()
            .expect("const stack")
            .insert(name.to_string(), init);
    }

    pub(super) fn is_module_alias(&self, alias: &str) -> bool {
        self.module_alias_exports.contains_key(alias)
    }

    pub(super) fn module_export_tid(&self, alias: &str, name: &str) -> Option<TypeId> {
        self.module_alias_exports
            .get(alias)
            .and_then(|exports| exports.get(name).copied())
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

    pub(super) fn with_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        self.push_scope();
        let out = f(self);
        self.pop_scope();
        out
    }

    pub(super) fn check_program(&mut self, p: &Program) -> Result<(), CompileError> {
        self.visit_program(p)
    }

    fn check_program_impl(&mut self, p: &Program) -> Result<(), CompileError> {
        // `__global` exists in lowering; treat it as Object for capture/type consistency.
        self.bind_local("__global", T_OBJECT);

        for s in &p.stmts {
            self.check_stmt(s)?;
        }
        let _ = self.check_expr(&p.expr, Some(self.expected_program_expr_type))?;
        Ok(())
    }

    pub(super) fn record_expr(&mut self, span: Span, tid: TypeId) {
        self.info.expr_types.insert(NodeId(span), tid);
    }

    pub(super) fn record_binding(&mut self, span: Span, tid: TypeId) {
        self.info.binding_types.insert(NodeId(span), tid);
    }

    pub(super) fn check_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
        self.visit_stmt(s)
    }

    fn check_stmt_impl(&mut self, s: &Stmt) -> Result<(), CompileError> {
        stmt::check_stmt(self, s)
    }

    pub(super) fn check_expr(
        &mut self,
        e: &Expr,
        expect: Option<TypeId>,
    ) -> Result<TypeId, CompileError> {
        // IMPORTANT: `Dynamic` is a boxing boundary, not an inference hint.
        let impl_expect = if expect == Some(T_DYNAMIC) {
            None
        } else {
            expect
        };
        let tid0 = self.check_expr_impl(e, impl_expect)?;
        let tid = if let Some(et) = expect {
            self.coerce_type(tid0, et, e.span)?
        } else {
            tid0
        };
        let record_tid = if expect == Some(T_DYNAMIC) { tid0 } else { tid };
        self.record_expr(e.span, record_tid);
        Ok(tid)
    }

    fn check_expr_impl(
        &mut self,
        e: &Expr,
        expect: Option<TypeId>,
    ) -> Result<TypeId, CompileError> {
        dispatch::check_expr_impl(self, e, expect)
    }

    pub(super) fn resolve_ann_tid(&mut self, ann: &Ty) -> Result<TypeId, CompileError> {
        self.info.type_ctx.resolve_ty(ann)
    }

    pub(super) fn infer_fn_tid_for_let(
        &mut self,
        self_name: &str,
        params: &[(String, Option<Ty>)],
        body: &[Stmt],
        tail: &Option<Box<Expr>>,
    ) -> Result<TypeId, CompileError> {
        let env_stack = &self.env_stack;
        let module_alias_exports = &self.module_alias_exports;
        let lookup_outer = |name: &str| env_stack.iter().rev().find_map(|m| m.get(name)).copied();
        let lookup_module_export = |alias: &str, name: &str| {
            module_alias_exports
                .get(alias)
                .and_then(|exports| exports.get(name).copied())
        };
        let (fun_tid, _args, _ret) = super::super::fn_infer::infer_fn_type_for_let(
            self_name,
            params,
            body,
            tail,
            &mut self.info.type_ctx,
            &lookup_outer,
            &lookup_module_export,
        )?;
        Ok(fun_tid)
    }

    pub(super) fn type_ctx_mut(&mut self) -> &mut TypeCtx {
        &mut self.info.type_ctx
    }

    pub(super) fn intern_fun_type(&mut self, ret: TypeId, args: &[TypeId]) -> TypeId {
        self.info.type_ctx.intern_fun_type(ret, args)
    }

    pub(super) fn intern_tuple_type(&mut self, elems: &[TypeId]) -> TypeId {
        self.info.type_ctx.intern_tuple_type(elems)
    }

    pub(super) fn is_tuple_type(&self, tid: TypeId) -> bool {
        self.info.type_ctx.is_tuple_type(tid)
    }

    pub(super) fn tuple_elems(&self, tid: TypeId, span: Span) -> Result<&[TypeId], CompileError> {
        self.info
            .type_ctx
            .tuple_elems(tid)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad tuple type"))
    }

    pub(super) fn tuple_elems_or_empty(&self, tid: TypeId) -> Vec<TypeId> {
        self.info
            .type_ctx
            .tuple_elems(tid)
            .map(|ts| ts.to_vec())
            .unwrap_or_default()
    }

    pub(super) fn push_ret_tid(&mut self, tid: TypeId) {
        self.ret_stack.push(tid);
    }

    pub(super) fn pop_ret_tid(&mut self) -> Option<TypeId> {
        self.ret_stack.pop()
    }

    pub(super) fn current_ret_tid(&self) -> Option<TypeId> {
        self.ret_stack.last().copied()
    }

    pub(super) fn fun_sig(
        &self,
        fun_tid: TypeId,
        span: Span,
    ) -> Result<(Vec<TypeId>, TypeId), CompileError> {
        let te = self
            .info
            .type_ctx
            .types
            .get(fun_tid as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad function type id"))?;
        if te.kind != crate::jlyb::TypeKind::Function {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "call target is not a function",
            ));
        }
        let sig = self
            .info
            .type_ctx
            .sigs
            .get(te.p0 as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad fun sig id"))?;
        Ok((sig.args.clone(), sig.ret_type))
    }

    pub(super) fn is_object_kind(&self, tid: TypeId) -> bool {
        self.info
            .type_ctx
            .types
            .get(tid as usize)
            .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
    }

    pub(super) fn coerce_type(
        &self,
        from: TypeId,
        to: TypeId,
        span: Span,
    ) -> Result<TypeId, CompileError> {
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

impl Visitor for TypeChecker {
    type Err = CompileError;

    fn visit_program(&mut self, p: &Program) -> Result<(), Self::Err> {
        self.check_program_impl(p)
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), Self::Err> {
        self.check_stmt_impl(s)
    }

    fn visit_expr(&mut self, e: &Expr) -> Result<(), Self::Err> {
        let _ = self.check_expr(e, None)?;
        Ok(())
    }
}
