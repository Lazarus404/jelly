use std::collections::HashMap;

use crate::ast::{Expr, ExprKind};
use crate::builtin_constraints::{
    array_constraints_from_arr_tid, builtin_constraints, list_constraints_from_list_tid,
    ArgConstraint,
};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F64, T_I16, T_I32, T_I64, T_I8};

use super::super::dsu::{infer_add_bin, infer_numeric_bin, is_numeric, unify, ITy};
use super::Infer;

impl<'a> Infer<'a> {
    pub(super) fn infer_expr(&mut self, e: &Expr) -> Result<ITy, CompileError> {
        match &e.node {
            ExprKind::I32Lit(_) => Ok(ITy::Known(T_I32)),
            ExprKind::I8Lit(_) => Ok(ITy::Known(T_I8)),
            ExprKind::I16Lit(_) => Ok(ITy::Known(T_I16)),
            ExprKind::I64Lit(_) => Ok(ITy::Known(T_I64)),
            ExprKind::F16Lit(_) => Ok(ITy::Known(T_F16)),
            ExprKind::F64Lit(_) => Ok(ITy::Known(T_F64)),
            ExprKind::BoolLit(_) => Ok(ITy::Known(T_BOOL)),
            ExprKind::BytesLit(_) => Ok(ITy::Known(T_BYTES)),
            ExprKind::AtomLit(_) => Ok(ITy::Known(T_ATOM)),
            ExprKind::Null => Ok(ITy::Known(T_DYNAMIC)),
            ExprKind::Var(n) => Ok(self.lookup(n).unwrap_or(ITy::Known(T_DYNAMIC))),
            ExprKind::Not(x) | ExprKind::Truthy(x) => {
                let _ = self.infer_expr(x)?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::Neg(x) => {
                let t = self.infer_expr(x)?;
                if let Some(tid) = self.resolve_known_tid(t) {
                    if !is_numeric(tid) {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            crate::ast::Span::point(0),
                            "numeric operator expects numeric operands",
                        ));
                    }
                    return Ok(ITy::Known(tid));
                }
                if let ITy::Var(v) = t {
                    self.dsu.mark_numeric(v);
                }
                Ok(t)
            }
            ExprKind::Add(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                infer_add_bin(&mut self.dsu, ta, tb)
            }
            ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                infer_numeric_bin(&mut self.dsu, ta, tb)
            }
            ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                let _ = unify(&mut self.dsu, ta, tb)?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::And(a, b) | ExprKind::Or(a, b) => {
                let _ = self.infer_expr(a)?;
                let _ = self.infer_expr(b)?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => {
                let _ = self.infer_expr(cond)?;
                let ta = self.infer_expr(then_br)?;
                let tb = self.infer_expr(else_br)?;
                unify(&mut self.dsu, ta, tb)
            }
            ExprKind::Block { stmts, expr } => {
                self.scopes.push(HashMap::new());
                for s in stmts {
                    self.infer_stmt(s)?;
                }
                let t = self.infer_expr(expr)?;
                self.scopes.pop();
                Ok(t)
            }
            ExprKind::Let { name, expr: init, .. } => {
                let t = self.infer_expr(init)?;
                if name != "_" {
                    if let Some(scope) = self.scopes.last_mut() {
                        scope.insert(name.clone(), t.clone());
                    }
                }
                Ok(t)
            }
            ExprKind::Call {
                callee,
                type_args,
                args,
            } => self.infer_call_expr(e, callee, type_args, args),
            ExprKind::Index { base, index } => {
                let _ = self.infer_expr(index)?;
                let tb = self.infer_expr(base)?;
                if let Some(base_tid) = self.resolve_known_tid(tb) {
                    match base_tid {
                        crate::typectx::T_ARRAY_I32 => Ok(ITy::Known(T_I32)),
                        crate::typectx::T_ARRAY_BYTES => Ok(ITy::Known(T_BYTES)),
                        T_BYTES => Ok(ITy::Known(T_I32)),
                        _ => Ok(ITy::Known(T_DYNAMIC)),
                    }
                } else {
                    Ok(ITy::Known(T_DYNAMIC))
                }
            }
            ExprKind::IndexAssign { base, index, expr } => {
                let _ = self.infer_expr(index)?;
                let tb = self.infer_expr(base)?;
                let te = self.infer_expr(expr)?;
                if let Some(base_tid) = self.resolve_known_tid(tb) {
                    let elem_tid = match base_tid {
                        crate::typectx::T_ARRAY_I32 => T_I32,
                        crate::typectx::T_ARRAY_BYTES => T_BYTES,
                        T_BYTES => T_I32,
                        _ => return Ok(ITy::Known(T_DYNAMIC)),
                    };
                    let _ = unify(&mut self.dsu, te, ITy::Known(elem_tid))?;
                }
                if let Some(base_tid) = self.resolve_known_tid(tb) {
                    Ok(ITy::Known(match base_tid {
                        crate::typectx::T_ARRAY_I32 => T_I32,
                        crate::typectx::T_ARRAY_BYTES => T_BYTES,
                        T_BYTES => T_I32,
                        _ => T_DYNAMIC,
                    }))
                } else {
                    Ok(ITy::Known(T_DYNAMIC))
                }
            }
            ExprKind::Member { .. }
            | ExprKind::TypeApp { .. }
            | ExprKind::ArrayLit(_)
            | ExprKind::TupleLit(_)
            | ExprKind::ObjLit(_)
            | ExprKind::Fn { .. }
            | ExprKind::Try { .. }
            | ExprKind::Match { .. }
            | ExprKind::New { .. } => Ok(ITy::Known(T_DYNAMIC)),
        }
    }

    fn infer_call_expr(
        &mut self,
        e: &Expr,
        callee: &Expr,
        type_args: &[crate::ast::Ty],
        args: &[Expr],
    ) -> Result<ITy, CompileError> {
        // Builtin namespaces can be shadowed by imports/lets.
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
                    if ns == "Array" && matches!(name.as_str(), "len" | "get" | "set") {
                        if !type_args.is_empty() {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                format!("Array.{} does not take type arguments", name),
                            ));
                        }
                        let t_arr = args
                            .get(0)
                            .ok_or_else(|| {
                                CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "Array.* expects at least 1 arg",
                                )
                            })
                            .and_then(|a0| self.infer_expr(a0))?;
                        let t_arr = self.resolve_known_tid(t_arr).unwrap_or(T_DYNAMIC);
                        if let Some(cs) =
                            array_constraints_from_arr_tid(name, args.len(), t_arr, e.span)?
                        {
                            self.apply_arg_constraints(args, &cs.args, e.span)?;
                            return Ok(ITy::Known(cs.ret));
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
                            .ok_or_else(|| {
                                CompileError::new(ErrorKind::Type, e.span, "List.* expects 1 arg")
                            })
                            .and_then(|a0| self.infer_expr(a0))?;
                        let t_list = self.resolve_known_tid(t_list).unwrap_or(T_DYNAMIC);
                        if let Some(cs) =
                            list_constraints_from_list_tid(name, args.len(), t_list, e.span)?
                        {
                            self.apply_arg_constraints(args, &cs.args, e.span)?;
                            return Ok(ITy::Known(cs.ret));
                        }
                    }
                }
            }
        }

        if !shadowed_ns {
            if let Some(cs) = builtin_constraints(
                callee,
                type_args,
                args.len(),
                None,
                self.type_ctx,
                false,
                e.span,
            )? {
                self.apply_arg_constraints(args, &cs.args, e.span)?;
                return Ok(ITy::Known(cs.ret));
            }
        }

        if let ExprKind::Var(n) = &callee.node {
            if n == self.self_name {
                if args.len() != self.param_vars.len() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "call arity mismatch",
                    ));
                }
                for (i, a) in args.iter().enumerate() {
                    let ta = self.infer_expr(a)?;
                    let _ = unify(&mut self.dsu, ITy::Var(self.param_vars[i]), ta)?;
                }
                return Ok(ITy::Var(self.ret_var));
            }
        }

        // Module export call: `Mod.f(...)` with known export type.
        if let ExprKind::Member { base, name } = &callee.node {
            if let ExprKind::Var(alias) = &base.node {
                if let Some(fun_tid) = (self.lookup_module_export)(alias, name) {
                    return self.infer_call_known_fun(fun_tid, args, e.span);
                }
            }
        }

        if let ExprKind::Var(n) = &callee.node {
            if let Some(ITy::Known(fun_tid)) = self.lookup(n) {
                return self.infer_call_known_fun(fun_tid, args, e.span);
            }
        }

        Ok(ITy::Known(T_DYNAMIC))
    }

    fn apply_arg_constraints(
        &mut self,
        args: &[Expr],
        constraints: &[ArgConstraint],
        span: crate::ast::Span,
    ) -> Result<(), CompileError> {
        for (i, a) in args.iter().enumerate() {
            match constraints.get(i).copied().unwrap_or(ArgConstraint::Any) {
                ArgConstraint::Exact(tid) => {
                    let ta = self.infer_expr(a)?;
                    let _ = unify(&mut self.dsu, ta, ITy::Known(tid))?;
                }
                ArgConstraint::Numeric => {
                    let ta = self.infer_expr(a)?;
                    match ta {
                        ITy::Known(tid) => {
                            if !is_numeric(tid) {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    span,
                                    "builtin expects numeric",
                                ));
                            }
                        }
                        ITy::Var(v) => self.dsu.mark_numeric(v),
                    }
                }
                ArgConstraint::Any => {}
                ArgConstraint::ObjectKind => {}
            }
        }
        Ok(())
    }

    fn infer_call_known_fun(
        &mut self,
        fun_tid: TypeId,
        args: &[Expr],
        span: crate::ast::Span,
    ) -> Result<ITy, CompileError> {
        let te =
            self.type_ctx.types.get(fun_tid as usize).ok_or_else(|| {
                CompileError::new(ErrorKind::Internal, span, "bad function type id")
            })?;
        if te.kind != crate::jlyb::TypeKind::Function {
            return Ok(ITy::Known(T_DYNAMIC));
        }
        let sig = self
            .type_ctx
            .sigs
            .get(te.p0 as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad fun sig id"))?
            .clone();
        if sig.args.len() != args.len() {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "call arity mismatch",
            ));
        }
        for (i, a) in args.iter().enumerate() {
            let ta = self.infer_expr(a)?;
            let _ = unify(&mut self.dsu, ta, ITy::Known(sig.args[i]))?;
        }
        Ok(ITy::Known(sig.ret_type))
    }
}
