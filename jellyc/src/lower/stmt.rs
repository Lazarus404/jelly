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

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::ast::Span;
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use super::{bind_local, intern_atom, lookup_var, resolve_opt_ty, FnCtx, LowerCtx, LoopTargets, T_BOOL, T_DYNAMIC};
use super::expr::{coerce_numeric, is_narrowing_numeric, lower_expr, lower_expr_expect};
use super::expr::fn_infer;

/// Collect variable names assigned in the given statements (Assign and Let).
fn vars_assigned_in(stmts: &[Stmt]) -> HashSet<String> {
    let mut out = HashSet::new();
    for s in stmts {
        match &s.node {
            StmtKind::Assign { name, .. } => {
                out.insert(name.clone());
            }
            StmtKind::Let { name, .. } => {
                out.insert(name.clone());
            }
            _ => {}
        }
    }
    out
}

/// Collect variable names used (read) in the given expression.
fn vars_used_in_expr(e: &Expr) -> HashSet<String> {
    let mut out = HashSet::new();
    fn walk(e: &Expr, out: &mut HashSet<String>) {
        match &e.node {
            ExprKind::Var(n) => {
                out.insert(n.clone());
            }
            ExprKind::Member { base, .. } => walk(base, out),
            ExprKind::Call { callee, args, .. } => {
                walk(callee, out);
                for a in args {
                    walk(a, out);
                }
            }
            ExprKind::Not(x) | ExprKind::Neg(x) => walk(x, out),
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b) | ExprKind::Le(a, b) | ExprKind::Gt(a, b) | ExprKind::Ge(a, b)
            | ExprKind::And(a, b) | ExprKind::Or(a, b) => {
                walk(a, out);
                walk(b, out);
            }
            ExprKind::If { cond, then_br, else_br } => {
                walk(cond, out);
                walk(then_br, out);
                walk(else_br, out);
            }
            ExprKind::Block { stmts: ss, expr } => {
                for s in ss {
                    vars_used_in_stmt(s, out);
                }
                walk(expr, out);
            }
            ExprKind::Index { base, index } => {
                walk(base, out);
                walk(index, out);
            }
            ExprKind::ArrayLit(es) | ExprKind::TupleLit(es) => {
                for x in es {
                    walk(x, out);
                }
            }
            ExprKind::ObjLit(fields) => {
                for (_, x) in fields {
                    walk(x, out);
                }
            }
            ExprKind::Fn { body, tail, .. } => {
                for s in body {
                    vars_used_in_stmt(s, out);
                }
                if let Some(t) = tail {
                    walk(t, out);
                }
            }
            ExprKind::Try { body, catch_body, .. } => {
                walk(body, out);
                walk(catch_body, out);
            }
            ExprKind::Match { subject, arms } => {
                walk(subject, out);
                for arm in arms {
                    if let Some(w) = &arm.when {
                        walk(w, out);
                    }
                    if let Some(t) = &arm.tail {
                        walk(t, out);
                    }
                    for s in &arm.body {
                        vars_used_in_stmt(s, out);
                    }
                }
            }
            ExprKind::New { proto, args } => {
                walk(proto, out);
                for a in args {
                    walk(a, out);
                }
            }
            ExprKind::BytesLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::I8Lit(_)
            | ExprKind::I16Lit(_)
            | ExprKind::I32Lit(_)
            | ExprKind::I64Lit(_)
            | ExprKind::F16Lit(_)
            | ExprKind::F64Lit(_)
            | ExprKind::AtomLit(_)
            | ExprKind::Null
            | ExprKind::TypeApp { .. } => {}
        }
    }
    walk(e, &mut out);
    out
}

fn vars_used_in_stmt(s: &Stmt, out: &mut HashSet<String>) {
    match &s.node {
        StmtKind::Assign { expr, .. } => {
            vars_used_in_expr(expr).into_iter().for_each(|n| {
                out.insert(n);
            });
        }
        StmtKind::MemberAssign { base, expr, .. } => {
            vars_used_in_expr(base).into_iter().for_each(|n| {
                out.insert(n);
            });
            vars_used_in_expr(expr).into_iter().for_each(|n| {
                out.insert(n);
            });
        }
        StmtKind::Let { expr, .. } => {
            vars_used_in_expr(expr).into_iter().for_each(|n| {
                out.insert(n);
            });
        }
        StmtKind::While { cond, body } => {
            vars_used_in_expr(cond).into_iter().for_each(|n| {
                out.insert(n);
            });
            for st in body {
                vars_used_in_stmt(st, out);
            }
        }
        StmtKind::Throw { expr } => {
            vars_used_in_expr(expr).into_iter().for_each(|n| {
                out.insert(n);
            });
        }
        StmtKind::Return { expr } => {
            if let Some(e) = expr {
                vars_used_in_expr(e).into_iter().for_each(|n| {
                    out.insert(n);
                });
            }
        }
        StmtKind::Expr { expr } => {
            vars_used_in_expr(expr).into_iter().for_each(|n| {
                out.insert(n);
            });
        }
        _ => {}
    }
}

pub fn lower_stmt(s: &Stmt, ctx: &mut LowerCtx, b: &mut IrBuilder) -> Result<(), CompileError> {
    fn ensure_open_block(b: &mut IrBuilder) {
        if !b.is_open() {
            let nb = b.new_block(Some("cont".to_string()));
            b.set_block(nb);
        }
    }

    match &s.node {
        StmtKind::ImportModule { path, alias } => {
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
            let is_discard = name == "_";
            if !is_discard && ctx.env_stack.iter().any(|m| m.contains_key(name)) {
                return Err(CompileError::new(
                    ErrorKind::Name,
                    s.span,
                    format!("duplicate let binding '{}'", name),
                ));
            }
            if is_discard {
                let _ = lower_expr(expr, ctx, b)?;
                return Ok(());
            }
            let expect = resolve_opt_ty(ty, ctx)?;
            if let (Some(et), true) = (expect, matches!(&expr.node, ExprKind::Fn { .. })) {
                ctx.pending_fn_self = Some((name.clone(), et));
                let dst = b.new_vreg(et);
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(name.clone(), super::Binding { v: dst, tid: et });
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
            } else if expect.is_none() && matches!(&expr.node, ExprKind::Fn { .. }) {
                // Infer a function type from the fn literal body, then lower using that
                // inferred type (enables recursion sugar).
                let (params, body, tail) = match &expr.node {
                    ExprKind::Fn { params, body, tail } => (params, body, tail),
                    _ => unreachable!(),
                };
                let (fun_tid, _args, _ret) =
                    fn_infer::infer_fn_type_for_let(name.as_str(), params, body, tail, ctx)?;

                ctx.pending_fn_self = Some((name.clone(), fun_tid));
                let dst = b.new_vreg(fun_tid);
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(name.clone(), super::Binding { v: dst, tid: fun_tid });
                let (v, tid) = lower_expr_expect(expr, Some(fun_tid), ctx, b)?;
                ctx.pending_fn_self = None;
                if tid != fun_tid {
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
                let (v0, tid0) = lower_expr_expect(expr, expect, ctx, b)?;
                ctx.pending_fn_self = None;
                let (v, tid) = if let Some(et) = expect {
                    if tid0 != et {
                        match coerce_numeric(s.span, v0, tid0, et, b) {
                            Ok(coerced) => {
                                if is_narrowing_numeric(tid0, et) {
                                    ctx.warnings.push(crate::error::CompileWarning::new(
                                        s.span,
                                        format!(
                                            "implicit narrowing conversion in let initializer from {} to {}",
                                            super::expr::type_name(tid0),
                                            super::expr::type_name(et)
                                        ),
                                    ));
                                }
                                (coerced, et)
                            }
                            Err(_) => {
                                return Err(CompileError::new(ErrorKind::Type, s.span, "let initializer type mismatch"));
                            }
                        }
                    } else {
                        (v0, tid0)
                    }
                } else {
                    (v0, tid0)
                };
                // Avoid aliasing: if the initializer vreg is already bound to some name,
                // `let y = x; y = ...` must not mutate `x`. In that case, allocate a
                // fresh vreg slot and `Mov` the initializer into it.
                // Also always use a fresh slot for Member (ObjGetAtom) results to avoid
                // reg-alloc overwriting the value before it's used (ObjGetAtom dst can
                // be reused for constants/intermediates if we don't materialize into a
                // dedicated let slot).
                let init_is_aliased = ctx
                    .env_stack
                    .iter()
                    .any(|m| m.values().any(|bd| bd.v == v));
                let init_is_member = matches!(expr.node, ExprKind::Member { .. });
                let dst = if init_is_aliased || init_is_member {
                    let dst = b.new_vreg(tid);
                    if v != dst {
                        b.emit(s.span, IrOp::Mov { dst, src: v });
                    }
                    dst
                } else {
                    v
                };
                ctx.env_stack
                    .last_mut()
                    .expect("env stack")
                    .insert(name.clone(), super::Binding { v: dst, tid });
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
            }
        }
        StmtKind::Assign { name, expr } => {
            ensure_open_block(b);
            let dst = *ctx.env_stack.iter().rev().find_map(|m| m.get(name)).ok_or_else(|| {
                CompileError::new(ErrorKind::Name, s.span, format!("unknown variable '{}'", name))
            })?;
            let (rhs, rt) = lower_expr(expr, ctx, b)?;
            let rhs = if rt != dst.tid {
                let coerced = coerce_numeric(s.span, rhs, rt, dst.tid, b).map_err(|_| {
                    CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        format!("assignment to '{}' changes type", name),
                    )
                })?;
                if is_narrowing_numeric(rt, dst.tid) {
                    ctx.warnings.push(crate::error::CompileWarning::new(
                        s.span,
                        format!(
                            "implicit narrowing conversion in assignment from {} to {}",
                            super::expr::type_name(rt),
                            super::expr::type_name(dst.tid)
                        ),
                    ));
                }
                coerced
            } else {
                rhs
            };
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

            b.set_block(from);
            b.term(IrTerminator::Jmp { target: cond_b });

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

            let assigned = vars_assigned_in(body);
            let used_in_cond = vars_used_in_expr(cond);
            let loop_vars: Vec<String> = assigned
                .intersection(&used_in_cond)
                .filter(|name| ctx.env_stack.iter().rev().any(|m| m.contains_key(*name)))
                .cloned()
                .collect();

            b.set_block(cond_b);
            let mut cond_env = HashMap::new();
            for name in &loop_vars {
                let bd = lookup_var(ctx, name, cond.span)?;
                let phi_dst = b.new_vreg(bd.tid);
                b.emit(
                    Span::point(0),
                    IrOp::Phi {
                        dst: phi_dst,
                        incomings: vec![(from, bd.v), (tail, bd.v)],
                    },
                );
                cond_env.insert(name.clone(), super::Binding { v: phi_dst, tid: bd.tid });
            }
            ctx.env_stack.push(cond_env);

            let (v_cond, t_cond) = lower_expr(cond, ctx, b)?;
            ctx.env_stack.pop();
            if t_cond != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, cond.span, "while condition must be bool"));
            }
            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: body_b,
                else_tgt: exit_b,
            });

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

                if let ExprKind::Call { callee, type_args, args } = &e.node {
                    if type_args.is_empty() {
                        if let ExprKind::Var(n) = &callee.node {
                            if fc.self_name.as_deref() == Some(n.as_str()) {
                                return emit_tail_self_call(e, n, args, fc, ret_tid, ctx, b);
                            }
                        }
                    }
                }

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
        StmtKind::Expr { expr } => {
            ensure_open_block(b);
            let _ = lower_expr(expr, ctx, b)?;
            Ok(())
        }
        _ => Err(CompileError::new(
            ErrorKind::Codegen,
            s.span,
            "IR lowering: statement not implemented yet",
        )),
    }
}
