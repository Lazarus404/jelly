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

use crate::ast::{Expr, ExprKind, Stmt};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use crate::jlyb::TypeKind;

use super::super::super::expr::call;
use super::super::super::expr::{lower_expr, lower_expr_expect};
use super::super::super::{ensure_open_block, FnCtx, LowerCtx, T_BOOL, T_DYNAMIC};

pub(super) fn lower_throw_stmt(
    s: &Stmt,
    expr: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
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

pub(super) fn lower_expr_stmt(
    _s: &Stmt,
    expr: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let _ = lower_expr(expr, ctx, b)?;
    Ok(())
}

pub(super) fn lower_return_stmt(
    s: &Stmt,
    expr: Option<&Expr>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);

    let fc = ctx
        .fn_stack
        .last()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                s.span,
                "return outside of function lowering",
            )
        })?
        .clone();
    let ret_tid = fc.ret_tid;

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
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "not a self tail call",
        ));
    }
    let self_tid = fc
        .self_fun_tid
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing self fun tid"))?;
    let te = ctx
        .type_ctx
        .types
        .get(self_tid as usize)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad self fun tid"))?;
    if te.kind != TypeKind::Function {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "self is not a function",
        ));
    }
    let sig_id = te.p0;
    let (sig_args, sig_ret) = ctx
        .type_ctx
        .sigs
        .get(sig_id as usize)
        .map(|s| (s.args.clone(), s.ret_type))
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad self fun sig id"))?;
    if sig_ret != ret_tid {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "tail call return type mismatch",
        ));
    }
    if sig_args.len() != args.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "tail call arity mismatch",
        ));
    }

    let mut tmps: Vec<VRegId> = Vec::with_capacity(args.len());
    for (i, a) in args.iter().enumerate() {
        let want = sig_args[i];
        let (va, ta) = lower_expr_expect(a, ctx, b)?;
        let va = if want == T_DYNAMIC && ta != T_DYNAMIC {
            let out = b.new_vreg(T_DYNAMIC);
            b.emit(a.span, IrOp::ToDyn { dst: out, src: va });
            out
        } else if ta != want {
            return Err(CompileError::new(
                ErrorKind::Type,
                a.span,
                "tail call argument type mismatch",
            ));
        } else {
            va
        };
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
    /* Unwrap block only when the inner expr is a self-call, so we can emit Jmp.
     * Avoids affecting other cases (e.g. mutual tail calls) by being very targeted. */
    if let ExprKind::Block { stmts, expr } = &e.node {
        if stmts.is_empty() {
            if let ExprKind::Call { callee, .. } = &expr.node {
                if let ExprKind::Var(n) = &callee.node {
                    if fc.self_name.as_deref() == Some(n.as_str()) {
                        return lower_return_expr(expr, fc, ret_tid, ctx, b);
                    }
                }
            }
        }
    }

    if let ExprKind::If {
        cond,
        then_br,
        else_br,
    } = &e.node
    {
        let (v_cond, t_cond) = lower_expr(cond, ctx, b)?;
        if t_cond != T_BOOL {
            return Err(CompileError::new(
                ErrorKind::Type,
                cond.span,
                "if condition must be bool",
            ));
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

    if let ExprKind::Call {
        callee,
        type_args,
        args,
    } = &e.node
    {
        if type_args.is_empty() {
            if let ExprKind::Var(n) = &callee.node {
                if fc.self_name.as_deref() == Some(n.as_str()) {
                    return emit_tail_self_call(e, n, args, fc, ret_tid, ctx, b);
                }
            }
            // Non-self tail call: emit TailCall terminator (VM replaces frame).
            if let Ok((vcallee, sig_id, arg_base, nargs)) =
                call::prepare_tail_call(e, callee, args, ret_tid, ctx, b)
            {
                b.term(IrTerminator::TailCall {
                    callee: vcallee,
                    sig_id,
                    arg_base,
                    nargs,
                });
                return Ok(());
            }
        }
    }

    let (v, tid) = lower_expr_expect(e, ctx, b)?;
    let v = if ret_tid == T_DYNAMIC && tid != T_DYNAMIC {
        let out = b.new_vreg(T_DYNAMIC);
        b.emit(e.span, IrOp::ToDyn { dst: out, src: v });
        out
    } else {
        if tid != ret_tid {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "return type mismatch",
            ));
        }
        v
    };
    b.term(IrTerminator::Ret { value: v });
    Ok(())
}
