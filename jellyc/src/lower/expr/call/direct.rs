use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{coerce_numeric, is_narrowing_numeric, lower_expr_expect, LowerCtx};

/// Prepare a tail call: lower callee and args, return (callee, sig_id, arg_base, nargs).
/// Returns Err if not a valid tail call (e.g. type mismatch).
pub fn prepare_tail_call(
    e: &crate::ast::Expr,
    callee: &crate::ast::Expr,
    args: &[crate::ast::Expr],
    ret_tid: TypeId,
    ctx: &mut super::super::LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, u32, VRegId, u8), CompileError> {
    let (vcallee, callee_tid) = lower_expr_expect(callee, ctx, b)?;
    let te = ctx
        .type_ctx
        .types
        .get(callee_tid as usize)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad callee type id"))?;
    if te.kind != crate::jlyb::TypeKind::Function {
        return Err(CompileError::new(
            ErrorKind::Type,
            callee.span,
            "call target is not a function",
        ));
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

    let mut arg_vals: Vec<VRegId> = Vec::with_capacity(args.len());
    for (i, a) in args.iter().enumerate() {
        let (va, ta) = lower_expr_expect(a, ctx, b)?;
        let va = if ta != sig_args[i] {
            if sig_args[i] == crate::typectx::T_DYNAMIC {
                let out = b.new_vreg(crate::typectx::T_DYNAMIC);
                b.emit(a.span, IrOp::ToDyn { dst: out, src: va });
                out
            } else if super::super::is_numeric(ta) && super::super::is_numeric(sig_args[i]) {
                let coerced = coerce_numeric(a.span, va, ta, sig_args[i], b).map_err(|_| {
                    CompileError::new(ErrorKind::Type, a.span, "tail call argument type mismatch")
                })?;
                if is_narrowing_numeric(ta, sig_args[i]) {
                    ctx.warnings.push(crate::error::CompileWarning::new(
                        a.span,
                        format!(
                            "implicit narrowing conversion from {} to {}",
                            super::super::type_name(ta),
                            super::super::type_name(sig_args[i])
                        ),
                    ));
                }
                coerced
            } else {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    a.span,
                    "tail call argument type mismatch",
                ));
            }
        } else {
            va
        };
        arg_vals.push(va);
    }

    let (arg_base, nargs) =
        super::marshal::marshal_args_window(e.span, &sig_args, &arg_vals, b, true);
    Ok((vcallee, sig_id, arg_base, nargs))
}

pub(super) fn lower_direct_call_expr(
    e: &crate::ast::Expr,
    callee: &crate::ast::Expr,
    args: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (vcallee, callee_tid) = lower_expr_expect(callee, ctx, b)?;
    let te = ctx
        .type_ctx
        .types
        .get(callee_tid as usize)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad callee type id"))?;
    if te.kind != crate::jlyb::TypeKind::Function {
        return Err(CompileError::new(
            ErrorKind::Type,
            callee.span,
            "call target is not a function",
        ));
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
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "call arity mismatch",
        ));
    }

    // Build a contiguous vreg window holding the arguments (implicit widening/narrowing with warning).
    let mut arg_vals: Vec<VRegId> = Vec::with_capacity(args.len());
    for (i, a) in args.iter().enumerate() {
        let (va, ta) = lower_expr_expect(a, ctx, b)?;
        let va = if ta != sig_args[i] {
            if sig_args[i] == crate::typectx::T_DYNAMIC {
                let out = b.new_vreg(crate::typectx::T_DYNAMIC);
                b.emit(a.span, IrOp::ToDyn { dst: out, src: va });
                out
            } else if super::super::is_numeric(ta) && super::super::is_numeric(sig_args[i]) {
                let coerced = coerce_numeric(a.span, va, ta, sig_args[i], b).map_err(|_| {
                    CompileError::new(ErrorKind::Type, a.span, "call argument type mismatch")
                })?;
                if is_narrowing_numeric(ta, sig_args[i]) {
                    ctx.warnings.push(crate::error::CompileWarning::new(
                        a.span,
                        format!(
                            "implicit narrowing conversion from {} to {}",
                            super::super::type_name(ta),
                            super::super::type_name(sig_args[i])
                        ),
                    ));
                }
                coerced
            } else {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    a.span,
                    "call argument type mismatch",
                ));
            }
        } else {
            va
        };
        arg_vals.push(va);
    }

    let (arg_base, nargs) =
        super::marshal::marshal_args_window(e.span, &sig_args, &arg_vals, b, false);
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
