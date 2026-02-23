use crate::ast::{Expr, ExprKind, Spanned};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{is_numeric, lower_expr, LowerCtx};
use super::super::{T_BOOL, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

pub(super) fn lower_lt_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (va, ta) = lower_expr(a, ctx, b)?;
    let (vb, tb) = lower_expr(bb, ctx, b)?;
    if !is_numeric(ta) || !is_numeric(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'<' expects numeric operands",
        ));
    }
    let (va2, _ta2, vb2, _tb2, out_t) =
        super::promote_numeric_bin(e.span, va, ta, vb, tb, "<", None, b)?;
    let out = b.new_vreg(T_BOOL);
    match out_t {
        T_I8 | T_I16 | T_I32 => b.emit(
            e.span,
            IrOp::LtI32 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_I64 => b.emit(
            e.span,
            IrOp::LtI64 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_F16 => {
            // F16 has no direct compare op; compare via F32.
            let a32 = b.new_vreg(T_F32);
            let b32 = b.new_vreg(T_F32);
            b.emit(e.span, IrOp::F32FromF16 { dst: a32, src: va2 });
            b.emit(e.span, IrOp::F32FromF16 { dst: b32, src: vb2 });
            b.emit(
                e.span,
                IrOp::LtF32 {
                    dst: out,
                    a: a32,
                    b: b32,
                },
            );
        }
        T_F32 => b.emit(
            e.span,
            IrOp::LtF32 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_F64 => b.emit(
            e.span,
            IrOp::LtF64 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        _ => {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "bad lt promotion",
            ))
        }
    }
    Ok((out, T_BOOL))
}

pub(super) fn lower_gt_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // a > b  ==  b < a
    lower_expr(
        &Spanned::new(
            ExprKind::Lt(Box::new(bb.clone()), Box::new(a.clone())),
            e.span,
        ),
        ctx,
        b,
    )
}

pub(super) fn lower_le_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // a <= b  ==  !(b < a)
    let (t, tt) = lower_expr(
        &Spanned::new(
            ExprKind::Lt(Box::new(bb.clone()), Box::new(a.clone())),
            e.span,
        ),
        ctx,
        b,
    )?;
    if tt != T_BOOL {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "bad le lowering",
        ));
    }
    let out = b.new_vreg(T_BOOL);
    b.emit(e.span, IrOp::NotBool { dst: out, src: t });
    Ok((out, T_BOOL))
}

pub(super) fn lower_ge_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // a >= b  ==  !(a < b)
    let (t, tt) = lower_expr(
        &Spanned::new(
            ExprKind::Lt(Box::new(a.clone()), Box::new(bb.clone())),
            e.span,
        ),
        ctx,
        b,
    )?;
    if tt != T_BOOL {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "bad ge lowering",
        ));
    }
    let out = b.new_vreg(T_BOOL);
    b.emit(e.span, IrOp::NotBool { dst: out, src: t });
    Ok((out, T_BOOL))
}
