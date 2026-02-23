use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::super::{coerce_numeric, is_numeric, lower_expr, LowerCtx};
use super::super::super::{T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

pub(super) fn lower_mul_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let out_t = expect.ok_or_else(|| {
        CompileError::new(
            ErrorKind::Internal,
            e.span,
            "missing semantic type for '*' expression",
        )
    })?;
    if !is_numeric(out_t) {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "semantic type for '*' is not numeric",
        ));
    }
    let (va, ta) = lower_expr(a, ctx, b)?;
    let (vb, tb) = lower_expr(bb, ctx, b)?;
    if !is_numeric(ta) || !is_numeric(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'*' expects numeric operands",
        ));
    }
    let va2 = coerce_numeric(e.span, va, ta, out_t, b)?;
    let vb2 = coerce_numeric(e.span, vb, tb, out_t, b)?;
    let out = b.new_vreg(out_t);
    match out_t {
        T_I8 | T_I16 | T_I32 => b.emit(
            e.span,
            IrOp::MulI32 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_I64 => b.emit(
            e.span,
            IrOp::MulI64 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_F16 => b.emit(
            e.span,
            IrOp::MulF16 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_F32 => b.emit(
            e.span,
            IrOp::MulF32 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        T_F64 => b.emit(
            e.span,
            IrOp::MulF64 {
                dst: out,
                a: va2,
                b: vb2,
            },
        ),
        _ => {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "bad numeric mul type",
            ))
        }
    }
    Ok((out, out_t))
}
