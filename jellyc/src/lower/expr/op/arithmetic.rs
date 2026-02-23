use crate::ast::Expr;
use crate::error::CompileError;
use crate::ir::{IrBuilder, TypeId, VRegId};

use super::super::LowerCtx;

mod add;
mod div;
mod mul;
mod neg;
mod sub;

pub(super) fn lower_add_expr(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    add::lower_add_expr(e, expect, ctx, b)
}

pub(super) fn lower_sub_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    sub::lower_sub_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_mul_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    mul::lower_mul_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_div_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    div::lower_div_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_neg_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    neg::lower_neg_expr(e, inner, ctx, b)
}
