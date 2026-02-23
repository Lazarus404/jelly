use super::*;

mod index;
mod member;
mod var;

pub(super) fn lower_var_expr(
    e: &Expr,
    name: &str,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    var::lower_var_expr(e, name, expect, ctx, b)
}

pub(super) fn lower_member_expr(
    e: &Expr,
    base: &Expr,
    name: &str,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    member::lower_member_expr(e, base, name, expect, ctx, b)
}

pub(super) fn lower_index_assign_expr(
    e: &Expr,
    base: &Expr,
    index: &Expr,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    index::lower_index_assign_expr(e, base, index, expr0, ctx, b)
}

pub(super) fn lower_index_expr(
    e: &Expr,
    base: &Expr,
    index: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    index::lower_index_expr(e, base, index, ctx, b)
}
