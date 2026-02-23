use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use super::super::{lower_expr, lower_truthy, LowerCtx, T_BOOL};

pub(super) fn lower_truthy_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (v, t) = lower_expr(inner, ctx, b)?;
    let out = lower_truthy(e.span, v, t, ctx, b)?;
    Ok((out, T_BOOL))
}

pub(super) fn lower_not_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (v, t) = lower_expr(inner, ctx, b)?;
    if t != T_BOOL {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'!' expects bool",
        ));
    }
    let out = b.new_vreg(T_BOOL);
    b.emit(e.span, IrOp::NotBool { dst: out, src: v });
    Ok((out, T_BOOL))
}

pub(super) fn lower_and_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Short-circuit: if(!a) res=a(false) else res=b
    let (va0, ta0) = lower_expr(a, ctx, b)?;
    let va = if ta0 == T_BOOL {
        va0
    } else {
        lower_truthy(e.span, va0, ta0, ctx, b)?
    };

    let rhs_b = b.new_block(Some("and_rhs".to_string()));
    let short_b = b.new_block(Some("and_short".to_string()));
    let join_b = b.new_block(Some("and_join".to_string()));

    b.term(IrTerminator::JmpIf {
        cond: va,
        then_tgt: rhs_b,
        else_tgt: short_b,
    });

    b.set_block(short_b);
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(rhs_b);
    let (vb0, tb0) = lower_expr(bb, ctx, b)?;
    let vb = if tb0 == T_BOOL {
        vb0
    } else {
        lower_truthy(e.span, vb0, tb0, ctx, b)?
    };
    let rhs_end = b.cur_block();
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(join_b);
    let v_res = b.new_vreg(T_BOOL);
    b.emit(
        e.span,
        IrOp::Phi {
            dst: v_res,
            incomings: vec![(short_b, va), (rhs_end, vb)],
        },
    );
    Ok((v_res, T_BOOL))
}

pub(super) fn lower_or_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Short-circuit:
    //   if(a) res=a(true) else res=b
    let (va0, ta0) = lower_expr(a, ctx, b)?;
    let va = if ta0 == T_BOOL {
        va0
    } else {
        lower_truthy(e.span, va0, ta0, ctx, b)?
    };

    let short_b = b.new_block(Some("or_short".to_string()));
    let rhs_b = b.new_block(Some("or_rhs".to_string()));
    let join_b = b.new_block(Some("or_join".to_string()));

    b.term(IrTerminator::JmpIf {
        cond: va,
        then_tgt: short_b,
        else_tgt: rhs_b,
    });

    b.set_block(short_b);
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(rhs_b);
    let (vb0, tb0) = lower_expr(bb, ctx, b)?;
    let vb = if tb0 == T_BOOL {
        vb0
    } else {
        lower_truthy(e.span, vb0, tb0, ctx, b)?
    };
    let rhs_end = b.cur_block();
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(join_b);
    let v_res = b.new_vreg(T_BOOL);
    b.emit(
        e.span,
        IrOp::Phi {
            dst: v_res,
            incomings: vec![(short_b, va), (rhs_end, vb)],
        },
    );
    Ok((v_res, T_BOOL))
}
