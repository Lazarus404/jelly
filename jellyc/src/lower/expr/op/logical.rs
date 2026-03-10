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
