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
// Operator lowering helpers (numeric ops, boolean ops).
use crate::ast::{Expr, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, TypeId, VRegId};

use super::{coerce_numeric, is_numeric, LowerCtx, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

mod arithmetic;
mod compare;
mod equality;
mod logical;

fn numeric_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        T_F16 => 4,
        T_F32 => 5,
        T_F64 => 6,
        _ => 255,
    }
}

pub(super) fn promote_numeric_bin(
    span: Span,
    va: VRegId,
    ta: TypeId,
    vb: VRegId,
    tb: TypeId,
    op: &str,
    expect: Option<TypeId>,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId, VRegId, TypeId, TypeId), CompileError> {
    if !is_numeric(ta) || !is_numeric(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            format!("'{}' expects numeric operands", op),
        ));
    }

    let out_t = match op {
        "/" => {
            if expect == Some(T_F64) {
                T_F64
            } else if ta == T_F64 || tb == T_F64 {
                T_F64
            } else {
                T_F32
            }
        }
        _ => {
            if expect == Some(T_F64) {
                T_F64
            } else if expect == Some(T_F32) && (numeric_rank(ta) >= 2 || numeric_rank(tb) >= 2) {
                // If a float is involved and we have a float expectation, respect it.
                T_F32
            } else {
                // Widen to the "largest" participating numeric type.
                if numeric_rank(ta) >= numeric_rank(tb) {
                    ta
                } else {
                    tb
                }
            }
        }
    };

    let va2 = coerce_numeric(span, va, ta, out_t, b)?;
    let vb2 = coerce_numeric(span, vb, tb, out_t, b)?;
    Ok((va2, out_t, vb2, out_t, out_t))
}

pub(super) fn lower_add_expr(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    arithmetic::lower_add_expr(e, expect, ctx, b)
}

pub(super) fn lower_sub_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    arithmetic::lower_sub_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_mul_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    arithmetic::lower_mul_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_div_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    arithmetic::lower_div_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_neg_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    arithmetic::lower_neg_expr(e, inner, ctx, b)
}

pub(super) fn lower_truthy_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    logical::lower_truthy_expr(e, inner, ctx, b)
}

pub(super) fn lower_not_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    logical::lower_not_expr(e, inner, ctx, b)
}

pub(super) fn lower_eq_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    equality::lower_eq_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_ne_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    equality::lower_ne_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_lt_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    compare::lower_lt_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_gt_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    compare::lower_gt_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_le_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    compare::lower_le_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_ge_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    compare::lower_ge_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_and_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    logical::lower_and_expr(e, a, bb, ctx, b)
}

pub(super) fn lower_or_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    logical::lower_or_expr(e, a, bb, ctx, b)
}
