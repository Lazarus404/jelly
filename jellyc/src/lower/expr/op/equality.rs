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

use crate::ast::{Expr, ExprKind, Spanned};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{is_numeric, lower_expr, lower_truthy, LowerCtx};
use super::super::{T_BOOL, T_BYTES, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

pub(super) fn lower_eq_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (va, ta) = lower_expr(a, ctx, b)?;
    let (vb, tb) = lower_expr(bb, ctx, b)?;
    let (va, ta, vb, tb) = if ta != tb && is_numeric(ta) && is_numeric(tb) {
        let (va2, _ta2, vb2, _tb2, out_t) =
            super::promote_numeric_bin(e.span, va, ta, vb, tb, "==", None, b)?;
        (va2, out_t, vb2, out_t)
    } else {
        (va, ta, vb, tb)
    };

    // Truthiness equality: when comparing against a boolean,
    // interpret the other side via truthy/falsey semantics.
    if ta == T_BOOL && tb != T_BOOL {
        let vbt = lower_truthy(e.span, vb, tb, ctx, b)?;
        let out = b.new_vreg(T_BOOL);
        b.emit(
            e.span,
            IrOp::Physeq {
                dst: out,
                a: va,
                b: vbt,
            },
        );
        return Ok((out, T_BOOL));
    }
    if tb == T_BOOL && ta != T_BOOL {
        let vat = lower_truthy(e.span, va, ta, ctx, b)?;
        let out = b.new_vreg(T_BOOL);
        b.emit(
            e.span,
            IrOp::Physeq {
                dst: out,
                a: vat,
                b: vb,
            },
        );
        return Ok((out, T_BOOL));
    }
    if ta != tb {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'==' expects operands of same type",
        ));
    }

    // Tuple structural equality (element-wise).
    if ctx.type_ctx.is_tuple_type(ta) {
        let out = super::super::lower_tuple_eq(e.span, va, vb, ta, ctx, b)?;
        return Ok((out, T_BOOL));
    }

    let out = b.new_vreg(T_BOOL);
    match ta {
        T_BOOL | super::super::T_ATOM => {
            b.emit(
                e.span,
                IrOp::Physeq {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            Ok((out, T_BOOL))
        }
        T_I8 | T_I16 | T_I32 => {
            b.emit(
                e.span,
                IrOp::EqI32 {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            Ok((out, T_BOOL))
        }
        T_I64 => {
            b.emit(
                e.span,
                IrOp::EqI64 {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            Ok((out, T_BOOL))
        }
        T_F16 => {
            let va32 = b.new_vreg(T_F32);
            let vb32 = b.new_vreg(T_F32);
            b.emit(e.span, IrOp::F32FromF16 { dst: va32, src: va });
            b.emit(e.span, IrOp::F32FromF16 { dst: vb32, src: vb });
            b.emit(
                e.span,
                IrOp::EqF32 {
                    dst: out,
                    a: va32,
                    b: vb32,
                },
            );
            Ok((out, T_BOOL))
        }
        T_F32 => {
            b.emit(
                e.span,
                IrOp::EqF32 {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            Ok((out, T_BOOL))
        }
        T_F64 => {
            b.emit(
                e.span,
                IrOp::EqF64 {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            Ok((out, T_BOOL))
        }
        T_BYTES => {
            super::super::emit_bytes_eq(e.span, out, va, vb, ctx, b)?;
            Ok((out, T_BOOL))
        }
        super::super::T_DYNAMIC => {
            // Dynamic equality is physical equality on boxed values.
            // This is primarily used for `x == null` checks.
            b.emit(
                e.span,
                IrOp::Physeq {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            Ok((out, T_BOOL))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'==' not supported for this type yet",
        )),
    }
}

pub(super) fn lower_ne_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // a != b  ==  !(a == b)
    let (t, tt) = lower_expr(
        &Spanned::new(
            ExprKind::Eq(Box::new(a.clone()), Box::new(bb.clone())),
            e.span,
        ),
        ctx,
        b,
    )?;
    if tt != T_BOOL {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "bad ne lowering",
        ));
    }
    let out = b.new_vreg(T_BOOL);
    b.emit(e.span, IrOp::NotBool { dst: out, src: t });
    Ok((out, T_BOOL))
}
