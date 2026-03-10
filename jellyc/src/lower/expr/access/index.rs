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

use super::super::*;
use crate::error::ErrorKind;
use crate::ir::IrOp;

pub(super) fn lower_index_assign_expr(
    e: &Expr,
    base: &Expr,
    index: &Expr,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (v_base, t_base) = lower_expr(base, ctx, b)?;
    let (v_idx_raw, t_idx_raw) = lower_expr(index, ctx, b)?;
    let v_idx = match t_idx_raw {
        T_I32 => v_idx_raw,
        T_I8 | T_I16 | T_I64 => coerce_numeric(index.span, v_idx_raw, t_idx_raw, T_I32, b)?,
        T_DYNAMIC => lower_expr_expect(index, ctx, b)?.0,
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                index.span,
                "index must be an integer",
            ))
        }
    };
    match t_base {
        T_ARRAY_I32 => {
            let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
            let v_val = if t_val == T_I32 {
                v_val
            } else if super::super::is_numeric(t_val) {
                let coerced = coerce_numeric(expr0.span, v_val, t_val, T_I32, b).map_err(|_| {
                    CompileError::new(
                        ErrorKind::Type,
                        expr0.span,
                        "Array<I32> index assignment requires numeric value",
                    )
                })?;
                if is_narrowing_numeric(t_val, T_I32) {
                    ctx.warnings.push(crate::error::CompileWarning::new(
                        expr0.span,
                        format!(
                            "implicit narrowing conversion in array assignment from {} to {}",
                            super::super::type_name(t_val),
                            super::super::type_name(T_I32)
                        ),
                    ));
                }
                coerced
            } else {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    expr0.span,
                    "Array<I32> index assignment requires numeric value",
                ));
            };
            b.emit(
                e.span,
                IrOp::ArraySet {
                    arr: v_base,
                    index: v_idx,
                    value: v_val,
                },
            );
            Ok((v_val, T_I32))
        }
        T_ARRAY_BYTES => {
            let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
            if t_val != T_BYTES {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    expr0.span,
                    "Array<Bytes> index assignment requires Bytes value",
                ));
            }
            b.emit(
                e.span,
                IrOp::ArraySet {
                    arr: v_base,
                    index: v_idx,
                    value: v_val,
                },
            );
            Ok((v_val, T_BYTES))
        }
        T_BYTES => {
            let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
            let v_val = match t_val {
                T_I32 => v_val,
                T_I8 | T_I16 | T_I64 | T_F16 | T_F32 | T_F64 => {
                    let coerced =
                        coerce_numeric(expr0.span, v_val, t_val, T_I32, b).map_err(|_| {
                            CompileError::new(
                                ErrorKind::Type,
                                expr0.span,
                                "bytes index assignment requires numeric value",
                            )
                        })?;
                    if is_narrowing_numeric(t_val, T_I32) {
                        ctx.warnings.push(crate::error::CompileWarning::new(
                            expr0.span,
                            format!(
                                "implicit narrowing conversion in bytes assignment from {} to {}",
                                super::super::type_name(t_val),
                                super::super::type_name(T_I32)
                            ),
                        ));
                    }
                    coerced
                }
                T_DYNAMIC => lower_expr_expect(expr0, ctx, b)?.0,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        expr0.span,
                        "bytes index assignment requires numeric value",
                    ))
                }
            };
            b.emit(
                e.span,
                IrOp::BytesSetU8 {
                    bytes: v_base,
                    index: v_idx,
                    value: v_val,
                },
            );
            Ok((v_val, T_I32))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "index assignment only supported for Array<I32>, Array<Bytes>, and bytes",
        )),
    }
}

pub(super) fn lower_index_expr(
    e: &Expr,
    base: &Expr,
    index: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (vb, tb) = lower_expr(base, ctx, b)?;
    let (vi_raw, ti_raw) = lower_expr(index, ctx, b)?;
    let (vi, _ti) = match ti_raw {
        T_I32 => (vi_raw, T_I32),
        // Allow any integer index type (and Dynamic via unboxing), but the VM op expects I32.
        T_I8 | T_I16 | T_I64 => (coerce_numeric(index.span, vi_raw, ti_raw, T_I32, b)?, T_I32),
        T_DYNAMIC => {
            return Err(CompileError::new(
                ErrorKind::Internal,
                index.span,
                "index expression should have been coerced to I32 by semantic analysis",
            ))
        }
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                index.span,
                "index must be an integer",
            ))
        }
    };
    match tb {
        T_ARRAY_I32 => {
            let out = b.new_vreg(T_I32);
            b.emit(
                e.span,
                IrOp::ArrayGet {
                    dst: out,
                    arr: vb,
                    index: vi,
                },
            );
            Ok((out, T_I32))
        }
        T_ARRAY_BYTES => {
            let out = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::ArrayGet {
                    dst: out,
                    arr: vb,
                    index: vi,
                },
            );
            Ok((out, T_BYTES))
        }
        T_BYTES => {
            let out = b.new_vreg(T_I32);
            b.emit(
                e.span,
                IrOp::BytesGetU8 {
                    dst: out,
                    bytes: vb,
                    index: vi,
                },
            );
            Ok((out, T_I32))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "indexing not supported for this type yet",
        )),
    }
}
