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
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::super::{is_numeric, lower_expr, LowerCtx};
use super::super::super::{T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

pub(super) fn lower_neg_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (v, t) = lower_expr(inner, ctx, b)?;
    if !is_numeric(t) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "unary '-' expects numeric",
        ));
    }
    let out = b.new_vreg(t);
    match t {
        T_I8 | T_I16 | T_I32 => b.emit(e.span, IrOp::NegI32 { dst: out, src: v }),
        T_I64 => b.emit(e.span, IrOp::NegI64 { dst: out, src: v }),
        T_F16 => {
            // F16 has no NegF16; convert via F32
            let v32 = b.new_vreg(T_F32);
            b.emit(e.span, IrOp::F32FromF16 { dst: v32, src: v });
            let neg32 = b.new_vreg(T_F32);
            b.emit(
                e.span,
                IrOp::NegF32 {
                    dst: neg32,
                    src: v32,
                },
            );
            b.emit(
                e.span,
                IrOp::F16FromF32 {
                    dst: out,
                    src: neg32,
                },
            );
        }
        T_F32 => b.emit(e.span, IrOp::NegF32 { dst: out, src: v }),
        T_F64 => b.emit(e.span, IrOp::NegF64 { dst: out, src: v }),
        _ => {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "bad neg type",
            ))
        }
    }
    Ok((out, t))
}
