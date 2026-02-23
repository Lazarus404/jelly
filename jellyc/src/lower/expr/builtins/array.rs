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

use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{lower_expr_expect, LowerCtx, T_ARRAY_BYTES, T_ARRAY_I32};
use crate::typectx::T_I32;

pub(super) fn try_lower_array_builtin(
    e: &Expr,
    ns: &str,
    name: &str,
    out_tid: TypeId,
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    if ns != "Array" {
        return Ok(None);
    }

    if name == "new" {
        let (vlen, tlen) = lower_expr_expect(&args[0], ctx, b)?;
        if tlen != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Array.new",
            ));
        }
        let arr_tid = out_tid;
        let out = b.new_vreg(arr_tid);
        b.emit(
            e.span,
            IrOp::ArrayNew {
                dst: out,
                len: vlen,
            },
        );
        return Ok(Some((out, arr_tid)));
    }

    if name == "len" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        if ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Array.len receiver",
            ));
        }
        let out = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ArrayLen { dst: out, arr: va });
        return Ok(Some((out, T_I32)));
    }

    if name == "get" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], ctx, b)?;
        if (ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES) || ti != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.get",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(
            e.span,
            IrOp::ArrayGet {
                dst: out,
                arr: va,
                index: vi,
            },
        );
        return Ok(Some((out, out_tid)));
    }

    if name == "set" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], ctx, b)?;
        let want = super::super::elem_tid_for_array(ta).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.set receiver",
            )
        })?;
        let (vv, tv) = lower_expr_expect(&args[2], ctx, b)?;
        if ti != T_I32 || tv != want {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.set",
            ));
        }
        b.emit(
            e.span,
            IrOp::ArraySet {
                arr: va,
                index: vi,
                value: vv,
            },
        );
        if out_tid != ta {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.set return type",
            ));
        }
        return Ok(Some((va, out_tid)));
    }

    Ok(None)
}
