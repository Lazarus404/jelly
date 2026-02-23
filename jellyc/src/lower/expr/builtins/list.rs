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

use super::super::{lower_expr_expect, LowerCtx, T_LIST_BYTES, T_LIST_I32};

pub(super) fn try_lower_list_builtin(
    e: &Expr,
    ns: &str,
    name: &str,
    out_tid: TypeId,
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    if ns != "List" {
        return Ok(None);
    }

    if name == "nil" {
        let list_tid = out_tid;
        let out = b.new_vreg(list_tid);
        b.emit(e.span, IrOp::ListNil { dst: out });
        return Ok(Some((out, list_tid)));
    }
    if name == "cons" {
        let list_tid = out_tid;
        let elem_tid = super::super::elem_tid_for_list(list_tid).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for List.cons return type",
            )
        })?;
        let (vhead, thead) = lower_expr_expect(&args[0], ctx, b)?;
        let (vtail, ttail) = lower_expr_expect(&args[1], ctx, b)?;
        if thead != elem_tid || ttail != list_tid {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for List.cons",
            ));
        }
        let out = b.new_vreg(list_tid);
        b.emit(
            e.span,
            IrOp::ListCons {
                dst: out,
                head: vhead,
                tail: vtail,
            },
        );
        return Ok(Some((out, list_tid)));
    }
    if name == "head" {
        let (vl, tl) = lower_expr_expect(&args[0], ctx, b)?;
        if super::super::elem_tid_for_list(tl).is_none() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for List.head receiver",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ListHead { dst: out, list: vl });
        return Ok(Some((out, out_tid)));
    }
    if name == "tail" {
        let (vl, tl) = lower_expr_expect(&args[0], ctx, b)?;
        if tl != T_LIST_I32 && tl != T_LIST_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for List.tail receiver",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ListTail { dst: out, list: vl });
        return Ok(Some((out, out_tid)));
    }
    if name == "is_nil" {
        let (vl, tl) = lower_expr_expect(&args[0], ctx, b)?;
        if tl != T_LIST_I32 && tl != T_LIST_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for List.is_nil receiver",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ListIsNil { dst: out, list: vl });
        return Ok(Some((out, out_tid)));
    }

    Ok(None)
}
