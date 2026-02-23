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

use crate::lower::is_object_kind;

use super::super::{lower_expr_expect, LowerCtx, T_ATOM, T_OBJECT};

pub(super) fn try_lower_object_builtin(
    e: &Expr,
    ns: &str,
    name: &str,
    out_tid: TypeId,
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    if ns != "Object" {
        return Ok(None);
    }

    if name == "get" {
        let (v_obj, t_obj) = lower_expr_expect(&args[0], ctx, b)?;
        if !is_object_kind(&ctx.type_ctx, t_obj) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Object.get receiver",
            ));
        }
        let (v_key, t_key) = lower_expr_expect(&args[1], ctx, b)?;
        if t_key != T_ATOM {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[1].span,
                "semantic type mismatch for Object.get key",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(
            e.span,
            IrOp::ObjGet {
                dst: out,
                obj: v_obj,
                atom: v_key,
            },
        );
        return Ok(Some((out, out_tid)));
    }

    if name == "set" {
        let (v_obj, t_obj) = lower_expr_expect(&args[0], ctx, b)?;
        if !is_object_kind(&ctx.type_ctx, t_obj) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Object.set receiver",
            ));
        }
        let (v_key, t_key) = lower_expr_expect(&args[1], ctx, b)?;
        if t_key != T_ATOM {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[1].span,
                "semantic type mismatch for Object.set key",
            ));
        }
        let (v_val, _t_val) = lower_expr_expect(&args[2], ctx, b)?;
        b.emit(
            e.span,
            IrOp::ObjSet {
                obj: v_obj,
                atom: v_key,
                value: v_val,
            },
        );
        if out_tid != t_obj && out_tid != T_OBJECT {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Object.set",
            ));
        }
        return Ok(Some((v_obj, out_tid)));
    }

    Ok(None)
}
