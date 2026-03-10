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
use crate::typectx::{T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

use super::super::{coerce_numeric, lower_expr_expect, LowerCtx};

pub(super) fn try_lower_numeric_builtin(
    e: &Expr,
    ns: &str,
    name: &str,
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    // Numeric conversion: Integer.to_i8, Integer.to_i16, Integer.to_i32, Integer.to_i64,
    // Float.to_f16, Float.to_f32, Float.to_f64.
    if args.len() == 1 {
        let target_tid = match (ns, name) {
            ("Integer", "to_i8") => Some(T_I8),
            ("Integer", "to_i16") => Some(T_I16),
            ("Integer", "to_i32") => Some(T_I32),
            ("Integer", "to_i64") => Some(T_I64),
            ("Float", "to_f16") => Some(T_F16),
            ("Float", "to_f32") => Some(T_F32),
            ("Float", "to_f64") => Some(T_F64),
            _ => None,
        };
        if let Some(tid) = target_tid {
            let (v, t) = lower_expr_expect(&args[0], ctx, b)?;
            if !super::super::is_numeric(t) {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    args[0].span,
                    "semantic type mismatch for numeric conversion builtin",
                ));
            }
            let out = coerce_numeric(e.span, v, t, tid, b).map_err(|_| {
                CompileError::new(
                    ErrorKind::Internal,
                    args[0].span,
                    "unsupported numeric conversion in lowering",
                )
            })?;
            return Ok(Some((out, tid)));
        }
    }

    // Math.sqrt(x): any numeric -> F64 (native builtin)
    if ns == "Math" && name == "sqrt" {
        let (v, t) = lower_expr_expect(&args[0], ctx, b)?;
        if !super::super::is_numeric(t) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Math.sqrt",
            ));
        }
        let v_f64 = coerce_numeric(e.span, v, t, T_F64, b).map_err(|_| {
            CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "unsupported numeric conversion for Math.sqrt in lowering",
            )
        })?;

        let sig_args = [T_F64];
        let sig_id = ctx.type_ctx.intern_sig(T_F64, &sig_args);
        let fun_tid = ctx.type_ctx.intern_fun_type(T_F64, &sig_args);

        let vcallee = b.new_vreg(fun_tid);
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::NATIVE_BUILTIN_MATH_SQRT,
            },
        );

        let arg0 = b.new_vreg(T_F64);
        b.emit(
            args[0].span,
            IrOp::Mov {
                dst: arg0,
                src: v_f64,
            },
        );

        let out = b.new_vreg(T_F64);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base: arg0,
                nargs: 1,
            },
        );
        return Ok(Some((out, T_F64)));
    }
    Ok(None)
}
