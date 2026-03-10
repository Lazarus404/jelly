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
use crate::ir::IrOp;

pub(super) fn lower_var_expr(
    e: &Expr,
    name: &str,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Self-recursive references reuse the prologue binding (single ConstFun per function).
    if ctx
        .env_stack
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .is_none()
    {
        let _ = ensure_capture_binding(ctx, b, name, e.span)?;
    }
    let bd = lookup_var(ctx, name, e.span)?;
    if let Some(et) = expect {
        if bd.tid == et {
            return Ok((bd.v, bd.tid));
        }
        // Nominal object upcast: allow `Box<I32>` where plain `Object` is expected.
        if et == T_OBJECT && is_object_kind(&ctx.type_ctx, bd.tid) && bd.tid != T_OBJECT {
            let out = b.new_vreg(T_OBJECT);
            b.emit(
                e.span,
                IrOp::Mov {
                    dst: out,
                    src: bd.v,
                },
            );
            return Ok((out, T_OBJECT));
        }
        // Implicit boxing/unboxing at Dynamic boundaries.
        if et == T_DYNAMIC {
            let out = b.new_vreg(T_DYNAMIC);
            b.emit(
                e.span,
                IrOp::ToDyn {
                    dst: out,
                    src: bd.v,
                },
            );
            return Ok((out, T_DYNAMIC));
        }
        if bd.tid == T_DYNAMIC {
            let out = b.new_vreg(et);
            match et {
                T_I8 => b.emit(
                    e.span,
                    IrOp::FromDynI8 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_I16 => b.emit(
                    e.span,
                    IrOp::FromDynI16 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_I32 => b.emit(
                    e.span,
                    IrOp::FromDynI32 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_I64 => b.emit(
                    e.span,
                    IrOp::FromDynI64 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_F16 => b.emit(
                    e.span,
                    IrOp::FromDynF16 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_F32 => b.emit(
                    e.span,
                    IrOp::FromDynF32 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_F64 => b.emit(
                    e.span,
                    IrOp::FromDynF64 {
                        dst: out,
                        src: bd.v,
                    },
                ),
                T_BOOL => b.emit(
                    e.span,
                    IrOp::FromDynBool {
                        dst: out,
                        src: bd.v,
                    },
                ),
                // bytes/object/array ptr kinds
                T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(
                    e.span,
                    IrOp::FromDynPtr {
                        dst: out,
                        src: bd.v,
                    },
                ),
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "unsupported Dynamic->typed conversion",
                    ))
                }
            }
            return Ok((out, et));
        }
        // Implicit numeric conversion (widen or narrow with warning)
        if is_numeric(bd.tid) && is_numeric(et) {
            if let Ok(coerced) = coerce_numeric(e.span, bd.v, bd.tid, et, b) {
                if is_narrowing_numeric(bd.tid, et) {
                    ctx.warnings.push(crate::error::CompileWarning::new(
                        e.span,
                        format!(
                            "implicit narrowing conversion from {} to {}",
                            type_name(bd.tid),
                            type_name(et)
                        ),
                    ));
                }
                return Ok((coerced, et));
            }
        }
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "type mismatch (no implicit conversion)",
        ));
    }
    Ok((bd.v, bd.tid))
}
