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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::hir::ConstValue;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};
use crate::typectx::{T_ATOM, T_BYTES, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

use crate::lower::expr;
use crate::lower::{intern_atom, LowerCtx, T_BOOL, T_DYNAMIC};

pub(super) fn emit_const_typed(
    span: Span,
    tid: TypeId,
    cv: ConstValue,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    match cv {
        ConstValue::Bool(x) => {
            let out = b.new_vreg(T_BOOL);
            b.emit(span, IrOp::ConstBool { dst: out, imm: x });
            if tid != T_BOOL {
                let dst = b.new_vreg(tid);
                b.emit(span, IrOp::Mov { dst, src: out });
                Ok(dst)
            } else {
                Ok(out)
            }
        }
        ConstValue::Int(x) => match tid {
            T_I8 => {
                let out = b.new_vreg(T_I8);
                let imm = (x as i16).to_le_bytes()[0];
                b.emit(span, IrOp::ConstI8Imm { dst: out, imm });
                Ok(out)
            }
            T_I16 | T_I32 => {
                let out = b.new_vreg(tid);
                b.emit(
                    span,
                    IrOp::ConstI32 {
                        dst: out,
                        imm: x as i32,
                    },
                );
                Ok(out)
            }
            T_I64 => {
                let idx = ctx.const_i64.len() as u32;
                ctx.const_i64.push(x as i64);
                let out = b.new_vreg(T_I64);
                b.emit(
                    span,
                    IrOp::ConstI64 {
                        dst: out,
                        pool_index: idx,
                    },
                );
                Ok(out)
            }
            T_F16 => {
                let out = b.new_vreg(T_F16);
                let bits = expr::f32_to_f16_bits(x as f32);
                b.emit(span, IrOp::ConstF16 { dst: out, bits });
                Ok(out)
            }
            T_F32 => {
                let out = b.new_vreg(T_F32);
                let bits = (x as f32).to_bits();
                b.emit(span, IrOp::ConstF32 { dst: out, bits });
                Ok(out)
            }
            T_F64 => {
                let idx = ctx.const_f64.len() as u32;
                ctx.const_f64.push(x as f64);
                let out = b.new_vreg(T_F64);
                b.emit(
                    span,
                    IrOp::ConstF64 {
                        dst: out,
                        pool_index: idx,
                    },
                );
                Ok(out)
            }
            _ => Err(CompileError::new(
                ErrorKind::Type,
                span,
                "const integer initializer type mismatch",
            )),
        },
        ConstValue::Float(x) => match tid {
            T_F16 => {
                let out = b.new_vreg(T_F16);
                let bits = expr::f32_to_f16_bits(x as f32);
                b.emit(span, IrOp::ConstF16 { dst: out, bits });
                Ok(out)
            }
            T_F32 => {
                let out = b.new_vreg(T_F32);
                let bits = (x as f32).to_bits();
                b.emit(span, IrOp::ConstF32 { dst: out, bits });
                Ok(out)
            }
            T_F64 => {
                let idx = ctx.const_f64.len() as u32;
                ctx.const_f64.push(x);
                let out = b.new_vreg(T_F64);
                b.emit(
                    span,
                    IrOp::ConstF64 {
                        dst: out,
                        pool_index: idx,
                    },
                );
                Ok(out)
            }
            _ => Err(CompileError::new(
                ErrorKind::Type,
                span,
                "const float initializer type mismatch",
            )),
        },
        ConstValue::Bytes(bytes) => {
            let idx = ctx.const_bytes.len() as u32;
            ctx.const_bytes.push(bytes);
            let out = b.new_vreg(T_BYTES);
            b.emit(
                span,
                IrOp::ConstBytes {
                    dst: out,
                    pool_index: idx,
                },
            );
            if tid != T_BYTES {
                let dst = b.new_vreg(tid);
                b.emit(span, IrOp::Mov { dst, src: out });
                Ok(dst)
            } else {
                Ok(out)
            }
        }
        ConstValue::Atom(name) => {
            let atom_id = intern_atom(name.as_str(), ctx);
            let out = b.new_vreg(T_ATOM);
            b.emit(span, IrOp::ConstAtom { dst: out, atom_id });
            if tid != T_ATOM {
                let dst = b.new_vreg(tid);
                b.emit(span, IrOp::Mov { dst, src: out });
                Ok(dst)
            } else {
                Ok(out)
            }
        }
        ConstValue::Null => {
            let v_dyn = b.new_vreg(T_DYNAMIC);
            b.emit(span, IrOp::ConstNull { dst: v_dyn });
            if tid == T_DYNAMIC {
                Ok(v_dyn)
            } else {
                let dst = b.new_vreg(tid);
                b.emit(span, IrOp::Mov { dst, src: v_dyn });
                Ok(dst)
            }
        }
    }
}
