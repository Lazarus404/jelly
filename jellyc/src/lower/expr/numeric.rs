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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::{T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

/// Convert f32 to IEEE 754 binary16 bits (matches vm_f32_to_f16_bits in reg.c).
pub(crate) fn f32_to_f16_bits(f: f32) -> u16 {
    let u32_bits = f.to_bits();
    let sign = (u32_bits >> 16) & 0x8000;
    let exp = (u32_bits >> 23) & 0xFF;
    let mant = u32_bits & 0x7FFFFF;
    if exp == 0xFF {
        return (sign | 0x7C00 | if mant != 0 { 0x200 } else { 0 }) as u16;
    }
    if exp == 0 && mant == 0 {
        return sign as u16;
    }
    let exp16 = (exp as i32) - 127 + 15;
    if exp16 >= 31 {
        return (sign | 0x7C00) as u16;
    }
    if exp16 <= 0 {
        return sign as u16;
    }
    (sign | ((exp16 as u32) << 10) | (mant >> 13)) as u16
}

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

pub(super) fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

pub(super) fn join_numeric(a: TypeId, b: TypeId) -> TypeId {
    if numeric_rank(a) >= numeric_rank(b) {
        a
    } else {
        b
    }
}

pub(super) fn type_name(tid: TypeId) -> &'static str {
    match tid {
        T_I8 => "I8",
        T_I16 => "I16",
        T_I32 => "I32",
        T_I64 => "I64",
        T_F16 => "F16",
        T_F32 => "F32",
        T_F64 => "F64",
        _ => "?",
    }
}

/// True if converting from `from` to `to` is a narrowing conversion (may lose precision/range).
pub(crate) fn is_narrowing_numeric(from: TypeId, to: TypeId) -> bool {
    let r_from = numeric_rank(from);
    let r_to = numeric_rank(to);
    if r_from == 255 || r_to == 255 {
        return false;
    }
    // Float to int: always narrowing
    let from_int = r_from <= 3;
    let to_int = r_to <= 3;
    if !from_int && to_int {
        return true;
    }
    // Same category: narrowing when from has higher rank
    if from_int == to_int {
        return r_from > r_to;
    }
    // Int to float: not narrowing
    false
}

/// Coerce a numeric value from one type to another. Returns Err for unsupported conversions.
pub(crate) fn coerce_numeric(
    span: Span,
    v: VRegId,
    from: TypeId,
    to: TypeId,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    if from == to {
        return Ok(v);
    }
    let out = b.new_vreg(to);
    match (from, to) {
        // Integer widening
        (T_I8, T_I16) => b.emit(span, IrOp::SextI16 { dst: out, src: v }),
        (T_I8, T_I32) | (T_I16, T_I32) => b.emit(span, IrOp::Mov { dst: out, src: v }),
        (T_I32, T_I64) => b.emit(span, IrOp::SextI64 { dst: out, src: v }),
        (T_I8, T_I64) | (T_I16, T_I64) => {
            let v32 = b.new_vreg(T_I32);
            b.emit(span, IrOp::Mov { dst: v32, src: v });
            b.emit(span, IrOp::SextI64 { dst: out, src: v32 });
        }
        // Integer narrowing
        (T_I16, T_I8) | (T_I32, T_I8) => b.emit(span, IrOp::TruncI8 { dst: out, src: v }),
        (T_I32, T_I16) => b.emit(span, IrOp::TruncI16 { dst: out, src: v }),
        (T_I64, T_I32) => b.emit(span, IrOp::I32FromI64 { dst: out, src: v }),
        (T_I64, T_I16) => {
            let v32 = b.new_vreg(T_I32);
            b.emit(span, IrOp::I32FromI64 { dst: v32, src: v });
            b.emit(span, IrOp::TruncI16 { dst: out, src: v32 });
        }
        (T_I64, T_I8) => {
            let v32 = b.new_vreg(T_I32);
            b.emit(span, IrOp::I32FromI64 { dst: v32, src: v });
            b.emit(span, IrOp::TruncI8 { dst: out, src: v32 });
        }
        // Int to float
        (T_I32, T_F32) => b.emit(span, IrOp::F32FromI32 { dst: out, src: v }),
        (T_I32, T_F64) => b.emit(span, IrOp::F64FromI32 { dst: out, src: v }),
        (T_I64, T_F64) => b.emit(span, IrOp::F64FromI64 { dst: out, src: v }),
        (T_I64, T_F32) => b.emit(span, IrOp::F32FromI64 { dst: out, src: v }),
        (T_I32, T_F16) => b.emit(span, IrOp::F16FromI32 { dst: out, src: v }),
        (T_I8, T_F64) | (T_I16, T_F64) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_I8 => b.emit(span, IrOp::Mov { dst: v32, src: v }),
                T_I16 => b.emit(span, IrOp::Mov { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::F64FromI32 { dst: out, src: v32 });
        }
        (T_I8, T_F32) | (T_I16, T_F32) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_I8 => b.emit(span, IrOp::Mov { dst: v32, src: v }),
                T_I16 => b.emit(span, IrOp::Mov { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::F32FromI32 { dst: out, src: v32 });
        }
        (T_I8, T_F16) | (T_I16, T_F16) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_I8 => b.emit(span, IrOp::Mov { dst: v32, src: v }),
                T_I16 => b.emit(span, IrOp::Mov { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::F16FromI32 { dst: out, src: v32 });
        }
        (T_I64, T_F16) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromI64 { dst: v32, src: v });
            b.emit(span, IrOp::F16FromF32 { dst: out, src: v32 });
        }
        // Float widening
        (T_F16, T_F32) => b.emit(span, IrOp::F32FromF16 { dst: out, src: v }),
        (T_F16, T_F64) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromF16 { dst: v32, src: v });
            b.emit(span, IrOp::F64FromF32 { dst: out, src: v32 });
        }
        (T_F32, T_F64) => b.emit(span, IrOp::F64FromF32 { dst: out, src: v }),
        // Float narrowing
        (T_F32, T_F16) => b.emit(span, IrOp::F16FromF32 { dst: out, src: v }),
        (T_F64, T_F16) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromF64 { dst: v32, src: v });
            b.emit(span, IrOp::F16FromF32 { dst: out, src: v32 });
        }
        (T_F64, T_F32) => b.emit(span, IrOp::F32FromF64 { dst: out, src: v }),
        // Float to int
        (T_F64, T_I32) => b.emit(span, IrOp::I32FromF64 { dst: out, src: v }),
        (T_F64, T_I64) => b.emit(span, IrOp::I64FromF64 { dst: out, src: v }),
        (T_F32, T_I32) => b.emit(span, IrOp::I32FromF32 { dst: out, src: v }),
        (T_F32, T_I64) => b.emit(span, IrOp::I64FromF32 { dst: out, src: v }),
        (T_F16, T_I32) => b.emit(span, IrOp::I32FromF16 { dst: out, src: v }),
        (T_F16, T_I64) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromF16 { dst: v32, src: v });
            b.emit(span, IrOp::I64FromF32 { dst: out, src: v32 });
        }
        (T_F64, T_I8) | (T_F32, T_I8) | (T_F16, T_I8) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_F64 => b.emit(span, IrOp::I32FromF64 { dst: v32, src: v }),
                T_F32 => b.emit(span, IrOp::I32FromF32 { dst: v32, src: v }),
                T_F16 => b.emit(span, IrOp::I32FromF16 { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::TruncI8 { dst: out, src: v32 });
        }
        (T_F64, T_I16) | (T_F32, T_I16) | (T_F16, T_I16) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_F64 => b.emit(span, IrOp::I32FromF64 { dst: v32, src: v }),
                T_F32 => b.emit(span, IrOp::I32FromF32 { dst: v32, src: v }),
                T_F16 => b.emit(span, IrOp::I32FromF16 { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::TruncI16 { dst: out, src: v32 });
        }
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "unsupported numeric conversion",
            ))
        }
    }
    Ok(out)
}
