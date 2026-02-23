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

use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::SextI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I64 {
                return Err(ctx.err("sext_i64 dst must be i64"));
            }
            if !ctx.is_i32ish(ctx.rk(ins.b)?) {
                return Err(ctx.err("sext_i64 src must be i8/i16/i32"));
            }
            Ok(true)
        }
        x if x == Op::SextI16 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I16 || ctx.rk(ins.b)? != TypeKind::I8 {
                return Err(ctx.err("sext_i16 dst must be i16, src i8"));
            }
            Ok(true)
        }
        x if x == Op::TruncI8 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I8 {
                return Err(ctx.err("trunc_i8 dst must be i8"));
            }
            let sb = ctx.rk(ins.b)?;
            if sb != TypeKind::I16 && sb != TypeKind::I32 {
                return Err(ctx.err("trunc_i8 src must be i16 or i32"));
            }
            Ok(true)
        }
        x if x == Op::TruncI16 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I16 || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("trunc_i16 dst must be i16, src i32"));
            }
            Ok(true)
        }
        x if x == Op::I32FromI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::I64 {
                return Err(ctx.err("i32_from_i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::F64FromI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F64 || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("f64_from_i32 types required"));
            }
            Ok(true)
        }
        x if x == Op::I32FromF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::F64 {
                return Err(ctx.err("i32_from_f64 types required"));
            }
            Ok(true)
        }
        x if x == Op::F64FromI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F64 || ctx.rk(ins.b)? != TypeKind::I64 {
                return Err(ctx.err("f64_from_i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::I64FromF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I64 || ctx.rk(ins.b)? != TypeKind::F64 {
                return Err(ctx.err("i64_from_f64 types required"));
            }
            Ok(true)
        }
        x if x == Op::F32FromI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32 || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("f32_from_i32 types required"));
            }
            Ok(true)
        }
        x if x == Op::I32FromF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::F32 {
                return Err(ctx.err("i32_from_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::F64FromF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F64 || ctx.rk(ins.b)? != TypeKind::F32 {
                return Err(ctx.err("f64_from_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::F32FromF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32 || ctx.rk(ins.b)? != TypeKind::F64 {
                return Err(ctx.err("f32_from_f64 types required"));
            }
            Ok(true)
        }
        x if x == Op::F16FromF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F16 || ctx.rk(ins.b)? != TypeKind::F32 {
                return Err(ctx.err("f16_from_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::F32FromF16 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32 || ctx.rk(ins.b)? != TypeKind::F16 {
                return Err(ctx.err("f32_from_f16 types required"));
            }
            Ok(true)
        }
        x if x == Op::F16FromI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F16 || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("f16_from_i32 types required"));
            }
            Ok(true)
        }
        x if x == Op::I32FromF16 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::F16 {
                return Err(ctx.err("i32_from_f16 types required"));
            }
            Ok(true)
        }
        x if x == Op::F32FromI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32 || ctx.rk(ins.b)? != TypeKind::I64 {
                return Err(ctx.err("f32_from_i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::I64FromF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("conv reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I64 || ctx.rk(ins.b)? != TypeKind::F32 {
                return Err(ctx.err("i64_from_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::Kindof as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("kindof reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::Dynamic {
                return Err(ctx.err("kindof types required"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
