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
        x if x == Op::AddI32 as u8
            || x == Op::SubI32 as u8
            || x == Op::MulI32 as u8
            || x == Op::DivI32 as u8
            || x == Op::ModI32 as u8
            || x == Op::ShlI32 as u8
            || x == Op::ShrI32 as u8 =>
        {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("arith reg out of range"));
            }
            if !ctx.is_i32ish(ctx.rk(ins.a)?)
                || !ctx.is_i32ish(ctx.rk(ins.b)?)
                || !ctx.is_i32ish(ctx.rk(ins.c)?)
            {
                return Err(ctx.err("arith i8/i16/i32 types required"));
            }
            Ok(true)
        }
        x if x == Op::AddI32Imm as u8 || x == Op::SubI32Imm as u8 || x == Op::MulI32Imm as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("arith_imm reg out of range"));
            }
            if !ctx.is_i32ish(ctx.rk(ins.a)?) || !ctx.is_i32ish(ctx.rk(ins.b)?) {
                return Err(ctx.err("arith_imm i8/i16/i32 types required"));
            }
            Ok(true)
        }
        x if x == Op::AddI64 as u8
            || x == Op::SubI64 as u8
            || x == Op::MulI64 as u8
            || x == Op::DivI64 as u8
            || x == Op::ModI64 as u8
            || x == Op::ShlI64 as u8
            || x == Op::ShrI64 as u8 =>
        {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("arith reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I64
                || ctx.rk(ins.b)? != TypeKind::I64
                || ctx.rk(ins.c)? != TypeKind::I64
            {
                return Err(ctx.err("arith i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::AddF32 as u8
            || x == Op::SubF32 as u8
            || x == Op::MulF32 as u8
            || x == Op::DivF32 as u8 =>
        {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("arith reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32
                || ctx.rk(ins.b)? != TypeKind::F32
                || ctx.rk(ins.c)? != TypeKind::F32
            {
                return Err(ctx.err("arith f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::AddF16 as u8 || x == Op::SubF16 as u8 || x == Op::MulF16 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("arith reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F16
                || ctx.rk(ins.b)? != TypeKind::F16
                || ctx.rk(ins.c)? != TypeKind::F16
            {
                return Err(ctx.err("arith f16 types required"));
            }
            Ok(true)
        }
        x if x == Op::AddF64 as u8
            || x == Op::SubF64 as u8
            || x == Op::MulF64 as u8
            || x == Op::DivF64 as u8 =>
        {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("arith reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F64
                || ctx.rk(ins.b)? != TypeKind::F64
                || ctx.rk(ins.c)? != TypeKind::F64
            {
                return Err(ctx.err("arith f64 types required"));
            }
            Ok(true)
        }
        x if x == Op::NegI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("neg reg out of range"));
            }
            if !ctx.is_i32ish(ctx.rk(ins.a)?) || !ctx.is_i32ish(ctx.rk(ins.b)?) {
                return Err(ctx.err("neg_i32 types must be i8/i16/i32"));
            }
            Ok(true)
        }
        x if x == Op::NegI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("neg reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I64 || ctx.rk(ins.b)? != TypeKind::I64 {
                return Err(ctx.err("neg_i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::NegF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("neg reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32 || ctx.rk(ins.b)? != TypeKind::F32 {
                return Err(ctx.err("neg_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::NegF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("neg reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F64 || ctx.rk(ins.b)? != TypeKind::F64 {
                return Err(ctx.err("neg_f64 types required"));
            }
            Ok(true)
        }
        x if x == Op::NotBool as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("not reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool || ctx.rk(ins.b)? != TypeKind::Bool {
                return Err(ctx.err("not_bool types required"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
