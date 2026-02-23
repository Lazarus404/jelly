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
        x if x == Op::BytesNew as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("bytes new reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bytes || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("bytes_new types required"));
            }
            Ok(true)
        }
        x if x == Op::BytesLen as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("bytes len reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::Bytes {
                return Err(ctx.err("bytes_len types required"));
            }
            Ok(true)
        }
        x if x == Op::BytesGetU8 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("bytes get_u8 reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32
                || ctx.rk(ins.b)? != TypeKind::Bytes
                || ctx.rk(ins.c)? != TypeKind::I32
            {
                return Err(ctx.err("bytes_get_u8 types required"));
            }
            Ok(true)
        }
        x if x == Op::BytesSetU8 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("bytes set_u8 reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32
                || ctx.rk(ins.b)? != TypeKind::Bytes
                || ctx.rk(ins.c)? != TypeKind::I32
            {
                return Err(ctx.err("bytes_set_u8 types required"));
            }
            Ok(true)
        }
        x if x == Op::BytesConcat2 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("bytes concat2 reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bytes
                || ctx.rk(ins.b)? != TypeKind::Bytes
                || ctx.rk(ins.c)? != TypeKind::Bytes
            {
                return Err(ctx.err("bytes_concat2 types required"));
            }
            Ok(true)
        }
        x if x == Op::BytesConcatMany as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("bytes concat_many reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bytes {
                return Err(ctx.err("bytes_concat_many dst must be bytes"));
            }
            if ctx.rk(ins.b)? != TypeKind::Array {
                return Err(ctx.err("bytes_concat_many src must be array"));
            }
            let arr_tid = ctx.reg_types[ins.b as usize];
            let elem_tid = ctx.unary_elem_tid(arr_tid, TypeKind::Array, "bytes_concat_many")?;
            if ctx.type_kind(elem_tid)? != TypeKind::Bytes {
                return Err(ctx.err("bytes_concat_many requires Array<bytes>"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
