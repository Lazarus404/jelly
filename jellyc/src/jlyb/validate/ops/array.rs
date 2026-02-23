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
        x if x == Op::ArrayNew as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("array new reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Array || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("array_new types required"));
            }
            Ok(true)
        }
        x if x == Op::ArrayLen as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("array len reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::Array {
                return Err(ctx.err("array_len types required"));
            }
            Ok(true)
        }
        x if x == Op::ArrayGet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("array get reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Array || ctx.rk(ins.c)? != TypeKind::I32 {
                return Err(ctx.err("array_get types required"));
            }
            let arr_tid = ctx.reg_types[ins.b as usize];
            let elem_tid = ctx
                .m
                .types
                .get(arr_tid as usize)
                .map(|te| te.p0)
                .ok_or_else(|| ctx.err("array_get array type entry out of range"))?;
            if elem_tid != ctx.reg_types[ins.a as usize] {
                return Err(ctx.err("array_get dst type mismatch"));
            }
            Ok(true)
        }
        x if x == Op::ArraySet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("array set reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Array || ctx.rk(ins.c)? != TypeKind::I32 {
                return Err(ctx.err("array_set types required"));
            }
            let arr_tid = ctx.reg_types[ins.b as usize];
            let elem_tid = ctx
                .m
                .types
                .get(arr_tid as usize)
                .map(|te| te.p0)
                .ok_or_else(|| ctx.err("array_set array type entry out of range"))?;
            if elem_tid != ctx.reg_types[ins.a as usize] {
                return Err(ctx.err("array_set src type mismatch"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
