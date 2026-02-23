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
        x if x == Op::ObjNew as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("obj new reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Object {
                return Err(ctx.err("obj_new dst must be object"));
            }
            Ok(true)
        }
        x if x == Op::ObjHasAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("obj has reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool || ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_has types required"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("obj_has atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ObjGetAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("obj get reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_get src must be object"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("obj_get atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ObjSetAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("obj set reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_set dst must be object"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("obj_set atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ObjGet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("obj get reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_get src must be object"));
            }
            if ctx.rk(ins.c)? != TypeKind::Atom {
                return Err(ctx.err("obj_get key must be atom"));
            }
            Ok(true)
        }
        x if x == Op::ObjSet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("obj set reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_set dst must be object"));
            }
            if ctx.rk(ins.c)? != TypeKind::Atom {
                return Err(ctx.err("obj_set key must be atom"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
