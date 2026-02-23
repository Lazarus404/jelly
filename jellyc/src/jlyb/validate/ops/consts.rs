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
        x if x == Op::ConstI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            let k = ctx.rk(ins.a)?;
            if k != TypeKind::I8 && k != TypeKind::I16 && k != TypeKind::I32 {
                return Err(ctx.err("const_i32 dst must be i8/i16/i32"));
            }
            // Range constraints for small ints (stored as i32 slots in the VM).
            if k == TypeKind::I8 {
                let v = ins.imm as i32;
                if v < -128 || v > 127 {
                    return Err(ctx.err("const_i32 out of range for i8"));
                }
            } else if k == TypeKind::I16 {
                let v = ins.imm as i32;
                if v < -32768 || v > 32767 {
                    return Err(ctx.err("const_i32 out of range for i16"));
                }
            }
            Ok(true)
        }
        x if x == Op::ConstI8Imm as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I8 {
                return Err(ctx.err("const_i8_imm dst must be i8"));
            }
            Ok(true)
        }
        x if x == Op::ConstF16 as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F16 {
                return Err(ctx.err("const_f16 dst must be f16"));
            }
            Ok(true)
        }
        x if x == Op::ConstBool as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("const_bool dst must be bool"));
            }
            if ins.c > 1 {
                return Err(ctx.err("const_bool imm must be 0/1"));
            }
            Ok(true)
        }
        x if x == Op::ConstNull as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Dynamic {
                return Err(ctx.err("const_null dst must be Dynamic"));
            }
            Ok(true)
        }
        x if x == Op::ConstAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Atom {
                return Err(ctx.err("const_atom dst must be atom"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("const_atom atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ConstF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F32 {
                return Err(ctx.err("const_f32 dst must be f32"));
            }
            Ok(true)
        }
        x if x == Op::ConstI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I64 {
                return Err(ctx.err("const_i64 dst must be i64"));
            }
            if (ins.imm as usize) >= ctx.m.const_i64.len() {
                return Err(ctx.err("const_i64 pool index out of range"));
            }
            Ok(true)
        }
        x if x == Op::ConstF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::F64 {
                return Err(ctx.err("const_f64 dst must be f64"));
            }
            if (ins.imm as usize) >= ctx.m.const_f64.len() {
                return Err(ctx.err("const_f64 pool index out of range"));
            }
            Ok(true)
        }
        x if x == Op::ConstBytes as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bytes {
                return Err(ctx.err("const_bytes dst must be bytes"));
            }
            if (ins.imm as usize) >= ctx.m.const_bytes.len() {
                return Err(ctx.err("const_bytes pool index out of range"));
            }
            Ok(true)
        }
        x if x == Op::ConstFun as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("const reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Function {
                return Err(ctx.err("const_fun dst must be function"));
            }
            if ins.imm > ctx.nfuncs_logical_max {
                return Err(ctx.err("const_fun func index out of range"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
