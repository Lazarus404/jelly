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
        x if x == Op::Closure as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("closure reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Function {
                return Err(ctx.err("closure dst must be function"));
            }
            if ins.imm > ctx.nfuncs_logical_max {
                return Err(ctx.err("closure func index out of range"));
            }
            let first = ins.b as u32;
            let ncaps = ins.c as u32;
            if first + ncaps > ctx.nregs {
                return Err(ctx.err("closure capture range out of range"));
            }
            Ok(true)
        }
        x if x == Op::BindThis as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("bind_this reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Function || ctx.rk(ins.b)? != TypeKind::Function {
                return Err(ctx.err("bind_this requires function regs"));
            }
            Ok(true)
        }
        x if x == Op::Call as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("call reg out of range"));
            }
            if ins.imm > ctx.nfuncs_logical_max {
                return Err(ctx.err("call func index out of range"));
            }
            let first = ins.b as u32;
            let nargs = ins.c as u32;
            if first + nargs > ctx.nregs {
                return Err(ctx.err("call arg range out of range"));
            }
            Ok(true)
        }
        x if x == Op::CallR as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("callr reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Function {
                return Err(ctx.err("callr callee must be function"));
            }
            let first = ins.imm;
            let nargs = ins.c as u32;
            if first >= ctx.nregs {
                return Err(ctx.err("callr arg base out of range"));
            }
            if first + nargs > ctx.nregs {
                return Err(ctx.err("callr arg range out of range"));
            }
            Ok(true)
        }
        x if x == Op::TailCall as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("tailcall reg out of range"));
            }
            if ins.imm > ctx.nfuncs_logical_max {
                return Err(ctx.err("tailcall func index out of range"));
            }
            let first = ins.b as u32;
            let nargs = ins.c as u32;
            if first + nargs > ctx.nregs {
                return Err(ctx.err("tailcall arg range out of range"));
            }
            Ok(true)
        }
        x if x == Op::TailCallR as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("tailcallr reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Function {
                return Err(ctx.err("tailcallr callee must be function"));
            }
            let first = ins.imm;
            let nargs = ins.c as u32;
            if first >= ctx.nregs {
                return Err(ctx.err("tailcallr arg base out of range"));
            }
            if first + nargs > ctx.nregs {
                return Err(ctx.err("tailcallr arg range out of range"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
