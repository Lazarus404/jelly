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

mod arith;
mod array;
mod bytes;
mod calls;
mod compare;
mod consts;
mod control;
mod conv;
mod dyn_ops;
mod list;
mod object;

use crate::jlyb::Insn;

use super::ctx::ValidateCtx;

pub(super) fn validate_insn(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<(), String> {
    if control::validate(ctx, ins)? {
        return Ok(());
    }
    if calls::validate(ctx, ins)? {
        return Ok(());
    }
    if consts::validate(ctx, ins)? {
        return Ok(());
    }
    if dyn_ops::validate(ctx, ins)? {
        return Ok(());
    }
    if arith::validate(ctx, ins)? {
        return Ok(());
    }
    if compare::validate(ctx, ins)? {
        return Ok(());
    }
    if conv::validate(ctx, ins)? {
        return Ok(());
    }
    if list::validate(ctx, ins)? {
        return Ok(());
    }
    if array::validate(ctx, ins)? {
        return Ok(());
    }
    if bytes::validate(ctx, ins)? {
        return Ok(());
    }
    if object::validate(ctx, ins)? {
        return Ok(());
    }

    // If an opcode isn't explicitly validated above, fail fast.
    //
    // This keeps the validator in sync with the VM's rules as opcodes are added/changed,
    // and prevents accidental "accept by default" behavior.
    if ins.op > 117 {
        return Err(ctx.err(format!("unknown opcode: {}", ins.op)));
    }
    Err(ctx.err(format!("opcode not yet validated: {}", ins.op)))
}
