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

use crate::error::CompileError;
use crate::ir::{IrInsn, IrOp};
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::{blk_pc, delta, reg};

pub(super) fn emit(
    block_start: &[u32],
    ins: &IrInsn,
    this_pc: u32,
    vinsns: &mut Vec<VInsn>,
) -> Result<bool, CompileError> {
    match &ins.op {
        IrOp::Throw { payload } => {
            vinsns.push(VInsn {
                op: Op::Throw as u8,
                a: reg(*payload),
                b: 0,
                c: 0,
                imm: 0,
            });
            Ok(true)
        }
        IrOp::Try {
            catch_dst,
            catch_block,
            trap_only,
        } => {
            let catch_pc = blk_pc(block_start, *catch_block)?;
            vinsns.push(VInsn {
                op: Op::Try as u8,
                a: reg(*catch_dst),
                b: if *trap_only { 1 } else { 0 },
                c: 0,
                imm: delta(this_pc, catch_pc),
            });
            Ok(true)
        }
        IrOp::EndTry => {
            vinsns.push(VInsn {
                op: Op::EndTry as u8,
                a: 0,
                b: 0,
                c: 0,
                imm: 0,
            });
            Ok(true)
        }
        _ => Ok(false),
    }
}
