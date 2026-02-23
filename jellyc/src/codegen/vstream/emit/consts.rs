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

use crate::ir::{IrInsn, IrOp};
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::reg;

pub(super) fn emit(
    map_func_index: &impl Fn(u32) -> u32,
    ins: &IrInsn,
    vinsns: &mut Vec<VInsn>,
) -> Option<()> {
    match &ins.op {
        IrOp::ConstBytes { dst, pool_index } => {
            vinsns.push(VInsn {
                op: Op::ConstBytes as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *pool_index,
            });
            Some(())
        }
        IrOp::ConstI64 { dst, pool_index } => {
            vinsns.push(VInsn {
                op: Op::ConstI64 as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *pool_index,
            });
            Some(())
        }
        IrOp::ConstBool { dst, imm } => {
            vinsns.push(VInsn {
                op: Op::ConstBool as u8,
                a: reg(*dst),
                b: 0,
                c: if *imm { 1 } else { 0 },
                imm: 0,
            });
            Some(())
        }
        IrOp::ConstI32 { dst, imm } => {
            vinsns.push(VInsn {
                op: Op::ConstI32 as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *imm as u32,
            });
            Some(())
        }
        IrOp::ConstF32 { dst, bits } => {
            vinsns.push(VInsn {
                op: Op::ConstF32 as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *bits,
            });
            Some(())
        }
        IrOp::ConstI8Imm { dst, imm } => {
            vinsns.push(VInsn {
                op: Op::ConstI8Imm as u8,
                a: reg(*dst),
                b: 0,
                c: *imm as u32,
                imm: 0,
            });
            Some(())
        }
        IrOp::ConstF16 { dst, bits } => {
            vinsns.push(VInsn {
                op: Op::ConstF16 as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *bits as u32,
            });
            Some(())
        }
        IrOp::ConstF64 { dst, pool_index } => {
            vinsns.push(VInsn {
                op: Op::ConstF64 as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *pool_index,
            });
            Some(())
        }
        IrOp::ConstNull { dst } => {
            vinsns.push(VInsn {
                op: Op::ConstNull as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: 0,
            });
            Some(())
        }
        IrOp::ConstAtom { dst, atom_id } => {
            vinsns.push(VInsn {
                op: Op::ConstAtom as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: *atom_id,
            });
            Some(())
        }
        IrOp::ConstFun { dst, func_index } => {
            vinsns.push(VInsn {
                op: Op::ConstFun as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: map_func_index(*func_index),
            });
            Some(())
        }
        _ => None,
    }
}
