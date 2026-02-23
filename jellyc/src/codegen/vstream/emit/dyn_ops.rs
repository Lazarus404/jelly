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

pub(super) fn emit(ins: &IrInsn, vinsns: &mut Vec<VInsn>) -> Option<()> {
    match &ins.op {
        IrOp::ToDyn { dst, src } => op2(vinsns, Op::ToDyn, *dst, *src),
        IrOp::FromDynI8 { dst, src } => op2(vinsns, Op::FromDynI8, *dst, *src),
        IrOp::FromDynI16 { dst, src } => op2(vinsns, Op::FromDynI16, *dst, *src),
        IrOp::FromDynI32 { dst, src } => op2(vinsns, Op::FromDynI32, *dst, *src),
        IrOp::FromDynI64 { dst, src } => op2(vinsns, Op::FromDynI64, *dst, *src),
        IrOp::FromDynF16 { dst, src } => op2(vinsns, Op::FromDynF16, *dst, *src),
        IrOp::FromDynF32 { dst, src } => op2(vinsns, Op::FromDynF32, *dst, *src),
        IrOp::FromDynF64 { dst, src } => op2(vinsns, Op::FromDynF64, *dst, *src),
        IrOp::FromDynBool { dst, src } => op2(vinsns, Op::FromDynBool, *dst, *src),
        IrOp::FromDynPtr { dst, src } => op2(vinsns, Op::FromDynPtr, *dst, *src),
        _ => None,
    }
}

fn op2(vinsns: &mut Vec<VInsn>, op: Op, a: crate::ir::VRegId, b: crate::ir::VRegId) -> Option<()> {
    vinsns.push(VInsn {
        op: op as u8,
        a: reg(a),
        b: reg(b),
        c: 0,
        imm: 0,
    });
    Some(())
}
