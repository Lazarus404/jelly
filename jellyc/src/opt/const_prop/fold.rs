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

use crate::ir::{IrOp, TypeId, VRegId};
use crate::typectx::{T_I32, T_I8};

pub(super) fn fold_int_result(dst: VRegId, imm: i32, vreg_types: &[TypeId]) -> IrOp {
    let dst_tid = vreg_types.get(dst.0 as usize).copied().unwrap_or(T_I32);
    if dst_tid == T_I8 && imm >= -128 && imm <= 127 {
        IrOp::ConstI8Imm {
            dst,
            imm: imm as i8 as u8,
        }
    } else {
        IrOp::ConstI32 { dst, imm }
    }
}

pub(super) fn same_rewriteable_op(a: &IrOp, b: &IrOp) -> bool {
    use IrOp::*;
    match (a, b) {
        (ConstI32 { dst: ad, imm: ai }, ConstI32 { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (ConstI8Imm { dst: ad, imm: ai }, ConstI8Imm { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (ConstF16 { dst: ad, bits: ab }, ConstF16 { dst: bd, bits: bb }) => ad == bd && ab == bb,
        (ConstBool { dst: ad, imm: ai }, ConstBool { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (Mov { dst: ad, src: asrc }, Mov { dst: bd, src: bsrc }) => ad == bd && asrc == bsrc,
        _ => false,
    }
}
