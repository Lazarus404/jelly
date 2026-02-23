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
use crate::ir::{IrFunction, IrInsn, IrModule, IrOp};
use crate::jlyb::{Op, TypeKind};
use crate::regalloc::spill::VInsn;

use super::super::super::reg;

pub(super) fn emit(
    ir: &IrModule,
    f: &IrFunction,
    ins: &IrInsn,
    vinsns: &mut Vec<VInsn>,
) -> Result<bool, CompileError> {
    let IrOp::Mov { dst, src } = &ins.op else {
        return Ok(false);
    };

    // VM rejects Mov when src/dst have different kinds or slot sizes.
    // Emit FromDyn* for Dynamic->primitive; emit conversion ops for prim->prim.
    let src_tid = f.vreg_types.get(src.0 as usize).copied();
    let dst_tid = f.vreg_types.get(dst.0 as usize).copied();
    let op = match (src_tid, dst_tid) {
        (Some(st), Some(dt)) => {
            let src_kind = ir.types.get(st as usize).map(|te| te.kind);
            let dst_kind = ir.types.get(dt as usize).map(|te| te.kind);
            match (src_kind, dst_kind) {
                (Some(TypeKind::Dynamic), Some(dk)) if dk != TypeKind::Dynamic => match dk {
                    TypeKind::I8 => Op::FromDynI8,
                    TypeKind::I16 => Op::FromDynI16,
                    TypeKind::I32 => Op::FromDynI32,
                    TypeKind::I64 => Op::FromDynI64,
                    TypeKind::F16 => Op::FromDynF16,
                    TypeKind::F32 => Op::FromDynF32,
                    TypeKind::F64 => Op::FromDynF64,
                    TypeKind::Bool => Op::FromDynBool,
                    _ => Op::Mov,
                },
                (Some(sk), Some(dk)) if sk != dk => match (sk, dk) {
                    (TypeKind::F32, TypeKind::F64) => Op::F64FromF32,
                    (TypeKind::F64, TypeKind::F32) => Op::F32FromF64,
                    (TypeKind::F16, TypeKind::F32) => Op::F32FromF16,
                    (TypeKind::F32, TypeKind::F16) => Op::F16FromF32,
                    (TypeKind::I32, TypeKind::F32) => Op::F32FromI32,
                    (TypeKind::I32, TypeKind::F64) => Op::F64FromI32,
                    (TypeKind::I64, TypeKind::F32) => Op::F32FromI64,
                    (TypeKind::I64, TypeKind::F64) => Op::F64FromI64,
                    (TypeKind::F32, TypeKind::I32) => Op::I32FromF32,
                    (TypeKind::F32, TypeKind::I64) => Op::I64FromF32,
                    (TypeKind::F64, TypeKind::I32) => Op::I32FromF64,
                    (TypeKind::F64, TypeKind::I64) => Op::I64FromF64,
                    (TypeKind::I64, TypeKind::I32) => Op::I32FromI64,
                    (TypeKind::I32, TypeKind::I64) => Op::SextI64,
                    _ => Op::Mov,
                },
                _ => Op::Mov,
            }
        }
        _ => Op::Mov,
    };
    vinsns.push(VInsn {
        op: op as u8,
        a: reg(*dst),
        b: reg(*src),
        c: 0,
        imm: 0,
    });
    Ok(true)
}
