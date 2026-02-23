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
        IrOp::BytesNew { dst, len } => op2(vinsns, Op::BytesNew, *dst, *len),
        IrOp::BytesLen { dst, bytes } => op2(vinsns, Op::BytesLen, *dst, *bytes),
        IrOp::BytesGetU8 { dst, bytes, index } => op3(vinsns, Op::BytesGetU8, *dst, *bytes, *index),
        IrOp::BytesSetU8 {
            bytes,
            index,
            value,
        } => {
            vinsns.push(VInsn {
                op: Op::BytesSetU8 as u8,
                a: reg(*value),
                b: reg(*bytes),
                c: reg(*index),
                imm: 0,
            });
            Some(())
        }
        IrOp::BytesConcat2 { dst, a, b } => op3(vinsns, Op::BytesConcat2, *dst, *a, *b),
        IrOp::BytesConcatMany { dst, parts } => op2(vinsns, Op::BytesConcatMany, *dst, *parts),

        IrOp::ListNil { dst } => {
            vinsns.push(VInsn {
                op: Op::ListNil as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: 0,
            });
            Some(())
        }
        IrOp::ListCons { dst, head, tail } => op3(vinsns, Op::ListCons, *dst, *head, *tail),
        IrOp::ListHead { dst, list } => op2(vinsns, Op::ListHead, *dst, *list),
        IrOp::ListTail { dst, list } => op2(vinsns, Op::ListTail, *dst, *list),
        IrOp::ListIsNil { dst, list } => op2(vinsns, Op::ListIsNil, *dst, *list),

        IrOp::ArrayNew { dst, len } => op2(vinsns, Op::ArrayNew, *dst, *len),
        IrOp::ArrayLen { dst, arr } => op2(vinsns, Op::ArrayLen, *dst, *arr),
        IrOp::ArrayGet { dst, arr, index } => op3(vinsns, Op::ArrayGet, *dst, *arr, *index),
        IrOp::ArraySet { arr, index, value } => {
            vinsns.push(VInsn {
                op: Op::ArraySet as u8,
                a: reg(*value),
                b: reg(*arr),
                c: reg(*index),
                imm: 0,
            });
            Some(())
        }

        IrOp::ObjNew { dst } => {
            vinsns.push(VInsn {
                op: Op::ObjNew as u8,
                a: reg(*dst),
                b: 0,
                c: 0,
                imm: 0,
            });
            Some(())
        }
        IrOp::ObjHasAtom { dst, obj, atom_id } => {
            vinsns.push(VInsn {
                op: Op::ObjHasAtom as u8,
                a: reg(*dst),
                b: reg(*obj),
                c: 0,
                imm: *atom_id,
            });
            Some(())
        }
        IrOp::ObjGetAtom { dst, obj, atom_id } => {
            vinsns.push(VInsn {
                op: Op::ObjGetAtom as u8,
                a: reg(*dst),
                b: reg(*obj),
                c: 0,
                imm: *atom_id,
            });
            Some(())
        }
        IrOp::ObjSetAtom {
            obj,
            atom_id,
            value,
        } => {
            vinsns.push(VInsn {
                op: Op::ObjSetAtom as u8,
                a: reg(*value),
                b: reg(*obj),
                c: 0,
                imm: *atom_id,
            });
            Some(())
        }
        IrOp::ObjGet { dst, obj, atom } => op3(vinsns, Op::ObjGet, *dst, *obj, *atom),
        IrOp::ObjSet { obj, atom, value } => {
            vinsns.push(VInsn {
                op: Op::ObjSet as u8,
                a: reg(*value),
                b: reg(*obj),
                c: reg(*atom),
                imm: 0,
            });
            Some(())
        }
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

fn op3(
    vinsns: &mut Vec<VInsn>,
    op: Op,
    a: crate::ir::VRegId,
    b: crate::ir::VRegId,
    c: crate::ir::VRegId,
) -> Option<()> {
    vinsns.push(VInsn {
        op: op as u8,
        a: reg(a),
        b: reg(b),
        c: reg(c),
        imm: 0,
    });
    Some(())
}
