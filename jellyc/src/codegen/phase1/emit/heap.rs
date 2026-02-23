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
