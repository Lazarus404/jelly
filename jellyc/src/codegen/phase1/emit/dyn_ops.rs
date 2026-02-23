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
