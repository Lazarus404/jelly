use crate::ir::{IrInsn, IrOp};
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::reg;

pub(super) fn emit(ins: &IrInsn, vinsns: &mut Vec<VInsn>) -> Option<()> {
    match &ins.op {
        IrOp::SextI64 { dst, src } => op2(vinsns, Op::SextI64, *dst, *src),
        IrOp::SextI16 { dst, src } => op2(vinsns, Op::SextI16, *dst, *src),
        IrOp::TruncI8 { dst, src } => op2(vinsns, Op::TruncI8, *dst, *src),
        IrOp::TruncI16 { dst, src } => op2(vinsns, Op::TruncI16, *dst, *src),
        IrOp::F16FromF32 { dst, src } => op2(vinsns, Op::F16FromF32, *dst, *src),
        IrOp::F32FromF16 { dst, src } => op2(vinsns, Op::F32FromF16, *dst, *src),
        IrOp::F32FromI32 { dst, src } => op2(vinsns, Op::F32FromI32, *dst, *src),
        IrOp::F64FromI32 { dst, src } => op2(vinsns, Op::F64FromI32, *dst, *src),
        IrOp::F64FromI64 { dst, src } => op2(vinsns, Op::F64FromI64, *dst, *src),
        IrOp::F64FromF32 { dst, src } => op2(vinsns, Op::F64FromF32, *dst, *src),
        IrOp::F32FromF64 { dst, src } => op2(vinsns, Op::F32FromF64, *dst, *src),
        IrOp::F32FromI64 { dst, src } => op2(vinsns, Op::F32FromI64, *dst, *src),
        IrOp::I64FromF32 { dst, src } => op2(vinsns, Op::I64FromF32, *dst, *src),
        IrOp::I32FromI64 { dst, src } => op2(vinsns, Op::I32FromI64, *dst, *src),
        IrOp::I32FromF64 { dst, src } => op2(vinsns, Op::I32FromF64, *dst, *src),
        IrOp::I64FromF64 { dst, src } => op2(vinsns, Op::I64FromF64, *dst, *src),
        IrOp::I32FromF32 { dst, src } => op2(vinsns, Op::I32FromF32, *dst, *src),
        IrOp::F16FromI32 { dst, src } => op2(vinsns, Op::F16FromI32, *dst, *src),
        IrOp::I32FromF16 { dst, src } => op2(vinsns, Op::I32FromF16, *dst, *src),
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
