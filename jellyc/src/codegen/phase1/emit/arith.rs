use crate::ir::{IrInsn, IrOp};
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::reg;

pub(super) fn emit(ins: &IrInsn, vinsns: &mut Vec<VInsn>) -> Option<()> {
    match &ins.op {
        IrOp::AddI32 { dst, a, b } => op3(vinsns, Op::AddI32, *dst, *a, *b),
        IrOp::SubI32 { dst, a, b } => op3(vinsns, Op::SubI32, *dst, *a, *b),
        IrOp::MulI32 { dst, a, b } => op3(vinsns, Op::MulI32, *dst, *a, *b),
        IrOp::NegI32 { dst, src } => op2(vinsns, Op::NegI32, *dst, *src),
        IrOp::EqI32 { dst, a, b } => op3(vinsns, Op::EqI32, *dst, *a, *b),
        IrOp::LtI32 { dst, a, b } => op3(vinsns, Op::LtI32, *dst, *a, *b),
        IrOp::AddI64 { dst, a, b } => op3(vinsns, Op::AddI64, *dst, *a, *b),
        IrOp::SubI64 { dst, a, b } => op3(vinsns, Op::SubI64, *dst, *a, *b),
        IrOp::MulI64 { dst, a, b } => op3(vinsns, Op::MulI64, *dst, *a, *b),
        IrOp::NegI64 { dst, src } => op2(vinsns, Op::NegI64, *dst, *src),
        IrOp::EqI64 { dst, a, b } => op3(vinsns, Op::EqI64, *dst, *a, *b),
        IrOp::LtI64 { dst, a, b } => op3(vinsns, Op::LtI64, *dst, *a, *b),
        IrOp::AddF16 { dst, a, b } => op3(vinsns, Op::AddF16, *dst, *a, *b),
        IrOp::SubF16 { dst, a, b } => op3(vinsns, Op::SubF16, *dst, *a, *b),
        IrOp::MulF16 { dst, a, b } => op3(vinsns, Op::MulF16, *dst, *a, *b),
        IrOp::AddF32 { dst, a, b } => op3(vinsns, Op::AddF32, *dst, *a, *b),
        IrOp::SubF32 { dst, a, b } => op3(vinsns, Op::SubF32, *dst, *a, *b),
        IrOp::MulF32 { dst, a, b } => op3(vinsns, Op::MulF32, *dst, *a, *b),
        IrOp::DivF32 { dst, a, b } => op3(vinsns, Op::DivF32, *dst, *a, *b),
        IrOp::NegF32 { dst, src } => op2(vinsns, Op::NegF32, *dst, *src),
        IrOp::EqF32 { dst, a, b } => op3(vinsns, Op::EqF32, *dst, *a, *b),
        IrOp::LtF32 { dst, a, b } => op3(vinsns, Op::LtF32, *dst, *a, *b),
        IrOp::AddF64 { dst, a, b } => op3(vinsns, Op::AddF64, *dst, *a, *b),
        IrOp::SubF64 { dst, a, b } => op3(vinsns, Op::SubF64, *dst, *a, *b),
        IrOp::MulF64 { dst, a, b } => op3(vinsns, Op::MulF64, *dst, *a, *b),
        IrOp::DivF64 { dst, a, b } => op3(vinsns, Op::DivF64, *dst, *a, *b),
        IrOp::NegF64 { dst, src } => op2(vinsns, Op::NegF64, *dst, *src),
        IrOp::EqF64 { dst, a, b } => op3(vinsns, Op::EqF64, *dst, *a, *b),
        IrOp::LtF64 { dst, a, b } => op3(vinsns, Op::LtF64, *dst, *a, *b),
        IrOp::Physeq { dst, a, b } => op3(vinsns, Op::Physeq, *dst, *a, *b),
        IrOp::NotBool { dst, src } => op2(vinsns, Op::NotBool, *dst, *src),
        IrOp::Kindof { dst, src } => op2(vinsns, Op::Kindof, *dst, *src),
        IrOp::Assert { cond } => {
            vinsns.push(VInsn {
                op: Op::Assert as u8,
                a: reg(*cond),
                b: 0,
                c: 0,
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
