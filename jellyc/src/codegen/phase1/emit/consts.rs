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
