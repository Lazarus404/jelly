use crate::error::CompileError;
use crate::ir::{IrInsn, IrOp};
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::{blk_pc, delta, reg};

pub(super) fn emit(
    block_start: &[u32],
    ins: &IrInsn,
    this_pc: u32,
    vinsns: &mut Vec<VInsn>,
) -> Result<bool, CompileError> {
    match &ins.op {
        IrOp::Throw { payload } => {
            vinsns.push(VInsn {
                op: Op::Throw as u8,
                a: reg(*payload),
                b: 0,
                c: 0,
                imm: 0,
            });
            Ok(true)
        }
        IrOp::Try {
            catch_dst,
            catch_block,
            trap_only,
        } => {
            let catch_pc = blk_pc(block_start, *catch_block)?;
            vinsns.push(VInsn {
                op: Op::Try as u8,
                a: reg(*catch_dst),
                b: if *trap_only { 1 } else { 0 },
                c: 0,
                imm: delta(this_pc, catch_pc),
            });
            Ok(true)
        }
        IrOp::EndTry => {
            vinsns.push(VInsn {
                op: Op::EndTry as u8,
                a: 0,
                b: 0,
                c: 0,
                imm: 0,
            });
            Ok(true)
        }
        _ => Ok(false),
    }
}
