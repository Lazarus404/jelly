use crate::ir::IrOp;
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::{blk_pc, delta, reg};
use super::EmitState;

pub(super) fn emit(
    block_start: &[u32],
    this_pc: u32,
    op: &IrOp,
    st: &mut EmitState,
) -> Result<bool, crate::error::CompileError> {
    match op {
        IrOp::Throw { payload } => st.vinsns.push(VInsn {
            op: Op::Throw as u8,
            a: reg(*payload),
            b: 0,
            c: 0,
            imm: 0,
        }),
        IrOp::Try {
            catch_dst,
            catch_block,
            trap_only,
        } => {
            let catch_pc = blk_pc(block_start, *catch_block)?;
            st.vinsns.push(VInsn {
                op: Op::Try as u8,
                a: reg(*catch_dst),
                b: if *trap_only { 1 } else { 0 },
                c: 0,
                imm: delta(this_pc, catch_pc),
            });
        }
        IrOp::EndTry => st.vinsns.push(VInsn {
            op: Op::EndTry as u8,
            a: 0,
            b: 0,
            c: 0,
            imm: 0,
        }),
        _ => return Ok(false),
    }
    Ok(true)
}
