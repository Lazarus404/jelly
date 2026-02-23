use std::collections::HashSet;

use crate::error::CompileError;
use crate::ir::IrModule;
use crate::regalloc::spill::VInsn;

use super::state::RegState;

pub(super) fn pin_call_arg_blocks(
    ir: &IrModule,
    call_windows: &[(u32, u32, u8)],
    call_callee_vregs: &HashSet<u32>,
    vinsns: &[VInsn],
    state: &mut RegState,
) -> Result<(), CompileError> {
    // Allocate arg blocks per signature and pin each call's arg-window vregs.
    // This is a pure ABI/layout step; we keep it separate from LSRA allocation.
    let _arg_block_start = super::super::super::pin_call_arg_blocks_and_validate(
        ir,
        call_windows,
        call_callee_vregs,
        vinsns,
        &mut state.vreg_to_reg,
        &state.vreg_to_spill,
        &mut state.reg_types,
    )?;
    Ok(())
}
