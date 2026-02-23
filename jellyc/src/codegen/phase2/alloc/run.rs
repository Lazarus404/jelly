use std::collections::HashSet;

use crate::error::CompileError;
use crate::ir::{IrFunction, IrModule, TypeId};
use crate::regalloc::InstrInfo;

use super::super::type_class::allocate_type_class;
use super::state::RegState;

pub(super) fn allocate_all_type_classes(
    ir: &IrModule,
    f: &IrFunction,
    infos: &[InstrInfo],
    tids: &[TypeId],
    live_vregs: &HashSet<u32>,
    is_arg_vreg: &[bool],
    allow_special_global: &[bool],
    cap_reserve: u16,
    call_callee_vregs: &HashSet<u32>,
    state: &mut RegState,
) -> Result<(), CompileError> {
    for &tid in tids {
        let mut globals: Vec<u32> = Vec::new();
        for (i, &t) in f.vreg_types.iter().enumerate() {
            if i >= (f.param_count as usize)
                && !is_arg_vreg[i]
                && t == tid
                && live_vregs.contains(&(i as u32))
            {
                globals.push(i as u32);
            }
        }
        if globals.is_empty() {
            continue;
        }

        allocate_type_class(
            ir,
            f,
            infos,
            tid,
            &globals,
            // This slice serves two purposes:
            // - permit vregs with multiple defs (non-SSA) to be allocated
            // - permit a small set of known-implicit live-ins to be treated as defined at entry
            allow_special_global,
            &mut state.base,
            cap_reserve,
            call_callee_vregs,
            &mut state.dyn_spill_reload_regs,
            &mut state.vreg_to_reg,
            &mut state.vreg_to_spill,
            &mut state.reg_types,
        )?;
    }
    Ok(())
}
