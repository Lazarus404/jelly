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

mod escape;
mod finalize;
mod invariants;
mod mark;
mod pin;
mod run;
mod setup;
mod state;

use crate::error::CompileError;
use crate::ir::{IrFunction, IrModule};

use super::super::{Allocation, VirtualStream};

pub(super) fn allocate_registers(
    ir: &IrModule,
    f: &IrFunction,
    vs: &VirtualStream,
) -> Result<Allocation, CompileError> {
    let call_windows = &vs.call_windows;
    let vinsns = &vs.vinsns;
    let infos = &vs.infos;

    let setup::Setup {
        mut state,
        call_callee_vregs,
    } = setup::setup(ir, f, vs)?;

    let marks = mark::compute_marks(ir, f, call_windows, infos, &call_callee_vregs)?;

    run::allocate_all_type_classes(
        ir,
        f,
        infos,
        &marks.tids,
        &marks.live_vregs,
        &marks.is_arg_vreg,
        &marks.allow_special_global,
        marks.cap_reserve,
        &call_callee_vregs,
        &mut state,
    )?;

    invariants::check_spill_invariant(ir, f, &state)?;
    invariants::check_pinned_windows_not_spilled(&marks.is_arg_vreg, &state)?;
    invariants::check_callr_callees_not_spilled(&call_callee_vregs, &state)?;

    pin::pin_call_arg_blocks(ir, call_windows, &call_callee_vregs, vinsns, &mut state)?;

    escape::apply_object_param_overlap_fix_and_f64_escape(ir, f, vinsns, &mut state)?;

    invariants::check_typed_slot_invariant(f, &marks.live_vregs, &state)?;

    let last_def_pc = finalize::compute_last_def_pc(f, infos);

    Ok(Allocation {
        vreg_to_reg: state.vreg_to_reg,
        vreg_to_spill: state.vreg_to_spill,
        reg_types: state.reg_types,
        cap_start: state.cap_start,
        spill_reload_regs: state.dyn_spill_reload_regs.unwrap_or([0u8, 0u8, 0u8]),
        last_def_pc,
        f64_escape_per_param: state.f64_escape_per_param,
    })
}
