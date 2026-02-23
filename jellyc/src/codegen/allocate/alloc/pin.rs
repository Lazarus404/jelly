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
