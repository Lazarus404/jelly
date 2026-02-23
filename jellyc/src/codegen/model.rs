/**
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

use crate::ir::VRegId;
use crate::regalloc::spill::VInsn;
use crate::regalloc::InstrInfo;

pub(super) fn reg(v: VRegId) -> u32 {
    v.0
}

pub(super) struct VirtualStream {
    // Virtual instruction stream; operands are global vreg indices (u32).
    pub(super) vinsns: Vec<VInsn>,
    // Per-instruction uses/defs for regalloc/spill insertion; global vreg indices.
    pub(super) infos: Vec<InstrInfo>,
    // (sig_id, arg_base_vreg, nargs): used to pin arg windows to ABI arg blocks.
    pub(super) call_windows: Vec<(u32, u32, u8)>,
}

pub(super) struct Allocation {
    pub(super) vreg_to_reg: Vec<u8>,
    pub(super) vreg_to_spill: Vec<Option<u32>>,
    pub(super) reg_types: Vec<u32>,
    pub(super) cap_start: u32,
    pub(super) spill_reload_regs: [u8; 3],
    pub(super) last_def_pc: Vec<u32>,
    pub(super) f64_escape_per_param: Vec<u8>,
}
