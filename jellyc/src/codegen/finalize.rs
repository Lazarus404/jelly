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

use crate::error::{CompileError, ErrorKind};
use crate::jlyb::{Insn, Op};
use crate::regalloc::spill;

use super::{Allocation, VirtualStream};

pub(super) fn emit_final_insns(
    vs: &VirtualStream,
    alloc: &Allocation,
) -> Result<Vec<Insn>, CompileError> {
    // Defensive check: CALLR encodes arg_base as a vreg index in `imm` during the virtual stream.
    for vi in vs.vinsns.iter().filter(|vi| vi.op == Op::CallR as u8) {
        if (vi.imm as usize) >= alloc.vreg_to_reg.len() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "CALLR arg_base vreg out of range",
            ));
        }
    }

    let insns: Vec<Insn> = spill::insert_spill_ops(
        &vs.vinsns,
        &vs.infos,
        &alloc.vreg_to_reg,
        &alloc.vreg_to_spill,
        alloc.spill_reload_regs,
        &alloc.last_def_pc,
        Some(&alloc.f64_escape_per_param),
    );

    Ok(crate::peephole::peephole(&insns))
}
