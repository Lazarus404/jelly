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

use crate::jlyb::{Module, NATIVE_BUILTIN_COUNT};

use super::ctx::ValidateCtx;
use super::ops;
use super::switch_kind::validate_switch_kind_tables;

pub fn validate_module(m: &Module) -> Result<(), String> {
    // Logical index: 0..NATIVE=native, NATIVE..NATIVE+nfuncs=bytecode.
    let nfuncs_logical_max =
        NATIVE_BUILTIN_COUNT + (m.funcs.len().saturating_sub(1) as u32);

    for (fi, f) in m.funcs.iter().enumerate() {
        let nregs = f.reg_types.len() as u32;
        if nregs > 256 {
            return Err(format!("func[{fi}]: too many regs (nregs={nregs})"));
        }

        // Validate reg type ids.
        for (ri, &tid) in f.reg_types.iter().enumerate() {
            if (tid as usize) >= m.types.len() {
                return Err(format!(
                    "func[{fi}]: reg_types[{ri}] tid out of range: tid={tid} (types_len={})",
                    m.types.len()
                ));
            }
        }

        validate_switch_kind_tables(fi, &f.insns)?;

        for (pc, ins) in f.insns.iter().enumerate() {
            let ctx = ValidateCtx {
                m,
                func_i: fi,
                pc,
                reg_types: &f.reg_types,
                nregs,
                nfuncs_logical_max,
            };
            ops::validate_insn(&ctx, ins)?;
        }
    }
    Ok(())
}
