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
use crate::ir::{IrFunction, IrModule};
use crate::jlyb::{Op, TypeKind};
use crate::regalloc::spill::VInsn;
use crate::typectx::T_F64;

use super::state::RegState;

pub(super) fn apply_object_param_overlap_fix_and_f64_escape(
    ir: &IrModule,
    f: &IrFunction,
    vinsns: &[VInsn],
    state: &mut RegState,
) -> Result<(), CompileError> {
    // Fix: never map a non-Object vreg to a reg that holds an Object param.
    // (IR can produce AddF64 dst=param when param is incorrectly reused; this avoids VM validation failure.)
    for param_idx in 0..(f.param_count as usize) {
        let param_tid = f.vreg_types.get(param_idx).copied().unwrap_or(0);
        let param_is_object = ir
            .types
            .get(param_tid as usize)
            .map_or(false, |te| te.kind == TypeKind::Object);
        if !param_is_object {
            continue;
        }
        let param_reg = state.vreg_to_reg.get(param_idx).copied().unwrap_or(0);
        for (v, &tid) in f.vreg_types.iter().enumerate() {
            if state.vreg_to_reg.get(v).copied() == Some(param_reg) && tid != param_tid {
                if state.reg_types.len() >= 256 {
                    return Err(CompileError::new(
                        ErrorKind::Codegen,
                        crate::ast::Span::point(0),
                        "register allocation exceeded 256 regs (fixing param overlap)",
                    ));
                }
                state.reg_types.push(tid);
                state.vreg_to_reg[v] = (state.reg_types.len() - 1) as u8;
            }
        }
    }

    // Handle AddF64 dst=param vreg when param is Object: use escape regs (Object can't hold F64).
    for v in 0..(f.param_count as usize) {
        let param_tid = f.vreg_types.get(v).copied().unwrap_or(0);
        let param_is_object = ir
            .types
            .get(param_tid as usize)
            .map_or(false, |te| te.kind == TypeKind::Object);
        if param_is_object
            && vinsns
                .iter()
                .any(|vi| vi.op == Op::AddF64 as u8 && (vi.a as usize) == v)
        {
            if state.reg_types.len() >= 256 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs (F64 escape)",
                ));
            }
            state.reg_types.push(T_F64);
            state.f64_escape_per_param[v] = (state.reg_types.len() - 1) as u8;
        }
    }

    Ok(())
}
