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

use crate::jlyb::{Module, Op, NATIVE_BUILTIN_COUNT};

use super::entry::validate_module;
use super::helpers::err;

/// Stricter validation for compiler-emitted modules.
///
/// This runs the standard bytecode validator and then checks additional invariants that are
/// important for the typed-register VM contract but are not encoded directly in the ISA:
///
/// - For direct `CALL`, the caller arg window types must exactly match the callee's param register
///   types (`callee.reg_types[0..nargs)`), for bytecode callees in this module (native builtins are
///   excluded from this check).
#[cfg_attr(not(test), allow(dead_code))]
pub fn validate_module_strict(m: &Module) -> Result<(), String> {
    validate_module(m)?;

    // Logical indices: 0..NATIVE_BUILTIN_COUNT = native builtins, then 1..nfuncs = bytecode funcs.
    let nfuncs_logical_max = m.funcs.len() as u32;

    for (caller_fi, f) in m.funcs.iter().enumerate() {
        let nregs = f.reg_types.len();
        for (pc, ins) in f.insns.iter().enumerate() {
            let is_call = ins.op == Op::Call as u8 || ins.op == Op::TailCall as u8;
            if !is_call {
                continue;
            }
            // Skip native builtins (not represented in `m.funcs`).
            if ins.imm < NATIVE_BUILTIN_COUNT {
                continue;
            }
            if ins.imm == 0 || ins.imm > nfuncs_logical_max {
                continue; // already rejected by validate_module, but keep defensive
            }
            let callee_idx = (ins.imm - NATIVE_BUILTIN_COUNT) as usize;
            let callee = m
                .funcs
                .get(callee_idx)
                .ok_or_else(|| err(caller_fi, pc, "call callee func index out of range"))?;

            let nargs = ins.c as usize;
            if nargs > callee.reg_types.len() {
                return Err(err(
                    caller_fi,
                    pc,
                    "call nargs exceeds callee reg file (bad callee ABI)",
                ));
            }

            let first = ins.b as usize;
            if first + nargs > nregs {
                return Err(err(caller_fi, pc, "call arg range out of range"));
            }

            for i in 0..nargs {
                let caller_tid = *f
                    .reg_types
                    .get(first + i)
                    .ok_or_else(|| err(caller_fi, pc, "call arg reg out of range"))?;
                let callee_tid = callee.reg_types[i];
                if caller_tid != callee_tid {
                    return Err(err(
                        caller_fi,
                        pc,
                        &format!(
                            "call arg type mismatch at arg {i}: caller tid={caller_tid} callee tid={callee_tid}"
                        ),
                    ));
                }
            }
        }
    }

    Ok(())
}
