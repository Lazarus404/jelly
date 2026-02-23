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

pub(super) fn validate_call_windows(
    ir: &IrModule,
    f: &IrFunction,
    call_windows: &[(u32, u32, u8)],
) -> Result<(), CompileError> {
    for &(sig_id, arg_base_v, nargs) in call_windows {
        let sig = ir.sigs.get(sig_id as usize).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "bad fun sig id",
            )
        })?;
        if sig.args.len() != (nargs as usize) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "CALLR arg count does not match signature",
            ));
        }
        for i in 0..(nargs as u32) {
            let vi = (arg_base_v + i) as usize;
            if vi >= f.vreg_types.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "CALLR arg vreg out of range",
                ));
            }
            let vt = f.vreg_types[vi];
            let st = sig.args[i as usize];
            if vt != st {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "CALLR arg window type mismatch (vreg {}: {} != {})",
                        vi, vt, st
                    ),
                ));
            }
        }
    }
    Ok(())
}
