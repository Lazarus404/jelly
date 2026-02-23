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

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule};
use crate::jlyb::TypeKind;
use crate::typectx::T_DYNAMIC;

use super::state::RegState;

pub(super) fn check_spill_invariant(
    ir: &IrModule,
    f: &IrFunction,
    state: &RegState,
) -> Result<(), CompileError> {
    // Spill invariant: only Dynamic vregs may be spilled (spill stack is boxed-only).
    for (gv, slot) in state.vreg_to_spill.iter().enumerate() {
        if slot.is_none() {
            continue;
        }
        let tid = f.vreg_types.get(gv).copied().unwrap_or(T_DYNAMIC);
        let is_dyn = ir
            .types
            .get(tid as usize)
            .map_or(false, |te| te.kind == TypeKind::Dynamic);
        if !is_dyn {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!(
                    "spill invariant violated: spilled vreg {} has non-Dynamic tid {}",
                    gv, tid
                ),
            ));
        }
    }
    Ok(())
}

pub(super) fn check_pinned_windows_not_spilled(
    is_arg_vreg: &[bool],
    state: &RegState,
) -> Result<(), CompileError> {
    // Pinned-window invariant: vregs pinned to ABI windows (call args, closure capture args,
    // and callee capture slots) must never be spilled.
    for (i, &pinned) in is_arg_vreg.iter().enumerate() {
        if pinned && state.vreg_to_spill.get(i).is_some_and(|s| s.is_some()) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!(
                    "pinned window vreg {} was spilled (should be excluded from allocation)",
                    i
                ),
            ));
        }
    }
    Ok(())
}

pub(super) fn check_callr_callees_not_spilled(
    call_callee_vregs: &HashSet<u32>,
    state: &RegState,
) -> Result<(), CompileError> {
    // Verify CallR callees were not spilled (spill/reload can corrupt closures).
    for &gv in call_callee_vregs {
        if (gv as usize) < state.vreg_to_spill.len() && state.vreg_to_spill[gv as usize].is_some() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!(
                    "CallR callee vreg {} was spilled (spillable should prevent this)",
                    gv
                ),
            ));
        }
    }
    Ok(())
}

pub(super) fn check_typed_slot_invariant(
    f: &IrFunction,
    live_vregs: &HashSet<u32>,
    state: &RegState,
) -> Result<(), CompileError> {
    // Typed-slot invariant: a physical register never changes type.
    // Every vreg must map to a preg whose static type matches.
    for (i, &tid) in f.vreg_types.iter().enumerate() {
        if !live_vregs.contains(&(i as u32)) {
            continue; // dead vreg (eg after DCE), skip type check
        }
        let r = state.vreg_to_reg.get(i).copied().ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "vreg_to_reg out of range",
            )
        })? as usize;
        let rt = state.reg_types.get(r).copied().ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "vreg mapped to missing reg_types entry",
            )
        })?;
        if rt != tid {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!(
                    "typed slot invariant violated: vreg {} (tid={}) mapped to reg {} (tid={})",
                    i, tid, r, rt
                ),
            ));
        }
    }
    Ok(())
}
