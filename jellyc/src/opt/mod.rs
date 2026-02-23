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

// Mid-end optimization passes on SSA IR (with Phi nodes).
// Runs after lowering, before phi elimination and emission.

mod cfg_simplify;
mod boundary_simplify;
mod boundary_fold;
mod copy_prop;
mod phi_prune;
mod phi_simplify;
mod gvn;
mod ssa;
mod const_prop;
mod cse;
mod dce;

use crate::error::CompileError;
use crate::ir::IrModule;

/// Run all optimization passes on the IR module.
/// Pass order (iterated until fixpoint):
/// cfg_simplify → phi_prune → boundary_simplify → copy_prop → phi_simplify → GVN → DCE → CSE → const_prop.
pub fn run_passes(m: &mut IrModule) -> Result<(), CompileError> {
    // Do one-time cleanup/normalization before the main fixpoint loop.
    cfg_simplify::simplify_cfg(m);
    ssa::convert_to_ssa(m);

    let mut changed = true;
    while changed {
        changed = false;
        changed |= cfg_simplify::simplify_cfg(m);
        changed |= phi_prune::prune_phi_incomings(m);
        changed |= boundary_simplify::simplify_dynamic_boundaries(m);
        changed |= boundary_fold::fold_local_dynamic_boundaries(m);
        changed |= copy_prop::copy_propagation(m);
        changed |= phi_simplify::simplify_phis(m);
        changed |= gvn::global_value_numbering(m);
        changed |= dce::dead_code_elimination(m);
        changed |= cse::common_subexpression_elimination(m);
        changed |= const_prop::constant_propagation(m);
    }
    Ok(())
}

/// Run a small cleanup pipeline after phi elimination.
///
/// Phi elimination introduces parallel-move sequences and temporary copies. This pass reduces
/// redundant `Mov`s and cascades simple cleanups before codegen, without requiring SSA.
pub fn run_post_phi_cleanup(m: &mut IrModule) {
    // Keep this loop small; these are all cheap passes and the IR is already "late".
    let mut changed = true;
    for _ in 0..8 {
        if !changed {
            break;
        }
        changed = false;
        changed |= cfg_simplify::simplify_cfg(m);
        changed |= boundary_simplify::simplify_dynamic_boundaries(m);
        changed |= boundary_fold::fold_local_dynamic_boundaries(m);
        changed |= copy_prop::copy_propagation(m);
        changed |= dce::dead_code_elimination(m);
        changed |= cse::common_subexpression_elimination(m);
        changed |= const_prop::constant_propagation(m);
    }
}

