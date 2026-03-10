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

use crate::ir::{IrFunction, IrOp, IrTerminator};

use super::cfg::term_succs;

/// Vregs that must not be SSA-renamed.
///
/// Includes:
/// - implicit windows (Call args, Closure captures)
/// - any vreg used in catch regions (phi-moves can't run on exception edges)
pub(super) fn compute_pinned(func: &IrFunction) -> Vec<bool> {
    let nb = func.blocks.len();

    // Pin vregs that belong to implicit windows (Call args, Closure captures).
    let mut pinned: Vec<bool> = vec![false; func.vreg_types.len()];
    for blk in &func.blocks {
        for ins in &blk.insns {
            match ins.op {
                IrOp::Call {
                    arg_base, nargs, ..
                } => {
                    for i in 0..(nargs as u32) {
                        let vi = (arg_base.0 + i) as usize;
                        if vi < pinned.len() {
                            pinned[vi] = true;
                        }
                    }
                }
                IrOp::Closure {
                    cap_base, ncaps, ..
                } => {
                    for i in 0..(ncaps as u32) {
                        let vi = (cap_base.0 + i) as usize;
                        if vi < pinned.len() {
                            pinned[vi] = true;
                        }
                    }
                }
                _ => {}
            }
        }
    }

    // Also pin vregs used in the catch region(s).
    //
    // Rationale: the VM transfers control to `catch_block` without executing any IR-level moves.
    // If we SSA-rename a variable that's read in catch, we'd need phi-moves on exception edges
    // to select the "current" version; we don't have a place to run those moves. Pinning keeps
    // these variables in stable vregs across the try region.
    let mut catch_roots: Vec<usize> = Vec::new();
    for blk in &func.blocks {
        for ins in &blk.insns {
            if let IrOp::Try { catch_block, .. } = ins.op {
                catch_roots.push(catch_block.0 as usize);
            }
        }
    }
    if catch_roots.is_empty() {
        return pinned;
    }

    catch_roots.sort_unstable();
    catch_roots.dedup();

    let mut in_catch_region: Vec<bool> = vec![false; nb];
    let mut work: Vec<usize> = catch_roots;
    while let Some(bi) = work.pop() {
        if bi >= nb || in_catch_region[bi] {
            continue;
        }
        in_catch_region[bi] = true;
        for s in term_succs(&func.blocks[bi].term) {
            work.push(s);
        }
    }

    for (bi, blk) in func.blocks.iter().enumerate() {
        if !in_catch_region[bi] {
            continue;
        }
        for ins in &blk.insns {
            for v in ins.op.uses() {
                let vi = v.0 as usize;
                if vi < pinned.len() {
                    pinned[vi] = true;
                }
            }
        }

        match &blk.term {
            IrTerminator::Ret { value } => {
                let vi = value.0 as usize;
                if vi < pinned.len() {
                    pinned[vi] = true;
                }
            }
            IrTerminator::TailCall {
                callee,
                arg_base,
                nargs,
                ..
            } => {
                let vi = callee.0 as usize;
                if vi < pinned.len() {
                    pinned[vi] = true;
                }
                for i in 0..(*nargs as u32) {
                    let vi = (arg_base.0 + i) as usize;
                    if vi < pinned.len() {
                        pinned[vi] = true;
                    }
                }
            }
            IrTerminator::JmpIf { cond, .. } => {
                let vi = cond.0 as usize;
                if vi < pinned.len() {
                    pinned[vi] = true;
                }
            }
            IrTerminator::SwitchKind { kind, .. } => {
                let vi = kind.0 as usize;
                if vi < pinned.len() {
                    pinned[vi] = true;
                }
            }
            IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
        }
    }

    pinned
}
