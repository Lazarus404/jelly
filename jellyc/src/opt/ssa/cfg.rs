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

pub(super) fn term_succs(term: &IrTerminator) -> Vec<usize> {
    match term {
        IrTerminator::Jmp { target } => vec![target.0 as usize],
        IrTerminator::JmpIf {
            then_tgt, else_tgt, ..
        } => vec![then_tgt.0 as usize, else_tgt.0 as usize],
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => vec![],
    }
}

/// Terminator edges + conservative `Try` → `catch_block` edges.
pub(super) fn build_cfg(func: &IrFunction) -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
    let nb = func.blocks.len();
    let mut succs: Vec<Vec<usize>> = vec![Vec::new(); nb];
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nb];

    for (bi, blk) in func.blocks.iter().enumerate() {
        for s in term_succs(&blk.term) {
            if s < nb {
                succs[bi].push(s);
                preds[s].push(bi);
            }
        }
        for ins in &blk.insns {
            if let IrOp::Try { catch_block, .. } = ins.op {
                let s = catch_block.0 as usize;
                if s < nb {
                    succs[bi].push(s);
                    preds[s].push(bi);
                }
            }
        }
    }

    for s in &mut succs {
        s.sort_unstable();
        s.dedup();
    }
    for p in &mut preds {
        p.sort_unstable();
        p.dedup();
    }

    (succs, preds)
}
