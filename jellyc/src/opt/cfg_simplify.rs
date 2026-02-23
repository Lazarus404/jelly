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

// CFG (Control Flow Graph) simplification: remove unreachable blocks and update references.
//
// This pass removes unreachable blocks from each function and updates references to reachable blocks.
//
// This pass is used to simplify the CFG and make it easier to analyze and optimize.

use crate::ir::{BlockId, IrBlock, IrFunction, IrModule, IrTerminator};
use std::collections::HashSet;

/// Remove unreachable blocks from each function.
/// Returns true if any change was made.
pub fn simplify_cfg(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        changed |= simplify_cfg_func(func);
    }

    changed
}

fn simplify_cfg_func(func: &mut IrFunction) -> bool {
    let reachable = reachable_blocks(func);

    if reachable.len() == func.blocks.len() {
        return false;
    }

    let n = func.blocks.len();
    let mut old_to_new: Vec<Option<BlockId>> = vec![None; n];
    let mut new_blocks: Vec<IrBlock> = Vec::with_capacity(reachable.len());
    let mut new_id = 0u32;
    for old_id in 0..n {
        if reachable.contains(&old_id) {
            old_to_new[old_id] = Some(BlockId(new_id));
            new_blocks.push(func.blocks[old_id].clone());
            new_id += 1;
        }
    }

    func.blocks = new_blocks;

    for block in &mut func.blocks {
        remap_terminator(&mut block.term, &old_to_new);
        for ins in &mut block.insns {
            match &mut ins.op {
                crate::ir::IrOp::Phi { incomings, .. } => {
                    incomings.retain(|(bid, _)| old_to_new[bid.0 as usize].is_some());
                    for (bid, _) in incomings {
                        *bid = old_to_new[bid.0 as usize].unwrap();
                    }
                }
                crate::ir::IrOp::Try { catch_block, .. } => {
                    if let Some(new) = old_to_new.get(catch_block.0 as usize).and_then(|o| *o) {
                        *catch_block = new;
                    }
                }
                _ => {}
            }
        }
    }

    func.entry = old_to_new[func.entry.0 as usize].unwrap_or(func.entry);

    true
}

fn reachable_blocks(func: &IrFunction) -> HashSet<usize> {
    let mut reachable = HashSet::new();
    let mut worklist = vec![func.entry.0 as usize];

    while let Some(i) = worklist.pop() {
        if !reachable.insert(i) {
            continue;
        }
        if i >= func.blocks.len() {
            continue;
        }
        for succ in terminator_successors(&func.blocks[i].term) {
            worklist.push(succ);
        }
        // Exception edges: Try jumps to catch on throw; catch block must stay reachable.
        for ins in &func.blocks[i].insns {
            if let crate::ir::IrOp::Try { catch_block, .. } = &ins.op {
                worklist.push(catch_block.0 as usize);
            }
        }
    }

    reachable
}

fn terminator_successors(term: &IrTerminator) -> Vec<usize> {
    match term {
        IrTerminator::Jmp { target } => vec![target.0 as usize],
        IrTerminator::JmpIf { then_tgt, else_tgt, .. } => {
            vec![then_tgt.0 as usize, else_tgt.0 as usize]
        }
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => vec![],
    }
}

fn remap_terminator(term: &mut IrTerminator, old_to_new: &[Option<BlockId>]) {
    match term {
        IrTerminator::Jmp { target } => {
            if let Some(new) = old_to_new.get(target.0 as usize).and_then(|o| *o) {
                *target = new;
            }
        }
        IrTerminator::JmpIf { then_tgt, else_tgt, .. } => {
            if let Some(new) = old_to_new.get(then_tgt.0 as usize).and_then(|o| *o) {
                *then_tgt = new;
            }
            if let Some(new) = old_to_new.get(else_tgt.0 as usize).and_then(|o| *o) {
                *else_tgt = new;
            }
        }
        IrTerminator::SwitchKind { cases, default, .. } => {
            for (_, b) in cases {
                if let Some(new) = old_to_new.get(b.0 as usize).and_then(|o| *o) {
                    *b = new;
                }
            }
            if let Some(new) = old_to_new.get(default.0 as usize).and_then(|o| *o) {
                *default = new;
            }
        }
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => {}
    }
}
