use std::collections::HashSet;

use crate::ir::{BlockId, IrFunction, IrOp, IrTerminator};

pub(super) fn succ_count(term: &IrTerminator) -> usize {
    match term {
        IrTerminator::Jmp { .. } => 1,
        IrTerminator::JmpIf { .. } => 2,
        IrTerminator::SwitchKind { cases, .. } => cases.len() + 1,
        IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => 0,
    }
}

pub(super) fn rewrite_edge_target(term: &mut IrTerminator, old: BlockId, new: BlockId) -> bool {
    match term {
        IrTerminator::Jmp { target } => {
            if *target == old {
                *target = new;
                true
            } else {
                false
            }
        }
        IrTerminator::JmpIf {
            then_tgt, else_tgt, ..
        } => {
            let mut changed = false;
            if *then_tgt == old {
                *then_tgt = new;
                changed = true;
            }
            if *else_tgt == old {
                *else_tgt = new;
                changed = true;
            }
            changed
        }
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut changed = false;
            if *default == old {
                *default = new;
                changed = true;
            }
            for (_k, tgt) in cases.iter_mut() {
                if *tgt == old {
                    *tgt = new;
                    changed = true;
                }
            }
            changed
        }
        IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => false,
    }
}

fn terminator_successors(term: &IrTerminator) -> Vec<usize> {
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
            if let IrOp::Try { catch_block, .. } = &ins.op {
                worklist.push(catch_block.0 as usize);
            }
        }
    }

    reachable
}

fn remap_terminator(term: &mut IrTerminator, old_to_new: &[Option<BlockId>]) {
    match term {
        IrTerminator::Jmp { target } => {
            if let Some(new) = old_to_new.get(target.0 as usize).and_then(|o| *o) {
                *target = new;
            }
        }
        IrTerminator::JmpIf {
            then_tgt, else_tgt, ..
        } => {
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
        IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => {}
    }
}

/// After phi elimination, shrink away empty jump-only blocks (typically edge-split blocks
/// that ended up with no scheduled phi moves).
pub(super) fn shrink_empty_jmp_blocks(func: &mut IrFunction) {
    let n = func.blocks.len();
    if n == 0 {
        return;
    }

    // Compute redirect targets for empty jump-only blocks.
    let mut redirect: Vec<Option<BlockId>> = vec![None; n];
    for i in 0..n {
        if func.blocks[i].insns.is_empty() {
            if let IrTerminator::Jmp { target } = func.blocks[i].term {
                if target.0 as usize != i {
                    redirect[i] = Some(target);
                }
            }
        }
    }
    // Chase chains.
    for i in 0..n {
        let mut cur = redirect[i];
        let mut steps = 0;
        while let Some(b) = cur {
            if steps > n {
                break;
            }
            let bi = b.0 as usize;
            if bi >= n {
                break;
            }
            if let Some(next) = redirect[bi] {
                cur = Some(next);
                steps += 1;
                continue;
            }
            break;
        }
        redirect[i] = cur;
    }

    let mut changed = true;
    while changed {
        changed = false;
        for blk in &mut func.blocks {
            // Rewrite terminator edges.
            match &mut blk.term {
                IrTerminator::Jmp { target } => {
                    if let Some(t2) = redirect.get(target.0 as usize).and_then(|x| *x) {
                        if t2 != *target {
                            *target = t2;
                            changed = true;
                        }
                    }
                }
                IrTerminator::JmpIf {
                    then_tgt, else_tgt, ..
                } => {
                    for tgt in [then_tgt, else_tgt] {
                        if let Some(t2) = redirect.get(tgt.0 as usize).and_then(|x| *x) {
                            if t2 != *tgt {
                                *tgt = t2;
                                changed = true;
                            }
                        }
                    }
                }
                IrTerminator::SwitchKind { cases, default, .. } => {
                    if let Some(t2) = redirect.get(default.0 as usize).and_then(|x| *x) {
                        if t2 != *default {
                            *default = t2;
                            changed = true;
                        }
                    }
                    for (_k, tgt) in cases.iter_mut() {
                        if let Some(t2) = redirect.get(tgt.0 as usize).and_then(|x| *x) {
                            if t2 != *tgt {
                                *tgt = t2;
                                changed = true;
                            }
                        }
                    }
                }
                IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => {}
            }

            // Rewrite try catch_block pointers.
            for ins in &mut blk.insns {
                if let IrOp::Try { catch_block, .. } = &mut ins.op {
                    if let Some(t2) = redirect.get(catch_block.0 as usize).and_then(|x| *x) {
                        if t2 != *catch_block {
                            *catch_block = t2;
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    // Remove unreachable blocks and remap ids.
    let reachable = reachable_blocks(func);
    if reachable.len() == func.blocks.len() {
        return;
    }

    let old_n = func.blocks.len();
    let mut old_to_new: Vec<Option<BlockId>> = vec![None; old_n];
    let mut new_blocks: Vec<crate::ir::IrBlock> = Vec::with_capacity(reachable.len());
    let mut new_id = 0u32;
    for old_id in 0..old_n {
        if reachable.contains(&old_id) {
            old_to_new[old_id] = Some(BlockId(new_id));
            new_blocks.push(func.blocks[old_id].clone());
            new_id += 1;
        }
    }
    func.blocks = new_blocks;

    for blk in &mut func.blocks {
        remap_terminator(&mut blk.term, &old_to_new);
        for ins in &mut blk.insns {
            if let IrOp::Try { catch_block, .. } = &mut ins.op {
                if let Some(new) = old_to_new.get(catch_block.0 as usize).and_then(|o| *o) {
                    *catch_block = new;
                }
            }
        }
    }
    func.entry = old_to_new[func.entry.0 as usize].unwrap_or(func.entry);
}
