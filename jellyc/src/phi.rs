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

use std::collections::{HashMap, HashSet};

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrInsn, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::regalloc::{self, PReg};

fn succ_count(term: &IrTerminator) -> usize {
    match term {
        IrTerminator::Jmp { .. } => 1,
        IrTerminator::JmpIf { .. } => 2,
        IrTerminator::SwitchKind { cases, .. } => cases.len() + 1,
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => 0,
    }
}

fn rewrite_edge_target(term: &mut IrTerminator, old: BlockId, new: BlockId) -> bool {
    match term {
        IrTerminator::Jmp { target } => {
            if *target == old {
                *target = new;
                true
            } else {
                false
            }
        }
        IrTerminator::JmpIf { then_tgt, else_tgt, .. } => {
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
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => false,
    }
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

fn reachable_blocks(func: &crate::ir::IrFunction) -> HashSet<usize> {
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

/// After phi elimination, shrink away empty jump-only blocks (typically edge-split blocks
/// that ended up with no scheduled phi moves).
fn shrink_empty_jmp_blocks(func: &mut crate::ir::IrFunction) {
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
                IrTerminator::JmpIf { then_tgt, else_tgt, .. } => {
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
                IrTerminator::Ret { .. } | IrTerminator::Unreachable => {}
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
            match &mut ins.op {
                IrOp::Try { catch_block, .. } => {
                    if let Some(new) = old_to_new.get(catch_block.0 as usize).and_then(|o| *o) {
                        *catch_block = new;
                    }
                }
                _ => {}
            }
        }
    }
    func.entry = old_to_new[func.entry.0 as usize].unwrap_or(func.entry);
}

/// Eliminate phi nodes from an IR module.
/// Phi is a special instruction that is used to merge multiple values into a single value.
pub fn eliminate_phis(m: &mut IrModule) -> Result<(), CompileError> {
    for f in &mut m.funcs {
        // Split critical edges into phi blocks.
        //
        // The old implementation inserted all phi moves into predecessor blocks. That's only correct when
        // the predecessor has a single successor (so the moves are executed only on the edge into the phi
        // block). With SSA insertion, phi blocks can be reached via conditional/switch edges; we must split
        // those edges so moves can be inserted on the edge.
        let mut bi = 0usize;
        while bi < f.blocks.len() {
            let this_block = BlockId(bi as u32);

            // Collect unique predecessors mentioned by leading phi nodes.
            let mut phi_preds: Vec<BlockId> = Vec::new();
            let mut phi_count = 0usize;
            while let Some(ins) = f.blocks[bi].insns.get(phi_count) {
                if let IrOp::Phi { incomings, .. } = &ins.op {
                    for (p, _) in incomings {
                        phi_preds.push(*p);
                    }
                    phi_count += 1;
                } else {
                    break;
                }
            }
            if phi_count == 0 {
                bi += 1;
                continue;
            }
            phi_preds.sort_by_key(|b| b.0);
            phi_preds.dedup();

            for pred in phi_preds {
                let pbi = pred.0 as usize;
                if pbi >= f.blocks.len() {
                    continue;
                }
                if succ_count(&f.blocks[pbi].term) <= 1 {
                    continue;
                }

                let split = BlockId(f.blocks.len() as u32);
                f.blocks.push(crate::ir::IrBlock {
                    label: Some(format!("phi_edge_b{}_to_b{}", pred.0, this_block.0)),
                    insns: Vec::new(),
                    term: IrTerminator::Jmp { target: this_block },
                });

                // Redirect pred -> this_block to pred -> split
                let rewrote = rewrite_edge_target(&mut f.blocks[pbi].term, this_block, split);
                if !rewrote {
                    // Shouldn't happen unless phi incomings are malformed.
                    continue;
                }

                // Rewrite phi incomings from `pred` to `split` (for all leading phis).
                for ins_i in 0..phi_count {
                    let IrOp::Phi { incomings, .. } = &mut f.blocks[bi].insns[ins_i].op else {
                        break;
                    };
                    for (p, _src) in incomings.iter_mut() {
                        if *p == pred {
                            *p = split;
                        }
                    }
                }
            }

            bi += 1;
        }

        // Compute predecessor lists.
        let mut preds: Vec<Vec<BlockId>> = vec![Vec::new(); f.blocks.len()];
        for (bi, blk) in f.blocks.iter().enumerate() {
            let from = BlockId(bi as u32);
            match &blk.term {
                IrTerminator::Jmp { target } => {
                    preds
                        .get_mut(target.0 as usize)
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id"))?
                        .push(from);
                }
                IrTerminator::JmpIf { then_tgt, else_tgt, .. } => {
                    for tgt in [then_tgt, else_tgt] {
                        preds
                            .get_mut(tgt.0 as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id"))?
                            .push(from);
                    }
                }
                IrTerminator::SwitchKind { cases, default, .. } => {
                    preds
                        .get_mut(default.0 as usize)
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id"))?
                        .push(from);
                    for (_k, tgt) in cases {
                        preds
                            .get_mut(tgt.0 as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id"))?
                            .push(from);
                    }
                }
                IrTerminator::Ret { .. } | IrTerminator::Unreachable => {}
            }
        }

        // For each block, drain leading Phi ops.
        // Moves get inserted into predecessor blocks.
        let mut moves_by_pred: HashMap<BlockId, Vec<(VRegId, VRegId, TypeId)>> = HashMap::new();
        let labels: Vec<Option<String>> = f.blocks.iter().map(|b| b.label.clone()).collect();

        for (bi, blk) in f.blocks.iter_mut().enumerate() {
            let this_block = BlockId(bi as u32);

            while let Some(first) = blk.insns.first() {
                match &first.op {
                    IrOp::Phi { dst, incomings } => {
                        let dst_tid = *f
                            .vreg_types
                            .get(dst.0 as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad vreg type id"))?;

                        // Validate incomings reference real predecessors.
                        for (pred, src) in incomings {
                            if !preds[this_block.0 as usize].contains(pred) {
                                let fname = f.name.clone().unwrap_or_else(|| "<anon>".to_string());
                                let this_label = labels
                                    .get(this_block.0 as usize)
                                    .and_then(|x| x.clone())
                                    .unwrap_or_else(|| "<unlabeled>".to_string());
                                let pred_label = labels
                                    .get(pred.0 as usize)
                                    .and_then(|x| x.clone())
                                    .unwrap_or_else(|| "<unlabeled>".to_string());
                                let mut pred_list: Vec<String> = preds[this_block.0 as usize]
                                    .iter()
                                    .map(|bid| {
                                        let lab = labels
                                            .get(bid.0 as usize)
                                            .and_then(|x| x.clone())
                                            .unwrap_or_else(|| "<unlabeled>".to_string());
                                        format!("b{}({})", bid.0, lab)
                                    })
                                    .collect();
                                pred_list.sort();
                                return Err(CompileError::new(
                                    ErrorKind::Internal,
                                    Span::point(0),
                                    format!(
                                        "phi incoming from non-predecessor block: fn={} this=b{}({}) incoming=b{}({}) preds=[{}]",
                                        fname,
                                        this_block.0,
                                        this_label,
                                        pred.0,
                                        pred_label,
                                        pred_list.join(", ")
                                    ),
                                ));
                            }
                            // src type must match dst type.
                            let src_tid = *f
                                .vreg_types
                                .get(src.0 as usize)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad vreg type id"))?;
                            if src_tid != dst_tid {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    Span::point(0),
                                    "phi incoming type mismatch",
                                ));
                            }
                            moves_by_pred
                                .entry(*pred)
                                .or_default()
                                .push((*dst, *src, dst_tid));
                        }

                        blk.insns.remove(0);
                        continue;
                    }
                    _ => break, // Phi ops must be grouped at start.
                }
            }
        }

        // Insert scheduled moves into predecessors.
        let mut entries: Vec<(BlockId, Vec<(VRegId, VRegId, TypeId)>)> = moves_by_pred.into_iter().collect();
        entries.sort_by_key(|(bid, _)| bid.0);
        for (pred, moves) in entries {
            let pbi = pred.0 as usize;
            let pblk = f
                .blocks
                .get_mut(pbi)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id"))?;

            // Group by type so temp regs stay type-correct.
            let mut by_type: HashMap<TypeId, Vec<(VRegId, VRegId)>> = HashMap::new();
            for (d, s, tid) in moves {
                by_type.entry(tid).or_default().push((d, s));
            }

            // Stable order for determinism.
            let mut types: Vec<TypeId> = by_type.keys().copied().collect();
            types.sort();

            for tid in types {
                let mv = by_type.get(&tid).expect("type key exists");

                fn needs_temp(moves: &[(VRegId, VRegId)]) -> bool {
                    use std::collections::{HashMap, HashSet};
                    let mut map: HashMap<VRegId, VRegId> = HashMap::new();
                    for &(d, s) in moves {
                        if d != s {
                            map.insert(d, s);
                        }
                    }
                    let mut dsts: HashSet<VRegId> = map.keys().copied().collect();
                    loop {
                        let ready = map
                            .iter()
                            .find_map(|(&d, &s)| if !dsts.contains(&s) { Some(d) } else { None });
                        if let Some(d) = ready {
                            map.remove(&d);
                            dsts = map.keys().copied().collect();
                            continue;
                        }
                        break;
                    }
                    !map.is_empty()
                }

                let moves_p: Vec<(PReg, PReg)> = mv
                    .iter()
                    .map(|(d, s)| (PReg(d.0 as u16), PReg(s.0 as u16)))
                    .collect();

                let temp_p = if needs_temp(mv) {
                    let temp = VRegId(f.vreg_types.len() as u32);
                    f.vreg_types.push(tid);
                    PReg(temp.0 as u16)
                } else {
                    // Dummy temp: must not collide with any destination; will not be used.
                    PReg(u16::MAX)
                };
                let sched = regalloc::schedule_parallel_moves(&moves_p, temp_p);

                for (d, s) in sched {
                    pblk.insns.push(IrInsn {
                        span: Span::point(0),
                        op: IrOp::Mov {
                            dst: VRegId(d.0 as u32),
                            src: VRegId(s.0 as u32),
                        },
                    });
                }
            }
        }

        // Post-step: shrink empty edge blocks that had no scheduled moves.
        shrink_empty_jmp_blocks(f);
    }
    Ok(())
}

