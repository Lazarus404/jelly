use std::collections::HashMap;

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrInsn, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::regalloc::{self, PReg};

use super::cfg::{rewrite_edge_target, shrink_empty_jmp_blocks, succ_count};

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
                        .ok_or_else(|| {
                            CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id")
                        })?
                        .push(from);
                }
                IrTerminator::JmpIf {
                    then_tgt, else_tgt, ..
                } => {
                    for tgt in [then_tgt, else_tgt] {
                        preds
                            .get_mut(tgt.0 as usize)
                            .ok_or_else(|| {
                                CompileError::new(
                                    ErrorKind::Internal,
                                    Span::point(0),
                                    "bad block id",
                                )
                            })?
                            .push(from);
                    }
                }
                IrTerminator::SwitchKind { cases, default, .. } => {
                    preds
                        .get_mut(default.0 as usize)
                        .ok_or_else(|| {
                            CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id")
                        })?
                        .push(from);
                    for (_k, tgt) in cases {
                        preds
                            .get_mut(tgt.0 as usize)
                            .ok_or_else(|| {
                                CompileError::new(
                                    ErrorKind::Internal,
                                    Span::point(0),
                                    "bad block id",
                                )
                            })?
                            .push(from);
                    }
                }
                IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => {}
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
                        let dst_tid = *f.vreg_types.get(dst.0 as usize).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Internal,
                                Span::point(0),
                                "bad vreg type id",
                            )
                        })?;

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
                            let src_tid = *f.vreg_types.get(src.0 as usize).ok_or_else(|| {
                                CompileError::new(
                                    ErrorKind::Internal,
                                    Span::point(0),
                                    "bad vreg type id",
                                )
                            })?;
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
        let mut entries: Vec<(BlockId, Vec<(VRegId, VRegId, TypeId)>)> =
            moves_by_pred.into_iter().collect();
        entries.sort_by_key(|(bid, _)| bid.0);
        for (pred, moves) in entries {
            let pbi = pred.0 as usize;
            let pblk = f.blocks.get_mut(pbi).ok_or_else(|| {
                CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id")
            })?;

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
                        let ready =
                            map.iter().find_map(
                                |(&d, &s)| {
                                    if !dsts.contains(&s) {
                                        Some(d)
                                    } else {
                                        None
                                    }
                                },
                            );
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
