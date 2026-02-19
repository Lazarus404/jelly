use std::collections::HashMap;

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrInsn, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::regalloc::{self, PReg};

pub fn eliminate_phis(m: &mut IrModule) -> Result<(), CompileError> {
    for f in &mut m.funcs {
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
                                return Err(CompileError::new(
                                    ErrorKind::Internal,
                                    Span::point(0),
                                    "phi incoming from non-predecessor block",
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
        for (pred, moves) in moves_by_pred {
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
    }
    Ok(())
}

