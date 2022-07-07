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

use std::collections::HashMap;

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrInsn, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::regalloc::{self, PReg};

/// Eliminate phi nodes from an IR module.
/// Phi is a special instruction that is used to merge multiple values into a single value.
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

