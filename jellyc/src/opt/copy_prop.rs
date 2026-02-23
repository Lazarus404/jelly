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

// Copy propagation.
//
// This pass rewrites explicit operands through chains of `Mov {dst, src}`.
// It intentionally does NOT rewrite `Call.arg_base` or `Closure.cap_base` because those fields
// denote implicit contiguous vreg windows ([base..base+n)), and rewriting the base could break
// contiguity assumptions.
use crate::ir::{IrModule, IrOp, VRegId};
use std::collections::HashMap;

mod cfg;
mod rewrite;

use self::cfg::block_successors;
use self::rewrite::{resolve, rewrite_op_uses, rewrite_term_uses};

pub fn copy_propagation(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let vreg_types = &func.vreg_types;

        // Precompute def counts so we only propagate aliases to single-def typed sources.
        let mut def_counts: Vec<u32> = vec![0; func.vreg_types.len()];
        for b in &func.blocks {
            for ins in &b.insns {
                if let Some(d) = ins.op.def() {
                    let i = d.0 as usize;
                    if i < def_counts.len() {
                        def_counts[i] = def_counts[i].saturating_add(1);
                    }
                }
            }
        }

        // Build predecessor lists from normal CFG edges only.
        // (We intentionally do NOT propagate facts along exception edges; catch blocks are treated as unknown.)
        let nb = func.blocks.len();
        let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nb];
        for (i, b) in func.blocks.iter().enumerate() {
            for succ in block_successors(&b.term) {
                if succ < nb {
                    preds[succ].push(i);
                }
            }
        }

        let mut out_maps: Vec<HashMap<VRegId, VRegId>> = vec![HashMap::new(); nb];
        let mut changed_this_func = true;
        while changed_this_func {
            changed_this_func = false;
            for bid in 0..nb {
                // Meet: keep only alias facts that are identical across all predecessors.
                let mut alias: HashMap<VRegId, VRegId> = HashMap::new();
                let mut first = true;
                for &p in &preds[bid] {
                    if first {
                        alias = out_maps[p].clone();
                        first = false;
                    } else {
                        let pred_map = &out_maps[p];
                        alias.retain(|k, v| pred_map.get(k) == Some(v));
                    }
                }
                if first {
                    alias.clear();
                }

                let block = &mut func.blocks[bid];
                for ins in &mut block.insns {
                    changed |= rewrite_op_uses(&mut ins.op, &mut alias);

                    // Record copy aliases for subsequent instructions in this path.
                    if let IrOp::Mov { dst, src } = ins.op {
                        let src = resolve(src, &mut alias);
                        // Only propagate true copies. `Mov` is also used as a placeholder for certain
                        // type coercions (eg I16 -> I32) that are legalized later in codegen; those
                        // must NOT be treated as aliases here.
                        let dst_tid = vreg_types.get(dst.0 as usize).copied();
                        let src_tid = vreg_types.get(src.0 as usize).copied();
                        if src != dst
                            && dst_tid.is_some_and(|dt| Some(dt) == src_tid)
                            && def_counts.get(src.0 as usize).copied().unwrap_or(0) <= 1
                        {
                            alias.insert(dst, src);
                        } else {
                            alias.remove(&dst);
                        }
                    } else if let IrOp::Phi { dst, .. } = ins.op {
                        // If all incoming values are identical (after resolving through existing aliases),
                        // then this phi is a pure copy and we can treat it as an alias for subsequent uses.
                        let incomings = match &ins.op {
                            IrOp::Phi { incomings, .. } => incomings,
                            _ => unreachable!("phi arm must match a phi op"),
                        };
                        let Some((_, first_in)) = incomings.first() else {
                            alias.remove(&dst);
                            continue;
                        };
                        let cand = resolve(*first_in, &mut alias);
                        let all_same = incomings
                            .iter()
                            .all(|(_, v)| resolve(*v, &mut alias) == cand);

                        let dst_tid = vreg_types.get(dst.0 as usize).copied();
                        let cand_tid = vreg_types.get(cand.0 as usize).copied();
                        if all_same
                            && cand != dst
                            && dst_tid.is_some_and(|dt| Some(dt) == cand_tid)
                            && def_counts.get(cand.0 as usize).copied().unwrap_or(0) <= 1
                        {
                            alias.insert(dst, cand);
                        } else {
                            alias.remove(&dst);
                        }
                    } else if let Some(dst) = ins.op.def() {
                        alias.remove(&dst);
                    }
                }

                changed |= rewrite_term_uses(&mut block.term, &mut alias);

                if out_maps[bid] != alias {
                    out_maps[bid] = alias;
                    changed_this_func = true;
                }
            }
        }
    }

    changed
}

#[cfg(test)]
mod tests;
