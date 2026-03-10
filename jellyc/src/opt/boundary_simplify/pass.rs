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

use crate::ir::{IrModule, IrOp, VRegId};
use crate::typectx::T_DYNAMIC;
use std::collections::HashMap;

use super::cfg::block_successors;

fn compute_def_counts(func: &crate::ir::IrFunction) -> Vec<u32> {
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
    def_counts
}

fn build_preds(func: &crate::ir::IrFunction) -> Vec<Vec<usize>> {
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
    preds
}

fn vreg_tid(vreg_types: &[u32], v: VRegId) -> u32 {
    vreg_types.get(v.0 as usize).copied().unwrap_or(T_DYNAMIC)
}

fn is_single_def(def_counts: &[u32], v: VRegId) -> bool {
    def_counts.get(v.0 as usize).copied().unwrap_or(0) <= 1
}

/// Simplify redundant `ToDyn`/`FromDyn*` sequences.
/// Returns true if any change was made.
pub fn simplify_dynamic_boundaries(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let def_counts = compute_def_counts(func);
        let preds = build_preds(func);
        let nb = func.blocks.len();

        // Forward dataflow: dyn vreg -> typed origin vreg (from ToDyn).
        let mut out_maps: Vec<HashMap<VRegId, VRegId>> = vec![HashMap::new(); nb];

        let mut changed_this_func = true;
        while changed_this_func {
            changed_this_func = false;

            for bid in 0..nb {
                // Meet: keep only facts that are identical across all predecessors.
                let mut dyn_origin: HashMap<VRegId, VRegId> = HashMap::new();
                let mut first = true;
                for &p in &preds[bid] {
                    if first {
                        dyn_origin = out_maps[p].clone();
                        first = false;
                    } else {
                        let pred_map = &out_maps[p];
                        dyn_origin.retain(|k, v| pred_map.get(k) == Some(v));
                    }
                }
                if first {
                    dyn_origin.clear();
                }

                let vreg_types = func.vreg_types.as_slice();
                let block = &mut func.blocks[bid];
                for ins in &mut block.insns {
                    // If a vreg is redefined, any previous origin fact about it is invalid.
                    if let Some(d) = ins.op.def() {
                        if !matches!(
                            ins.op,
                            IrOp::ToDyn { .. } | IrOp::Mov { .. } | IrOp::Phi { .. }
                        ) {
                            dyn_origin.remove(&d);
                        }
                    }

                    match ins.op {
                        IrOp::ToDyn { dst, src } => {
                            let dst_tid = vreg_tid(vreg_types, dst);
                            let src_tid = vreg_tid(vreg_types, src);

                            // `ToDyn` from an already-Dynamic src is just a copy.
                            if dst_tid == T_DYNAMIC && src_tid == T_DYNAMIC {
                                ins.op = IrOp::Mov { dst, src };
                                changed = true;

                                // Preserve any origin fact from src if present.
                                if let Some(&o) = dyn_origin.get(&src) {
                                    dyn_origin.insert(dst, o);
                                } else {
                                    dyn_origin.remove(&dst);
                                }
                                continue;
                            }

                            // Record provenance only for Dynamic dst, and only when the typed origin is single-def.
                            // (Params/captures have 0 explicit defs, which is still single-def.)
                            if dst_tid == T_DYNAMIC
                                && src_tid != T_DYNAMIC
                                && is_single_def(&def_counts, src)
                            {
                                dyn_origin.insert(dst, src);
                            } else {
                                dyn_origin.remove(&dst);
                            }
                        }
                        IrOp::Mov { dst, src } => {
                            let dst_tid = vreg_tid(vreg_types, dst);
                            let src_tid = vreg_tid(vreg_types, src);
                            if dst_tid == T_DYNAMIC && src_tid == T_DYNAMIC {
                                if let Some(&o) = dyn_origin.get(&src) {
                                    dyn_origin.insert(dst, o);
                                } else {
                                    dyn_origin.remove(&dst);
                                }
                            } else {
                                dyn_origin.remove(&dst);
                            }
                        }
                        IrOp::Phi { dst, .. } => {
                            let dst_tid = vreg_tid(vreg_types, dst);
                            if dst_tid != T_DYNAMIC {
                                dyn_origin.remove(&dst);
                                continue;
                            }
                            dyn_origin.remove(&dst);
                            let incomings = match &ins.op {
                                IrOp::Phi { incomings, .. } => incomings,
                                _ => unreachable!("phi arm must match a phi op"),
                            };
                            let Some((_, first)) = incomings.first() else {
                                continue;
                            };
                            let Some(&origin0) = dyn_origin.get(first) else {
                                continue;
                            };
                            if !is_single_def(&def_counts, origin0) {
                                continue;
                            }
                            let ok = incomings
                                .iter()
                                .all(|(_, v)| dyn_origin.get(v) == Some(&origin0));
                            let origin_tid = vreg_tid(vreg_types, origin0);
                            if ok && origin_tid != T_DYNAMIC {
                                dyn_origin.insert(dst, origin0);
                            }
                        }
                        IrOp::FromDynI8 { dst, src }
                        | IrOp::FromDynI16 { dst, src }
                        | IrOp::FromDynI32 { dst, src }
                        | IrOp::FromDynI64 { dst, src }
                        | IrOp::FromDynF16 { dst, src }
                        | IrOp::FromDynF32 { dst, src }
                        | IrOp::FromDynF64 { dst, src }
                        | IrOp::FromDynBool { dst, src }
                        | IrOp::FromDynPtr { dst, src } => {
                            let Some(&origin) = dyn_origin.get(&src) else {
                                continue;
                            };
                            if !is_single_def(&def_counts, origin) {
                                continue;
                            }

                            let dst_tid = vreg_tid(vreg_types, dst);
                            let origin_tid = vreg_tid(vreg_types, origin);

                            // Only eliminate when the unbox is a no-op w.r.t static types.
                            if dst_tid == origin_tid && dst_tid != T_DYNAMIC {
                                ins.op = IrOp::Mov { dst, src: origin };
                                changed = true;
                            }
                        }
                        _ => {}
                    }
                }

                if out_maps[bid] != dyn_origin {
                    out_maps[bid] = dyn_origin;
                    changed_this_func = true;
                }
            }
        }
    }

    changed
}
