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

use crate::ir::{BlockId, IrFunction, IrInsn, IrOp, VRegId};
use std::collections::{BTreeSet, HashMap, HashSet};

use super::util::{count_leading_phis, new_vreg};

pub(super) fn insert_phis_for_multi_vars(
    func: &mut IrFunction,
    multi_vars: &[VRegId],
    is_multi: &[bool],
    multi_index: &[Option<usize>],
    preds: &[Vec<usize>],
    df: &[HashSet<usize>],
    live_in_multi: &[Vec<u64>],
) -> (HashMap<VRegId, VRegId>, bool) {
    let nb = func.blocks.len();
    let mut changed = false;

    // Def blocks per multi var.
    let mut def_blocks: HashMap<VRegId, HashSet<usize>> = HashMap::new();
    for (bi, blk) in func.blocks.iter().enumerate() {
        for ins in &blk.insns {
            if let Some(d) = ins.op.def() {
                if is_multi.get(d.0 as usize).copied().unwrap_or(false) {
                    def_blocks.entry(d).or_default().insert(bi);
                }
            }
        }
    }

    // NOTE: lowering already emits Phi nodes for loops (eg. while variables). Those Phis
    // conceptually define a new SSA name for some underlying variable v, but they don't
    // come with an explicit mapping. If we ignore them, the renamer won't push their `dst`
    // on the variable stack, which can leave stale uses of the pre-SSA vreg after we rename
    // away its defs.
    //
    // We treat any existing Phi where *all incomings are the same vreg `v`* as "the phi
    // for variable v" and reuse it instead of inserting a duplicate phi for v.
    let mut phi_dst_to_var: HashMap<VRegId, VRegId> = HashMap::new();

    for &v in multi_vars {
        let defs = def_blocks.get(&v).cloned().unwrap_or_default();
        let mut has_phi: Vec<bool> = vec![false; nb];
        let v_mi = multi_index[v.0 as usize].expect("multi var must have index");

        // Seed `has_phi` from existing leading phis that correspond to this var.
        for (bi, blk) in func.blocks.iter().enumerate() {
            let lead = count_leading_phis(&blk.insns);
            for i in 0..lead {
                let IrOp::Phi { dst, incomings } = &blk.insns[i].op else {
                    continue;
                };
                if incomings.is_empty() {
                    continue;
                }
                if incomings.iter().all(|(_, vv)| *vv == v) {
                    has_phi[bi] = true;
                    phi_dst_to_var.insert(*dst, v);
                }
            }
        }

        let mut work: BTreeSet<usize> = defs.iter().copied().collect();
        while let Some(x) = work.pop_first() {
            let mut ys: Vec<usize> = df[x].iter().copied().collect();
            ys.sort_unstable();
            for y in ys {
                // Pruned SSA: only place phi if v is live-in to y.
                if preds[y].len() < 2 {
                    continue;
                }
                let w = v_mi / 64;
                let b = v_mi % 64;
                if ((live_in_multi[y][w] >> b) & 1u64) == 0u64 {
                    continue;
                }
                if !has_phi[y] {
                    has_phi[y] = true;
                    let tid = func.vreg_types[v.0 as usize];
                    let phi_dst = new_vreg(&mut func.vreg_types, tid);
                    phi_dst_to_var.insert(phi_dst, v);

                    let mut incomings: Vec<(BlockId, VRegId)> = Vec::new();
                    for &p in &preds[y] {
                        incomings.push((BlockId(p as u32), v));
                    }

                    let idx = count_leading_phis(&func.blocks[y].insns);
                    func.blocks[y].insns.insert(
                        idx,
                        IrInsn {
                            span: crate::ast::Span::point(0),
                            op: IrOp::Phi {
                                dst: phi_dst,
                                incomings,
                            },
                        },
                    );
                    changed = true;

                    if !defs.contains(&y) {
                        work.insert(y);
                    }
                }
            }
        }
    }

    (phi_dst_to_var, changed)
}
