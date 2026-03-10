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

use crate::ir::{BlockId, IrBlock, IrFunction, IrOp, TypeId, VRegId};
use std::collections::HashMap;

use super::rewrite::{map_op_uses, map_term_uses, rename_use, set_op_def};
use super::util::{count_leading_phis, new_vreg};

pub(super) fn rename_function_to_ssa(
    func: &mut IrFunction,
    entry: usize,
    succs: &[Vec<usize>],
    children: &[Vec<usize>],
    is_multi: &[bool],
    multi_vars: &[VRegId],
    phi_dst_to_var: &HashMap<VRegId, VRegId>,
) {
    // vreg -> stack of renamed ids (only used for multi-vars).
    let mut stacks: Vec<Vec<VRegId>> = vec![Vec::new(); func.vreg_types.len()];
    for &v in multi_vars {
        stacks[v.0 as usize].push(v);
    }

    fn rename_block(
        bid: usize,
        blocks: &mut [IrBlock],
        vreg_types: &mut Vec<TypeId>,
        succs: &[Vec<usize>],
        children: &[Vec<usize>],
        is_multi: &[bool],
        stacks: &mut Vec<Vec<VRegId>>,
        multi_vars: &[VRegId],
        phi_dst_to_var: &HashMap<VRegId, VRegId>,
    ) {
        // Save stack lengths for restoration.
        let saved: Vec<(usize, usize)> = multi_vars
            .iter()
            .map(|v| (v.0 as usize, stacks[v.0 as usize].len()))
            .collect();

        // Process leading phi defs (only inserted phis affect multi-var stacks).
        let lead = count_leading_phis(&blocks[bid].insns);
        for i in 0..lead {
            if let IrOp::Phi { dst, .. } = blocks[bid].insns[i].op {
                if let Some(&var) = phi_dst_to_var.get(&dst) {
                    stacks[var.0 as usize].push(dst);
                }
            }
        }

        // Rename within block (skip leading phis; phi incomings are handled per-edge).
        for ins in blocks[bid].insns.iter_mut().skip(lead) {
            map_op_uses(&mut ins.op, |v| rename_use(v, is_multi, stacks));

            if let Some(d) = ins.op.def() {
                let di = d.0 as usize;
                if di < is_multi.len() && is_multi[di] {
                    let tid = vreg_types[di];
                    let newd = new_vreg(vreg_types, tid);
                    set_op_def(&mut ins.op, newd);
                    stacks[di].push(newd);
                }
            }
        }

        // Rename terminator uses.
        map_term_uses(&mut blocks[bid].term, |v| rename_use(v, is_multi, stacks));

        // Fill phi operands in successors for this predecessor.
        let this_bid = BlockId(bid as u32);
        for &sid in &succs[bid] {
            let succ_block = &mut blocks[sid];
            let lead_phi = count_leading_phis(&succ_block.insns);
            for i in 0..lead_phi {
                if let IrOp::Phi { incomings, .. } = &mut succ_block.insns[i].op {
                    for (pb, v) in incomings.iter_mut() {
                        if *pb == this_bid {
                            *v = rename_use(*v, is_multi, stacks);
                        }
                    }
                }
            }
        }

        // Recurse dom children.
        for &c in &children[bid] {
            rename_block(
                c,
                blocks,
                vreg_types,
                succs,
                children,
                is_multi,
                stacks,
                multi_vars,
                phi_dst_to_var,
            );
        }

        // Restore stacks.
        for (vi, len) in saved {
            stacks[vi].truncate(len);
        }
    }

    rename_block(
        entry,
        &mut func.blocks,
        &mut func.vreg_types,
        succs,
        children,
        is_multi,
        &mut stacks,
        multi_vars,
        phi_dst_to_var,
    );
}
