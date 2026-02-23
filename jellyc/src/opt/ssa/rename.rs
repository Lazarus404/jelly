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
