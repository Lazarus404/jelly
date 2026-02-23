//! ConstFun propagation through phi-defined vregs.
//!
//! After phi elimination, phis become Movs at the end of predecessor blocks.
//! We compute which vregs at block entry get their value from which (pred, src),
//! then merge const_fun from predecessors when all sources agree.

use std::collections::HashMap;

use crate::ir::{BlockId, IrFunction, IrOp, VRegId};

use super::super::block_successors;

/// For each block B and vreg dst, the (pred, src) pairs that flow into dst at B's entry.
/// Built from Mov dst, src at the end of each predecessor.
fn build_phi_flows(f: &IrFunction) -> HashMap<(BlockId, VRegId), Vec<(BlockId, VRegId)>> {
    let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (bi, blk) in f.blocks.iter().enumerate() {
        let from = BlockId(bi as u32);
        for succ in block_successors(&blk.term) {
            preds.entry(succ).or_default().push(from);
        }
    }

    let mut flows: HashMap<(BlockId, VRegId), Vec<(BlockId, VRegId)>> = HashMap::new();
    for (succ, pred_list) in preds {
        if pred_list.len() < 2 {
            continue;
        }
        for pred in &pred_list {
            let pbi = pred.0 as usize;
            let blk = match f.blocks.get(pbi) {
                Some(b) => b,
                None => continue,
            };
            let movs = blk
                .insns
                .iter()
                .rev()
                .take_while(|ins| matches!(ins.op, IrOp::Mov { .. }))
                .collect::<Vec<_>>();
            for ins in movs.iter().rev() {
                if let IrOp::Mov { dst, src } = ins.op {
                    flows
                        .entry((succ, dst))
                        .or_default()
                        .push((*pred, src));
                }
            }
        }
    }
    flows
}

/// Compute const_fun at block exit for each block, using RPO order.
/// Returns const_fun_at_exit[block_id] = vec of Option<func_index> per vreg.
pub(super) fn compute_const_fun_at_exit(
    f: &IrFunction,
    map_func_index: &impl Fn(u32) -> u32,
    order: &[BlockId],
) -> Vec<Vec<Option<u32>>> {
    let nvregs = f.vreg_types.len();
    let nblocks = f.blocks.len();
    let phi_flows = build_phi_flows(f);

    let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (bi, blk) in f.blocks.iter().enumerate() {
        let from = BlockId(bi as u32);
        for succ in block_successors(&blk.term) {
            preds.entry(succ).or_default().push(from);
        }
    }

    let mut const_fun_at_exit: Vec<Vec<Option<u32>>> = vec![vec![None; nvregs]; nblocks];

    for b in order {
        let bi = b.0 as usize;
        let blk = &f.blocks[bi];

        let mut state = vec![None; nvregs];

        // Initialize from single predecessor so Mov phi_dst, src can propagate ConstFun.
        let pred_list = preds.get(b).map(|p| p.as_slice()).unwrap_or(&[]);
        if pred_list.len() == 1 {
            let pred_i = pred_list[0].0 as usize;
            if pred_i < const_fun_at_exit.len() {
                state = const_fun_at_exit[pred_i].clone();
            }
        }

        for ((_, dst), pred_srcs) in phi_flows.iter().filter(|((bid, _), _)| *bid == *b) {
            let dst_i = dst.0 as usize;
            if dst_i >= nvregs {
                continue;
            }
            let vals: Vec<Option<u32>> = pred_srcs
                .iter()
                .map(|(pred, src)| {
                    let pred_i = pred.0 as usize;
                    let src_i = src.0 as usize;
                    if pred_i >= const_fun_at_exit.len() || src_i >= nvregs {
                        None
                    } else {
                        const_fun_at_exit[pred_i][src_i]
                    }
                })
                .collect();
            if vals.is_empty() {
                continue;
            }
            let first = vals[0];
            if vals.iter().all(|v| *v == first) {
                state[dst_i] = first;
            }
        }

        for ins in &blk.insns {
            if let Some(d) = ins.op.def() {
                let di = d.0 as usize;
                if di < nvregs {
                    state[di] = None;
                }
            }
            match &ins.op {
                IrOp::ConstFun { dst, func_index } => {
                    let di = dst.0 as usize;
                    if di < nvregs {
                        state[di] = Some(map_func_index(*func_index));
                    }
                }
                IrOp::Mov { dst, src } => {
                    let di = dst.0 as usize;
                    let si = src.0 as usize;
                    if di < nvregs && si < nvregs {
                        state[di] = state[si];
                    }
                }
                _ => {}
            }
        }

        const_fun_at_exit[bi] = state;
    }

    const_fun_at_exit
}

/// Compute const_fun at block entry by merging from predecessor exits.
/// For blocks with a single predecessor, copy that pred's exit state.
/// For blocks with multiple predecessors, merge for phi-defined vregs.
pub(super) fn compute_const_fun_at_entry(
    f: &IrFunction,
    const_fun_at_exit: &[Vec<Option<u32>>],
    order: &[BlockId],
) -> Vec<Vec<Option<u32>>> {
    let nvregs = f.vreg_types.len();
    let nblocks = f.blocks.len();
    let phi_flows = build_phi_flows(f);

    let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();
    for (bi, blk) in f.blocks.iter().enumerate() {
        let from = BlockId(bi as u32);
        for succ in block_successors(&blk.term) {
            preds.entry(succ).or_default().push(from);
        }
    }

    let mut const_fun_at_entry: Vec<Vec<Option<u32>>> = vec![vec![None; nvregs]; nblocks];

    for b in order {
        let bi = b.0 as usize;
        if bi >= nblocks {
            continue;
        }
        let pred_list = preds.get(b).map(|p| p.as_slice()).unwrap_or(&[]);
        if pred_list.len() == 1 {
            let pred_i = pred_list[0].0 as usize;
            if pred_i < const_fun_at_exit.len() {
                const_fun_at_entry[bi] = const_fun_at_exit[pred_i].clone();
            }
        } else if pred_list.len() > 1 {
            for ((_, dst), pred_srcs) in phi_flows.iter().filter(|((bid, _), _)| *bid == *b) {
                let dst_i = dst.0 as usize;
                if dst_i >= nvregs {
                    continue;
                }
                let vals: Vec<Option<u32>> = pred_srcs
                    .iter()
                    .map(|(pred, src)| {
                        let pred_i = pred.0 as usize;
                        let src_i = src.0 as usize;
                        if pred_i < const_fun_at_exit.len() && src_i < nvregs {
                            const_fun_at_exit[pred_i][src_i]
                        } else {
                            None
                        }
                    })
                    .collect();
                if !vals.is_empty() && vals.iter().all(|v| *v == vals[0]) {
                    const_fun_at_entry[bi][dst_i] = vals[0];
                }
            }
        }
    }

    const_fun_at_entry
}
