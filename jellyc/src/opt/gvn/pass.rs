use crate::ir::{IrBlock, IrFunction, IrModule, IrOp, VRegId};
use std::collections::HashMap;

use super::cfg::block_order_rpo;
use super::dom::compute_idom;
use super::key::{key_for_op, Key};

pub fn global_value_numbering(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        if global_value_numbering_func(func) {
            changed = true;
        }
    }

    changed
}

fn global_value_numbering_func(func: &mut IrFunction) -> bool {
    let rpo = block_order_rpo(func);
    if rpo.is_empty() {
        return false;
    }
    let idom = compute_idom(func, &rpo);
    let n = func.blocks.len();
    let entry = func.entry.0 as usize;

    let mut children: Vec<Vec<usize>> = vec![Vec::new(); n];
    for b in 0..n {
        if b == entry {
            continue;
        }
        if let Some(p) = idom[b] {
            if p != b {
                children[p].push(b);
            }
        }
    }
    for ch in &mut children {
        ch.sort_unstable();
    }

    fn dfs_gvn(
        bid: usize,
        blocks: &mut [IrBlock],
        children: &[Vec<usize>],
        vreg_types: &[u32],
        env: &mut HashMap<Key, VRegId>,
        changed: &mut bool,
    ) {
        let mut pushed: Vec<Key> = Vec::new();

        for ins in &mut blocks[bid].insns {
            // Skip phis; they are SSA plumbing and should be handled by copy/phi passes.
            if matches!(ins.op, IrOp::Phi { .. }) {
                continue;
            }

            let Some(dst) = ins.op.def() else {
                continue;
            };
            let Some(k) = key_for_op(&ins.op, vreg_types) else {
                continue;
            };

            if let Some(&existing) = env.get(&k) {
                if existing != dst {
                    ins.op = IrOp::Mov { dst, src: existing };
                    *changed = true;
                }
                continue;
            }

            env.insert(k, dst);
            pushed.push(k);
        }

        for &c in &children[bid] {
            dfs_gvn(c, blocks, children, vreg_types, env, changed);
        }

        for k in pushed.into_iter().rev() {
            env.remove(&k);
        }
    }

    let mut env: HashMap<Key, VRegId> = HashMap::new();
    let mut changed = false;
    dfs_gvn(
        entry,
        &mut func.blocks,
        &children,
        &func.vreg_types,
        &mut env,
        &mut changed,
    );
    changed
}
