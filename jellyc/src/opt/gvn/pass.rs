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
