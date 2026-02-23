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

use crate::ir::IrFunction;

use super::cfg::term_succs;

pub(super) fn compute_idom(func: &IrFunction, rpo: &[usize]) -> Vec<Option<usize>> {
    let n = func.blocks.len();
    let entry = func.entry.0 as usize;
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut reachable = vec![false; n];
    for &b in rpo {
        reachable[b] = true;
    }
    for &b in rpo {
        for s in term_succs(&func.blocks[b].term) {
            if s < n && reachable[s] {
                preds[s].push(b);
            }
        }
    }
    for p in &mut preds {
        p.sort_unstable();
        p.dedup();
    }

    let mut rpo_index: Vec<usize> = vec![usize::MAX; n];
    for (i, &b) in rpo.iter().enumerate() {
        rpo_index[b] = i;
    }

    fn intersect(
        mut b1: usize,
        mut b2: usize,
        idom: &[Option<usize>],
        rpo_index: &[usize],
    ) -> usize {
        while b1 != b2 {
            while rpo_index[b1] > rpo_index[b2] {
                b1 = idom[b1].expect("idom must be set");
            }
            while rpo_index[b2] > rpo_index[b1] {
                b2 = idom[b2].expect("idom must be set");
            }
        }
        b1
    }

    let mut idom: Vec<Option<usize>> = vec![None; n];
    idom[entry] = Some(entry);

    let mut changed = true;
    while changed {
        changed = false;
        for &b in rpo.iter().skip(1) {
            let mut it = preds[b].iter().copied().filter(|&p| idom[p].is_some());
            let Some(mut new_idom) = it.next() else {
                continue;
            };
            for p in it {
                new_idom = intersect(p, new_idom, &idom, &rpo_index);
            }
            if idom[b] != Some(new_idom) {
                idom[b] = Some(new_idom);
                changed = true;
            }
        }
    }

    idom
}
