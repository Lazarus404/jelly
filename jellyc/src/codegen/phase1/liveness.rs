use std::collections::HashSet;

use crate::ir::IrFunction;
use crate::regalloc::InstrInfo;
use crate::regalloc::VReg;

use super::super::VirtualStream;
use super::super::{block_successors, term_size};

pub(super) fn extend_for_backedges(f: &IrFunction, block_start: &[u32], infos: &mut [InstrInfo]) {
    // LSRA uses a linear instruction order. To remain correct in the presence of loops/backedges,
    // extend live ranges to block ends using CFG liveness (so loop-carried values aren't expired
    // before the backedge).
    //
    // We do this by computing block live-in sets and adding them as "uses" on the block's
    // terminator instruction info for backedges.
    let nb = f.blocks.len();
    let mut use_set: Vec<HashSet<u32>> = vec![HashSet::new(); nb];
    let mut def_set: Vec<HashSet<u32>> = vec![HashSet::new(); nb];
    let mut succs: Vec<Vec<usize>> = vec![Vec::new(); nb];

    for (bi, blk) in f.blocks.iter().enumerate() {
        let start = block_start[bi] as usize;
        let end = start + blk.insns.len() + (term_size(&blk.term) as usize);
        let mut defs: HashSet<u32> = HashSet::new();
        let mut uses: HashSet<u32> = HashSet::new();
        for ii in start..end {
            if ii >= infos.len() {
                break;
            }
            for &u in &infos[ii].uses {
                if !defs.contains(&u.0) {
                    uses.insert(u.0);
                }
            }
            for &d in &infos[ii].defs {
                defs.insert(d.0);
            }
        }
        use_set[bi] = uses;
        def_set[bi] = defs;
        succs[bi] = block_successors(&blk.term)
            .into_iter()
            .map(|b| b.0 as usize)
            .filter(|&s| s < nb)
            .collect();
    }

    let mut live_in: Vec<HashSet<u32>> = vec![HashSet::new(); nb];
    let mut live_out: Vec<HashSet<u32>> = vec![HashSet::new(); nb];

    let mut changed = true;
    while changed {
        changed = false;
        for bi in (0..nb).rev() {
            let mut out: HashSet<u32> = HashSet::new();
            for &s in &succs[bi] {
                out.extend(live_in[s].iter().copied());
            }

            let mut inb: HashSet<u32> = use_set[bi].clone();
            for v in out.iter().copied() {
                if !def_set[bi].contains(&v) {
                    inb.insert(v);
                }
            }

            if inb != live_in[bi] || out != live_out[bi] {
                live_in[bi] = inb;
                live_out[bi] = out;
                changed = true;
            }
        }
    }

    for (bi, blk) in f.blocks.iter().enumerate() {
        // Forward edges are naturally handled by linear order (uses in successors appear later).
        // The only missing case for linear-scan is **backedges**, where successor uses occur at
        // an earlier PC than the edge source. Extend those values to the edge source's end.
        let from_pc = block_start[bi];
        let mut need: HashSet<u32> = HashSet::new();
        for &s in &succs[bi] {
            if block_start[s] < from_pc {
                need.extend(live_in[s].iter().copied());
            }
        }
        if need.is_empty() {
            continue;
        }

        let start = block_start[bi] as usize;
        let term_begin = start + blk.insns.len();
        let term_info_idx = term_begin;
        if term_info_idx >= infos.len() {
            continue;
        }
        let info = &mut infos[term_info_idx];
        for v in need.into_iter() {
            if !info.uses.iter().any(|u| u.0 == v) {
                info.uses.push(VReg(v));
            }
        }
    }
}

pub(super) fn finish(f: &IrFunction, block_start: &[u32], mut vs: VirtualStream) -> VirtualStream {
    extend_for_backedges(f, block_start, &mut vs.infos);
    vs
}
