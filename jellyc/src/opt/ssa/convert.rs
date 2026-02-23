use crate::ir::{IrModule, VRegId};

use super::{cfg, dom, liveness, phi, pin, rename};

pub fn convert_to_ssa(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let nb = func.blocks.len();
        if nb == 0 {
            continue;
        }

        let pinned = pin::compute_pinned(func);

        // Def counts.
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

        let mut is_multi: Vec<bool> = vec![false; func.vreg_types.len()];
        let mut multi_vars: Vec<VRegId> = Vec::new();
        for (i, &c) in def_counts.iter().enumerate() {
            if c > 1 && !pinned.get(i).copied().unwrap_or(false) {
                is_multi[i] = true;
                multi_vars.push(VRegId(i as u32));
            }
        }
        if multi_vars.is_empty() {
            continue;
        }

        // Build a vreg -> multi-var index map for fast membership testing.
        let mut multi_index: Vec<Option<usize>> = vec![None; func.vreg_types.len()];
        for (idx, v) in multi_vars.iter().enumerate() {
            let vi = v.0 as usize;
            if vi < multi_index.len() {
                multi_index[vi] = Some(idx);
            }
        }

        // CFG preds/succs (includes conservative `Try` edges).
        let (succs, preds) = cfg::build_cfg(func);

        let entry = func.entry.0 as usize;
        let doms = dom::compute_dom(&preds, entry);
        let idom = dom::compute_idom(&doms, entry);
        let children = dom::dom_tree_children(&idom, entry);
        let df = dom::dominance_frontiers(entry, &succs, &children, &idom);

        // Liveness (pruned SSA): only insert Phi for a variable if it's live-in at the join.
        //
        // We compute liveness only for `multi_vars` and we intentionally ignore Phi nodes while
        // building per-block use/def, since Phi uses are edge-associated SSA plumbing (and can
        // form self-sustaining cycles that we don't want to treat as "real" liveness).
        let live_in_multi =
            liveness::compute_live_in_multi(func, &succs, &multi_vars, &multi_index);

        // Phi insertion.
        let (phi_dst_to_var, inserted_phi) = phi::insert_phis_for_multi_vars(
            func,
            &multi_vars,
            &is_multi,
            &multi_index,
            &preds,
            &df,
            &live_in_multi,
        );
        changed |= inserted_phi;

        // Rename.
        rename::rename_function_to_ssa(
            func,
            entry,
            &succs,
            &children,
            &is_multi,
            &multi_vars,
            &phi_dst_to_var,
        );
    }

    changed
}
