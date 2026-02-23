use std::collections::HashSet;

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule, TypeId};
use crate::jlyb::TypeKind;
use crate::regalloc::InstrInfo;
use crate::typectx::T_DYNAMIC;

pub(super) struct Marks {
    pub(super) allow_special_global: Vec<bool>,
    pub(super) is_arg_vreg: Vec<bool>,
    pub(super) live_vregs: HashSet<u32>,
    pub(super) tids: Vec<TypeId>,
    pub(super) cap_reserve: u16,
}

pub(super) fn compute_allow_special_global(
    infos: &[InstrInfo],
    nvregs: usize,
    call_callee_vregs: &HashSet<u32>,
) -> Vec<bool> {
    let mut allow_multi_def_global: Vec<bool> = vec![false; nvregs];
    let mut allow_live_in_global: Vec<bool> = vec![false; nvregs];
    let mut def_counts: Vec<u32> = vec![0; nvregs];
    for ins in infos {
        for &d in &ins.defs {
            if (d.0 as usize) < def_counts.len() {
                def_counts[d.0 as usize] += 1;
            }
        }
    }
    for (i, &c) in def_counts.iter().enumerate() {
        if c > 1 {
            allow_multi_def_global[i] = true;
        }
        // Some vregs are implicitly live-in at function entry (e.g. the self/callee binding
        // used for recursion). We only allow this for vregs that are actually used as CALLR
        // callees and have no explicit defs in the instruction stream.
        if c == 0 && call_callee_vregs.contains(&(i as u32)) {
            allow_live_in_global[i] = true;
        }
    }
    let mut allow_special_global: Vec<bool> = allow_multi_def_global.clone();
    for (i, &li) in allow_live_in_global.iter().enumerate() {
        if li {
            allow_special_global[i] = true;
        }
    }
    allow_special_global
}

pub(super) fn compute_is_arg_vreg(
    f: &IrFunction,
    call_windows: &[(u32, u32, u8)],
) -> Result<Vec<bool>, CompileError> {
    // Mark vregs that are part of a call arg window; we'll pin them to the
    // per-signature arg blocks instead of allocating them normally.
    let mut is_arg_vreg: Vec<bool> = vec![false; f.vreg_types.len()];
    for &(_sig_id, arg_base_v, nargs) in call_windows {
        if nargs == 0 {
            continue;
        }
        let end = arg_base_v
            .checked_add((nargs as u32).saturating_sub(1))
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call arg window overflow",
                )
            })?;
        if (end as usize) >= f.vreg_types.len() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "call arg window out of vreg range",
            ));
        }
        for v in arg_base_v..(arg_base_v + (nargs as u32)) {
            is_arg_vreg[v as usize] = true;
        }
    }
    // Also exclude closure capture-slot vregs from normal allocation; we pin them to the
    // end of the frame after allocating internal call arg blocks.
    for v in &f.cap_vregs {
        if (v.0 as usize) < is_arg_vreg.len() {
            is_arg_vreg[v.0 as usize] = true;
        }
    }
    Ok(is_arg_vreg)
}

pub(super) fn compute_live_vregs(infos: &[InstrInfo], nvregs: usize) -> HashSet<u32> {
    // Only allocate vregs that appear in the instruction stream (opt passes may leave
    // orphan vregs in vreg_types that are never used/defined).
    let mut live_vregs: HashSet<u32> = HashSet::new();
    for ins in infos {
        for &u in &ins.uses {
            live_vregs.insert(u.0);
        }
        for &d in &ins.defs {
            live_vregs.insert(d.0);
        }
    }
    // Keep the set bounded to the vreg table length.
    live_vregs.retain(|&v| (v as usize) < nvregs);
    live_vregs
}

pub(super) fn compute_tids(ir: &IrModule, f: &IrFunction) -> Vec<TypeId> {
    let mut tids: Vec<TypeId> = f.vreg_types.iter().copied().collect();
    tids.sort();
    tids.dedup();
    // Allocate Dynamic last so other type classes reserve their slice first.
    tids.sort_by_key(|&tid| {
        let is_dyn = ir
            .types
            .get(tid as usize)
            .map_or(false, |te| te.kind == TypeKind::Dynamic);
        (is_dyn, tid)
    });
    tids
}

pub(super) fn compute_cap_reserve(
    ir: &IrModule,
    f: &IrFunction,
    call_windows: &[(u32, u32, u8)],
) -> u16 {
    // Reserve space for arg blocks + capture slots so spill reload regs never overlap.
    // Compute once per function (avoid rebuilding sets inside the type-class loop).
    let arg_block_size: u16 = {
        let mut sig_ids: Vec<u32> = call_windows.iter().map(|(sig_id, _, _)| *sig_id).collect();
        sig_ids.sort_unstable();
        sig_ids.dedup();
        sig_ids
            .into_iter()
            .filter_map(|sig_id| ir.sigs.get(sig_id as usize))
            .map(|sig| sig.args.len() as u16)
            .sum()
    };
    (f.cap_vregs.len() as u16) + arg_block_size
}

pub(super) fn compute_marks(
    ir: &IrModule,
    f: &IrFunction,
    call_windows: &[(u32, u32, u8)],
    infos: &[InstrInfo],
    call_callee_vregs: &HashSet<u32>,
) -> Result<Marks, CompileError> {
    let nvregs = f.vreg_types.len();
    let allow_special_global = compute_allow_special_global(infos, nvregs, call_callee_vregs);
    let is_arg_vreg = compute_is_arg_vreg(f, call_windows)?;
    let live_vregs = compute_live_vregs(infos, nvregs);
    let tids = compute_tids(ir, f);
    let cap_reserve = compute_cap_reserve(ir, f, call_windows);

    // Avoid a footgun: treat missing types as Dynamic in later checks.
    let _ = T_DYNAMIC;

    Ok(Marks {
        allow_special_global,
        is_arg_vreg,
        live_vregs,
        tids,
        cap_reserve,
    })
}
