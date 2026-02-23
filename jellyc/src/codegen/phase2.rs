use std::collections::HashSet;

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule, TypeId};
use crate::jlyb::{Op, TypeKind};
use crate::regalloc::{self, InstrInfo, SpillPolicy, VReg};
use crate::typectx::{T_DYNAMIC, T_F64};

use super::{Allocation, VirtualStream, pin_call_arg_blocks_and_validate};

fn allocate_type_class(
    ir: &IrModule,
    f: &IrFunction,
    infos: &[InstrInfo],
    tid: TypeId,
    globals: &[u32],
    allow_multi_def_global: &[bool],
    base: &mut u16,
    cap_reserve: u16,
    call_callee_vregs: &HashSet<u32>,
    dyn_spill_reload_regs: &mut Option<[u8; 3]>,
    vreg_to_reg: &mut [u8],
    vreg_to_spill: &mut [Option<u32>],
    reg_types: &mut Vec<u32>,
) -> Result<(), CompileError> {
    if globals.is_empty() {
        return Ok(());
    }

    let mut g2l: Vec<Option<u32>> = vec![None; f.vreg_types.len()];
    for (li, &gv) in globals.iter().enumerate() {
        g2l[gv as usize] = Some(li as u32);
    }

    let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
    for ins in infos {
        let mut uses = Vec::new();
        let mut defs = Vec::new();
        for &u in &ins.uses {
            if let Some(li) = g2l[u.0 as usize] {
                uses.push(VReg(li));
            }
        }
        for &d in &ins.defs {
            if let Some(li) = g2l[d.0 as usize] {
                defs.push(VReg(li));
            }
        }
        cls_instrs.push(InstrInfo { uses, defs });
    }

    let mut allow_multi_def_local: Vec<bool> = vec![false; globals.len()];
    for (li, &gv) in globals.iter().enumerate() {
        allow_multi_def_local[li] = allow_multi_def_global[gv as usize];
    }

    let remaining = 256u16
        .checked_sub(*base)
        .and_then(|r| r.checked_sub(cap_reserve))
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "register allocation exceeded 256 regs",
            )
        })?;

    let tid_is_dyn = ir
        .types
        .get(tid as usize)
        .map_or(false, |te| te.kind == TypeKind::Dynamic);

    // Spill policy: spill stack is boxed-only. Only `Dynamic` vregs may be spilled.
    //
    // Never spill CallR callees (closures) - even for Dynamic, spill/reload can corrupt them.
    let spillable: Vec<bool> = globals
        .iter()
        .map(|&gv| tid_is_dyn && !call_callee_vregs.contains(&gv))
        .collect();

    // Reserve 3 regs for spill reload only for the Dynamic class (when spilling is permitted).
    let (spill_policy, num_pregs) = if tid_is_dyn {
        if remaining <= 3 {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "register allocation exceeded 256 regs (need spill reload regs)",
            ));
        }
        (SpillPolicy::Allow, remaining - 3)
    } else {
        (SpillPolicy::Forbid, remaining)
    };

    let alloc = regalloc::lsra_allocate(
        num_pregs,
        globals.len() as u32,
        &cls_instrs,
        &spillable,
        spill_policy,
        Some(&allow_multi_def_local),
    )
    .map_err(|e| match e {
        regalloc::AllocError::OutOfPhysicalRegsNoSpill => {
            let name = f.name.clone().unwrap_or_else(|| "<anon>".to_string());
            CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                format!(
                    "register pressure too high in function '{name}' for type class tid={tid} (typed regs are non-spillable)"
                ),
            )
        }
        regalloc::AllocError::UsedButNeverDefined { vreg } => {
            let name = f.name.clone().unwrap_or_else(|| "<anon>".to_string());
            let gv = globals.get(vreg.0 as usize).copied().unwrap_or(u32::MAX);
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!(
                    "LSRA allocation failed in function '{name}' tid={tid}: local vreg {} (global vreg {}) used but never defined",
                    vreg.0, gv
                ),
            )
        }
        _ => CompileError::new(
            ErrorKind::Internal,
            crate::ast::Span::point(0),
            format!("LSRA allocation failed: {:?}", e),
        ),
    })?;

    let used = alloc.used_pregs;
    let spill_reload_base = *base + used;
    let spill_reload_count = if tid_is_dyn && alloc.used_spill_slots > 0 {
        let regs = [
            spill_reload_base as u8,
            spill_reload_base as u8 + 1,
            spill_reload_base as u8 + 2,
        ];
        if dyn_spill_reload_regs.replace(regs).is_some() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "multiple Dynamic spill reload reg allocations (internal bug)",
            ));
        }
        3u16
    } else {
        0u16
    };

    if (*base as u32) + (used as u32) + (spill_reload_count as u32) > 256 {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            crate::ast::Span::point(0),
            "register allocation exceeded 256 regs",
        ));
    }

    let need = *base as usize + used as usize + spill_reload_count as usize;
    if reg_types.len() < need {
        reg_types.resize(need, 0);
    }
    for i in 0..used {
        reg_types[*base as usize + i as usize] = tid;
    }
    if spill_reload_count > 0 {
        for i in 0..spill_reload_count {
            reg_types[spill_reload_base as usize + i as usize] = tid;
        }
    }

    for (li, &gv) in globals.iter().enumerate() {
        if let Some(p) = alloc.vreg_to_preg[li] {
            vreg_to_reg[gv as usize] = (*base + p.0) as u8;
        } else {
            vreg_to_reg[gv as usize] = spill_reload_base as u8;
            vreg_to_spill[gv as usize] = alloc.vreg_to_spill[li];
        }
    }

    *base += used + spill_reload_count;
    Ok(())
}

pub(super) fn allocate_registers(ir: &IrModule, f: &IrFunction, vs: &VirtualStream) -> Result<Allocation, CompileError> {
    let call_windows = &vs.call_windows;
    let vinsns = &vs.vinsns;
    let infos = &vs.infos;

    // Collect CallR callee vregs so we can avoid spilling them (spill/reload can corrupt
    // closure values when pop order is wrong or stack is shared).
    let call_callee_vregs: HashSet<u32> = vinsns
        .iter()
        .filter(|vi| vi.op == Op::CallR as u8)
        .map(|vi| vi.b)
        .collect();

    // Sanity-check call windows: the IR relies on arg-window vregs having the exact
    // types described by the signature used to pin the window.
    for &(sig_id, arg_base_v, nargs) in call_windows {
        let sig = ir
            .sigs
            .get(sig_id as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "bad fun sig id"))?;
        if sig.args.len() != (nargs as usize) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "CALLR arg count does not match signature",
            ));
        }
        for i in 0..(nargs as u32) {
            let vi = (arg_base_v + i) as usize;
            if vi >= f.vreg_types.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "CALLR arg vreg out of range",
                ));
            }
            let vt = f.vreg_types[vi];
            let st = sig.args[i as usize];
            if vt != st {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("CALLR arg window type mismatch (vreg {}: {} != {})", vi, vt, st),
                ));
            }
        }
    }

    let mut vreg_to_reg: Vec<u8> = vec![0; f.vreg_types.len()];
    let mut vreg_to_spill: Vec<Option<u32>> = vec![None; f.vreg_types.len()];
    let mut reg_types: Vec<u32> = Vec::new();
    let mut base: u16;
    let mut dyn_spill_reload_regs: Option<[u8; 3]> = None;

    // Preserve ABI: params are fixed at 0..param_count-1.
    if (f.param_count as usize) > f.vreg_types.len() {
        return Err(CompileError::new(
            ErrorKind::Internal,
            crate::ast::Span::point(0),
            "bad param_count",
        ));
    }
    for i in 0..(f.param_count as usize) {
        vreg_to_reg[i] = i as u8;
    }
    reg_types.extend_from_slice(&f.vreg_types[..(f.param_count as usize)]);
    base = f.param_count as u16;

    // Add capture slots at the beginning (after params) so spill regs never overlap.
    let cap_start = if f.cap_vregs.is_empty() {
        0u32
    } else {
        let param_count = f.param_count as u32;
        if reg_types.len() + f.cap_vregs.len() > 256 {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "register allocation exceeded 256 regs",
            ));
        }
        for v in &f.cap_vregs {
            let tid = f.vreg_types.get(v.0 as usize).copied().unwrap_or(T_DYNAMIC);
            reg_types.push(tid);
        }
        for (i, v) in f.cap_vregs.iter().enumerate() {
            let idx = v.0 as usize;
            if idx >= vreg_to_reg.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "capture vreg out of range",
                ));
            }
            vreg_to_reg[idx] = (param_count as u8).checked_add(i as u8).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "capture slot overflow",
                )
            })?;
        }
        base = (base as u32 + f.cap_vregs.len() as u32) as u16;
        param_count
    };

    let mut allow_multi_def_global: Vec<bool> = vec![false; f.vreg_types.len()];
    let mut allow_live_in_global: Vec<bool> = vec![false; f.vreg_types.len()];
    let mut def_counts: Vec<u32> = vec![0; f.vreg_types.len()];
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

    // Mark vregs that are part of a call arg window; we'll pin them to the
    // per-signature arg blocks instead of allocating them normally.
    let mut is_arg_vreg: Vec<bool> = vec![false; f.vreg_types.len()];
    for &(_sig_id, arg_base_v, nargs) in call_windows {
        if nargs == 0 {
            continue;
        }
        let end = arg_base_v
            .checked_add((nargs as u32).saturating_sub(1))
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "call arg window overflow"))?;
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
    let cap_reserve: u16 = (f.cap_vregs.len() as u16) + arg_block_size;

    for tid in tids {
        let mut globals: Vec<u32> = Vec::new();
        for (i, &t) in f.vreg_types.iter().enumerate() {
            if i >= (f.param_count as usize) && !is_arg_vreg[i] && t == tid && live_vregs.contains(&(i as u32)) {
                globals.push(i as u32);
            }
        }
        if globals.is_empty() {
            continue;
        }

        allocate_type_class(
            ir,
            f,
            infos,
            tid,
            &globals,
            // This slice serves two purposes:
            // - permit vregs with multiple defs (non-SSA) to be allocated
            // - permit a small set of known-implicit live-ins to be treated as defined at entry
            &allow_special_global,
            &mut base,
            cap_reserve,
            &call_callee_vregs,
            &mut dyn_spill_reload_regs,
            &mut vreg_to_reg,
            &mut vreg_to_spill,
            &mut reg_types,
        )?;
    }

    // Spill invariant: only Dynamic vregs may be spilled (spill stack is boxed-only).
    for (gv, slot) in vreg_to_spill.iter().enumerate() {
        if slot.is_none() {
            continue;
        }
        let tid = f.vreg_types.get(gv).copied().unwrap_or(T_DYNAMIC);
        let is_dyn = ir
            .types
            .get(tid as usize)
            .map_or(false, |te| te.kind == TypeKind::Dynamic);
        if !is_dyn {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!("spill invariant violated: spilled vreg {} has non-Dynamic tid {}", gv, tid),
            ));
        }
    }

    // Pinned-window invariant: vregs pinned to ABI windows (call args, closure capture args,
    // and callee capture slots) must never be spilled.
    for (i, &pinned) in is_arg_vreg.iter().enumerate() {
        if pinned && vreg_to_spill.get(i).is_some_and(|s| s.is_some()) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!("pinned window vreg {} was spilled (should be excluded from allocation)", i),
            ));
        }
    }

    // Verify CallR callees were not spilled (spill/reload can corrupt closures).
    for &gv in &call_callee_vregs {
        if (gv as usize) < vreg_to_spill.len() && vreg_to_spill[gv as usize].is_some() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!("CallR callee vreg {} was spilled (spillable should prevent this)", gv),
            ));
        }
    }

    // Allocate arg blocks per signature and pin each call's arg-window vregs.
    // This is a pure ABI/layout step; we keep it separate from LSRA allocation.
    let _arg_block_start = pin_call_arg_blocks_and_validate(
        ir,
        call_windows,
        &call_callee_vregs,
        vinsns,
        &mut vreg_to_reg,
        &vreg_to_spill,
        &mut reg_types,
    )?;

    // Fix: never map a non-Object vreg to a reg that holds an Object param.
    // (IR can produce AddF64 dst=param when param is incorrectly reused; this avoids VM validation failure.)
    let mut f64_escape_per_param: Vec<u8> = vec![0; f.param_count as usize];
    for param_idx in 0..(f.param_count as usize) {
        let param_tid = f.vreg_types.get(param_idx).copied().unwrap_or(0);
        let param_is_object = ir
            .types
            .get(param_tid as usize)
            .map_or(false, |te| te.kind == crate::jlyb::TypeKind::Object);
        if !param_is_object {
            continue;
        }
        let param_reg = vreg_to_reg.get(param_idx).copied().unwrap_or(0);
        for (v, &tid) in f.vreg_types.iter().enumerate() {
            if vreg_to_reg.get(v).copied() == Some(param_reg) && tid != param_tid {
                if reg_types.len() >= 256 {
                    return Err(CompileError::new(
                        ErrorKind::Codegen,
                        crate::ast::Span::point(0),
                        "register allocation exceeded 256 regs (fixing param overlap)",
                    ));
                }
                reg_types.push(tid);
                vreg_to_reg[v] = (reg_types.len() - 1) as u8;
            }
        }
    }
    // Handle AddF64 dst=param vreg when param is Object: use escape regs (Object can't hold F64).
    for v in 0..(f.param_count as usize) {
        let param_tid = f.vreg_types.get(v).copied().unwrap_or(0);
        let param_is_object = ir
            .types
            .get(param_tid as usize)
            .map_or(false, |te| te.kind == crate::jlyb::TypeKind::Object);
        if param_is_object && vinsns.iter().any(|vi| vi.op == Op::AddF64 as u8 && (vi.a as usize) == v) {
            if reg_types.len() >= 256 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs (F64 escape)",
                ));
            }
            reg_types.push(T_F64);
            f64_escape_per_param[v] = (reg_types.len() - 1) as u8;
        }
    }

    // Typed-slot invariant: a physical register never changes type.
    // Every vreg must map to a preg whose static type matches.
    for (i, &tid) in f.vreg_types.iter().enumerate() {
        if !live_vregs.contains(&(i as u32)) {
            continue; // dead vreg (eg after DCE), skip type check
        }
        let r = vreg_to_reg
            .get(i)
            .copied()
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "vreg_to_reg out of range"))?
            as usize;
        let rt = reg_types.get(r).copied().ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "vreg mapped to missing reg_types entry",
            )
        })?;
        if rt != tid {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                format!(
                    "typed slot invariant violated: vreg {} (tid={}) mapped to reg {} (tid={})",
                    i, tid, r, rt
                ),
            ));
        }
    }

    // Compute last-def PC per vreg (for spill pop order)
    let mut last_def_pc: Vec<u32> = vec![0; f.vreg_types.len()];
    for (pc, info) in infos.iter().enumerate() {
        for &d in &info.defs {
            if (d.0 as usize) < last_def_pc.len() {
                last_def_pc[d.0 as usize] = pc as u32;
            }
        }
    }

    Ok(Allocation {
        vreg_to_reg,
        vreg_to_spill,
        reg_types,
        cap_start,
        spill_reload_regs: dyn_spill_reload_regs.unwrap_or([0u8, 0u8, 0u8]),
        last_def_pc,
        f64_escape_per_param,
    })
}

