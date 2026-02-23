use std::collections::HashSet;

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule, TypeId};
use crate::jlyb::TypeKind;
use crate::regalloc::{self, InstrInfo, SpillPolicy, VReg};
pub(super) fn allocate_type_class(
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
