use std::collections::{HashMap, HashSet};

use crate::error::{CompileError, ErrorKind};
use crate::ir::IrModule;
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;
use crate::typectx::T_DYNAMIC;

pub(super) fn pin_call_arg_blocks_and_validate(
    ir: &IrModule,
    call_windows: &[(u32, u32, u8)], // (sig_id, arg_base_vreg, nargs)
    call_callee_vregs: &HashSet<u32>,
    vinsns: &[VInsn],
    vreg_to_reg: &mut [u8],
    vreg_to_spill: &[Option<u32>],
    reg_types: &mut Vec<u32>,
) -> Result<HashMap<u32, u8>, CompileError> {
    let mut arg_block_start: HashMap<u32, u8> = HashMap::new();
    if call_windows.is_empty() {
        return Ok(arg_block_start);
    }

    // Arg blocks must not start at reg 0 when params occupy 0..param_count-1.
    // Otherwise F64 arg vregs could be pinned to reg 0 (Object param).
    if reg_types.is_empty() {
        reg_types.push(T_DYNAMIC);
    }

    // Arg block must not overlap CallR callee regs (pinning args would overwrite the closure).
    let max_callee_reg: u8 = call_callee_vregs
        .iter()
        .filter_map(|&gv| ((gv as usize) < vreg_to_reg.len()).then(|| vreg_to_reg[gv as usize]))
        .max()
        .unwrap_or(0);
    while (reg_types.len() as u8) <= max_callee_reg {
        reg_types.push(T_DYNAMIC);
    }

    let mut need_sigs: Vec<u32> = call_windows.iter().map(|(sig_id, _, _)| *sig_id).collect();
    need_sigs.sort();
    need_sigs.dedup();
    for sig_id in need_sigs {
        let sig = ir.sigs.get(sig_id as usize).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "bad fun sig id",
            )
        })?;
        if reg_types.len() + sig.args.len() > 256 {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "register allocation exceeded 256 regs",
            ));
        }
        let start = reg_types.len() as u8;
        for &tid in &sig.args {
            reg_types.push(tid);
        }
        arg_block_start.insert(sig_id, start);
    }

    for &(sig_id, arg_base_v, nargs) in call_windows {
        if nargs == 0 {
            continue;
        }
        let start = *arg_block_start.get(&sig_id).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "missing arg block",
            )
        })?;
        for i in 0..(nargs as u32) {
            let v = arg_base_v + i;
            if (v as usize) >= vreg_to_reg.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call arg vreg out of range",
                ));
            }
            let dst = start.checked_add(i as u8).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "arg block overflow",
                )
            })?;
            vreg_to_reg[v as usize] = dst;
        }
    }

    // Validate pinned arg windows after pinning:
    // - each arg vreg maps to the expected physical reg in its signature's arg block
    // - each such reg has the signature's static type in `reg_types`
    // - pinned arg vregs are never spilled
    for &(sig_id, arg_base_v, nargs) in call_windows {
        if nargs == 0 {
            continue;
        }
        let sig = ir.sigs.get(sig_id as usize).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "bad fun sig id",
            )
        })?;
        if sig.args.len() != (nargs as usize) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "call window arg count does not match signature",
            ));
        }
        let start = *arg_block_start.get(&sig_id).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "missing arg block",
            )
        })?;
        for i in 0..(nargs as u32) {
            let v = (arg_base_v + i) as usize;
            let expected_r = start.checked_add(i as u8).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "arg block overflow",
                )
            })? as usize;
            let r = *vreg_to_reg.get(v).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call arg vreg out of range",
                )
            })? as usize;
            if r != expected_r {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: arg vreg {} mapped to reg {} but expected {} (sig_id={})",
                        v, r, expected_r, sig_id
                    ),
                ));
            }
            if vreg_to_spill.get(v).is_some_and(|s| s.is_some()) {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: pinned arg vreg {} was spilled",
                        v
                    ),
                ));
            }
            let rt = *reg_types.get(r).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call ABI invariant violated: arg reg missing reg_types entry",
                )
            })?;
            let expected_tid = sig.args[i as usize];
            if rt != expected_tid {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: arg reg {} has tid {} but signature expects {} (sig_id={})",
                        r, rt, expected_tid, sig_id
                    ),
                ));
            }
        }
    }

    // CALLR/TailCallR-specific ABI invariant: the callee register must not overlap the argument window.
    for vi in vinsns
        .iter()
        .filter(|vi| vi.op == Op::CallR as u8 || vi.op == Op::TailCallR as u8)
    {
        let callee_v = vi.b as usize;
        let arg_base_v = vi.imm as usize;
        let nargs = vi.c as usize;
        if callee_v >= vreg_to_reg.len() {
            continue;
        }
        let callee_r = vreg_to_reg[callee_v];
        for i in 0..nargs {
            let av = arg_base_v.saturating_add(i);
            if av >= vreg_to_reg.len() {
                break;
            }
            let ar = vreg_to_reg[av];
            if ar == callee_r {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: CALLR callee reg {} overlaps arg window reg (callee_vreg={} arg_vreg={})",
                        callee_r, callee_v, av
                    ),
                ));
            }
        }
    }

    Ok(arg_block_start)
}
