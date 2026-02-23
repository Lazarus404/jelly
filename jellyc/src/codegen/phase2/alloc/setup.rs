use std::collections::HashSet;

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule};
use crate::jlyb::Op;
use crate::typectx::T_DYNAMIC;

use super::super::super::VirtualStream;
use super::super::call_windows::validate_call_windows;
use super::state::RegState;

pub(super) struct Setup {
    pub(super) state: RegState,
    pub(super) call_callee_vregs: HashSet<u32>,
}

pub(super) fn setup(
    ir: &IrModule,
    f: &IrFunction,
    vs: &VirtualStream,
) -> Result<Setup, CompileError> {
    let call_windows = &vs.call_windows;
    let vinsns = &vs.vinsns;
    let _infos = &vs.infos;

    // Collect CallR/TailCallR callee vregs so we can avoid spilling them (spill/reload can corrupt
    // closure values when pop order is wrong or stack is shared).
    let call_callee_vregs: HashSet<u32> = vinsns
        .iter()
        .filter(|vi| vi.op == Op::CallR as u8 || vi.op == Op::TailCallR as u8)
        .map(|vi| vi.b)
        .collect();

    // Sanity-check call windows: the IR relies on arg-window vregs having the exact
    // types described by the signature used to pin the window.
    validate_call_windows(ir, f, call_windows)?;

    let mut state = RegState::new(f.vreg_types.len());

    // Preserve ABI: params are fixed at 0..param_count-1.
    if (f.param_count as usize) > f.vreg_types.len() {
        return Err(CompileError::new(
            ErrorKind::Internal,
            crate::ast::Span::point(0),
            "bad param_count",
        ));
    }
    for i in 0..(f.param_count as usize) {
        state.vreg_to_reg[i] = i as u8;
    }
    state
        .reg_types
        .extend_from_slice(&f.vreg_types[..(f.param_count as usize)]);
    state.base = f.param_count as u16;

    // Add capture slots at the beginning (after params) so spill regs never overlap.
    state.cap_start = if f.cap_vregs.is_empty() {
        0u32
    } else {
        let param_count = f.param_count as u32;
        if state.reg_types.len() + f.cap_vregs.len() > 256 {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "register allocation exceeded 256 regs",
            ));
        }
        for v in &f.cap_vregs {
            let tid = f.vreg_types.get(v.0 as usize).copied().unwrap_or(T_DYNAMIC);
            state.reg_types.push(tid);
        }
        for (i, v) in f.cap_vregs.iter().enumerate() {
            let idx = v.0 as usize;
            if idx >= state.vreg_to_reg.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "capture vreg out of range",
                ));
            }
            state.vreg_to_reg[idx] = (param_count as u8).checked_add(i as u8).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "capture slot overflow",
                )
            })?;
        }
        state.base = (state.base as u32 + f.cap_vregs.len() as u32) as u16;
        param_count
    };

    // Pre-allocate f64 escape vector sized to param_count.
    state.f64_escape_per_param = vec![0; f.param_count as usize];

    Ok(Setup {
        state,
        call_callee_vregs,
    })
}
