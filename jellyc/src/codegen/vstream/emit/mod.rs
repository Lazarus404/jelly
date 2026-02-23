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

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrInsn, IrModule, IrOp};
use crate::regalloc::spill::VInsn;
use crate::regalloc::{InstrInfo, VReg};

use super::super::VirtualStream;

mod arith;
mod calls;
mod consts;
mod control;
mod conv;
mod dyn_ops;
mod heap;
mod mov;

pub(super) fn emit_insn(
    ir: &IrModule,
    f: &IrFunction,
    map_func_index: &impl Fn(u32) -> u32,
    ins: &IrInsn,
    block_start: &[u32],
    this_pc: u32,
    vinsns: &mut Vec<VInsn>,
    infos: &mut Vec<InstrInfo>,
    call_windows: &mut Vec<(u32, u32, u8)>,
    const_fun_of_vreg: &mut Vec<Option<u32>>,
) -> Result<(), CompileError> {
    let mut is_direct_call = false;

    // Emit VInsn(s) for this IrOp.
    if consts::emit(map_func_index, ins, vinsns).is_some() {
        // ok
    } else if let Some(direct) =
        calls::emit(map_func_index, ins, vinsns, call_windows, const_fun_of_vreg)?
    {
        is_direct_call = direct;
    } else if mov::emit(ir, f, ins, vinsns)? {
        // ok
    } else if arith::emit(ins, vinsns).is_some() {
        // ok
    } else if conv::emit(ins, vinsns).is_some() {
        // ok
    } else if heap::emit(ins, vinsns).is_some() {
        // ok
    } else if dyn_ops::emit(ins, vinsns).is_some() {
        // ok
    } else if control::emit(block_start, ins, this_pc, vinsns)? {
        // ok
    } else {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            ins.span,
            "IR→bytecode bridge: unsupported instruction op",
        ));
    }

    // InstrInfo for regalloc:
    // derive from the authoritative `IrOp::uses/def`, with one special-case:
    // when we emit a direct `CALL`, the callee vreg is not read at runtime.
    if matches!(ins.op, IrOp::Phi { .. }) {
        return Err(CompileError::new(
            ErrorKind::Internal,
            ins.span,
            "Phi survived into emission (run phi elimination first)",
        ));
    }
    let mut uses: Vec<VReg> = ins.op.uses().into_iter().map(|v| VReg(v.0)).collect();
    if is_direct_call {
        if let IrOp::Call { callee, .. } = &ins.op {
            uses.retain(|u| u.0 != callee.0);
        }
    }
    let defs: Vec<VReg> = ins.op.def().into_iter().map(|d| VReg(d.0)).collect();
    infos.push(InstrInfo { uses, defs });

    // Update local constant-function tracking after this instruction.
    //
    // NOTE: use the op's explicit def (not the regalloc InstrInfo) so we never
    // accidentally keep a stale `ConstFun` fact for values like `Closure(...)`.
    if let Some(d) = ins.op.def() {
        let di = d.0 as usize;
        if di < const_fun_of_vreg.len() {
            const_fun_of_vreg[di] = None;
        }
    }
    match &ins.op {
        IrOp::ConstFun { dst, func_index } => {
            let di = dst.0 as usize;
            if di < const_fun_of_vreg.len() {
                const_fun_of_vreg[di] = Some(map_func_index(*func_index));
            }
        }
        IrOp::Mov { dst, src } => {
            let di = dst.0 as usize;
            let si = src.0 as usize;
            if di < const_fun_of_vreg.len() && si < const_fun_of_vreg.len() {
                const_fun_of_vreg[di] = const_fun_of_vreg[si];
            }
        }
        _ => {}
    }

    Ok(())
}

pub(super) fn into_stream(
    vinsns: Vec<VInsn>,
    infos: Vec<InstrInfo>,
    call_windows: Vec<(u32, u32, u8)>,
) -> VirtualStream {
    VirtualStream {
        vinsns,
        infos,
        call_windows,
    }
}
