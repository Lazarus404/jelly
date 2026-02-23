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

use crate::error::CompileError;
use crate::ir::{IrInsn, IrOp};
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;

use super::super::super::reg;

pub(super) fn emit(
    map_func_index: &impl Fn(u32) -> u32,
    ins: &IrInsn,
    vinsns: &mut Vec<VInsn>,
    call_windows: &mut Vec<(u32, u32, u8)>,
    const_fun_of_vreg: &mut Vec<Option<u32>>,
) -> Result<Option<bool>, CompileError> {
    match &ins.op {
        IrOp::Call {
            dst,
            callee,
            sig_id,
            arg_base,
            nargs,
        } => {
            let mut is_direct_call = false;
            if let Some(fi) = const_fun_of_vreg.get(callee.0 as usize).copied().flatten() {
                is_direct_call = true;
                vinsns.push(VInsn {
                    op: Op::Call as u8,
                    a: reg(*dst),
                    b: reg(*arg_base),
                    c: *nargs as u32,
                    imm: fi,
                });
            } else {
                vinsns.push(VInsn {
                    op: Op::CallR as u8,
                    a: reg(*dst),
                    b: reg(*callee),
                    c: *nargs as u32,
                    imm: arg_base.0,
                });
            }
            // Skip call window when using param range directly (arg_base == 0); params are
            // already pinned by setup, and adding a window would overwrite vreg_to_reg.
            if arg_base.0 != 0 {
                call_windows.push((*sig_id, arg_base.0, *nargs));
            }
            Ok(Some(is_direct_call))
        }
        IrOp::Closure {
            dst,
            func_index,
            cap_sig_id,
            cap_base,
            ncaps,
        } => {
            vinsns.push(VInsn {
                op: Op::Closure as u8,
                a: reg(*dst),
                b: reg(*cap_base),
                c: *ncaps as u32,
                imm: map_func_index(*func_index),
            });
            call_windows.push((*cap_sig_id, cap_base.0, *ncaps));
            Ok(Some(false))
        }
        IrOp::BindThis { dst, func, this } => {
            vinsns.push(VInsn {
                op: Op::BindThis as u8,
                a: reg(*dst),
                b: reg(*func),
                c: reg(*this),
                imm: 0,
            });
            Ok(Some(false))
        }
        _ => Ok(None),
    }
}
