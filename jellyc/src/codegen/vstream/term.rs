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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::IrTerminator;
use crate::jlyb::Op;
use crate::regalloc::spill::VInsn;
use crate::regalloc::{InstrInfo, VReg};

use super::super::{blk_pc, delta, reg};

pub(super) fn emit_terminator(
    block_start: &[u32],
    term: &IrTerminator,
    this_pc: u32,
    vinsns: &mut Vec<VInsn>,
    infos: &mut Vec<InstrInfo>,
    call_windows: &mut Vec<(u32, u32, u8)>,
    map_func_index: &impl Fn(u32) -> u32,
    const_fun_of_vreg: &[Option<u32>],
) -> Result<(), CompileError> {
    match term {
        IrTerminator::Ret { value } => {
            vinsns.push(VInsn {
                op: Op::Ret as u8,
                a: reg(*value),
                b: 0,
                c: 0,
                imm: 0,
            });
            infos.push(InstrInfo {
                uses: vec![VReg(value.0)],
                defs: vec![],
            });
        }
        IrTerminator::Jmp { target } => {
            let to = blk_pc(block_start, *target)?;
            vinsns.push(VInsn {
                op: Op::Jmp as u8,
                a: 0,
                b: 0,
                c: 0,
                imm: delta(this_pc, to),
            });
            infos.push(InstrInfo {
                uses: vec![],
                defs: vec![],
            });
        }
        IrTerminator::JmpIf {
            cond,
            then_tgt,
            else_tgt,
        } => {
            let then_pc = blk_pc(block_start, *then_tgt)?;
            let else_pc = blk_pc(block_start, *else_tgt)?;
            vinsns.push(VInsn {
                op: Op::JmpIf as u8,
                a: reg(*cond),
                b: 0,
                c: 0,
                imm: delta(this_pc, then_pc),
            });
            vinsns.push(VInsn {
                op: Op::Jmp as u8,
                a: 0,
                b: 0,
                c: 0,
                imm: delta(this_pc + 1, else_pc),
            });
            infos.push(InstrInfo {
                uses: vec![VReg(cond.0)],
                defs: vec![],
            });
            infos.push(InstrInfo {
                uses: vec![],
                defs: vec![],
            });
        }
        IrTerminator::SwitchKind {
            kind,
            cases,
            default,
        } => {
            let ncases = cases.len();
            if ncases > 255 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    Span::point(0),
                    "SWITCH_KIND too many cases (max 255)",
                ));
            }
            let table_end_pc = this_pc + 1 + (ncases as u32);
            let def_pc = blk_pc(block_start, *default)?;
            let def_delta: u32 = ((def_pc as i32) - (table_end_pc as i32)) as u32;
            vinsns.push(VInsn {
                op: Op::SwitchKind as u8,
                a: reg(*kind),
                b: ncases as u32,
                c: 0,
                imm: def_delta,
            });
            infos.push(InstrInfo {
                uses: vec![VReg(kind.0)],
                defs: vec![],
            });
            for (k, tgt) in cases {
                let tgt_pc = blk_pc(block_start, *tgt)?;
                let d: u32 = ((tgt_pc as i32) - (table_end_pc as i32)) as u32;
                vinsns.push(VInsn {
                    op: Op::CaseKind as u8,
                    a: *k as u32,
                    b: 0,
                    c: 0,
                    imm: d,
                });
                infos.push(InstrInfo {
                    uses: vec![],
                    defs: vec![],
                });
            }
        }
        IrTerminator::TailCall {
            callee,
            sig_id,
            arg_base,
            nargs,
        } => {
            // Skip call window when using param range directly (arg_base == 0); params are
            // already pinned by setup, and adding a window would overwrite vreg_to_reg[0].
            if arg_base.0 != 0 {
                call_windows.push((*sig_id, arg_base.0, *nargs));
            }
            let (op, a, b, c, imm) = if let Some(fi) =
                const_fun_of_vreg.get(callee.0 as usize).copied().flatten()
            {
                (
                    Op::TailCall as u8,
                    0u32,
                    reg(*arg_base),
                    *nargs as u32,
                    map_func_index(fi),
                )
            } else {
                (
                    Op::TailCallR as u8,
                    0u32,
                    reg(*callee),
                    *nargs as u32,
                    arg_base.0,
                )
            };
            vinsns.push(VInsn { op, a, b, c, imm });
            let mut uses: Vec<VReg> = (0..*nargs as usize)
                .map(|i| VReg(arg_base.0 + i as u32))
                .collect();
            if op == Op::TailCallR as u8 {
                uses.push(VReg(callee.0));
            }
            infos.push(InstrInfo {
                uses,
                defs: vec![],
            });
        }
        IrTerminator::Unreachable => {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                Span::point(0),
                "IR→bytecode bridge: unreachable terminator not supported",
            ));
        }
    }
    Ok(())
}
