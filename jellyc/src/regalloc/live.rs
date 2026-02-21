/**
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

// Live interval computation for linear-scan register allocation.
// Extracted from regalloc per docs/compiler_architecture.md.

use super::{AllocError, InstrInfo, VReg};
use crate::ir::{IrFunction, IrOp, IrTerminator};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LiveInterval {
    pub vreg: VReg,
    pub start: u32,
    pub end: u32, // inclusive
}

/// Compute live intervals from a linearized instruction stream.
/// Each vreg gets [def_pos, last_use_pos] (inclusive).
///
/// `allow_multi_def`: if set, vregs with multiple defs (eg phi join targets)
/// are allowed; the interval spans from first def to last use.
pub fn build_intervals(
    num_vregs: u32,
    instrs: &[InstrInfo],
    allow_multi_def: Option<&[bool]>,
) -> Result<Vec<LiveInterval>, AllocError> {
    if let Some(a) = allow_multi_def {
        if a.len() != num_vregs as usize {
            return Err(AllocError::IntervalInternalBug);
        }
    }
    let mut def_pos: Vec<Option<u32>> = vec![None; num_vregs as usize];
    let mut end_pos: Vec<Option<u32>> = vec![None; num_vregs as usize];

    for (pc, ins) in instrs.iter().enumerate() {
        let pos = pc as u32;
        for &d in &ins.defs {
            if d.0 >= num_vregs {
                return Err(AllocError::VRegOutOfRange { vreg: d, vregs: num_vregs });
            }
            if def_pos[d.0 as usize].is_some() {
                let allow = allow_multi_def.map(|a| a[d.0 as usize]).unwrap_or(false);
                if !allow {
                    return Err(AllocError::MultipleDefs { vreg: d });
                }
            }
            if def_pos[d.0 as usize].is_none() {
                def_pos[d.0 as usize] = Some(pos);
            }
            // A def is also a live position (even if never used).
            end_pos[d.0 as usize] = Some(end_pos[d.0 as usize].unwrap_or(pos).max(pos));
        }
        for &u in &ins.uses {
            if u.0 >= num_vregs {
                return Err(AllocError::VRegOutOfRange { vreg: u, vregs: num_vregs });
            }
            let dpos = def_pos[u.0 as usize].ok_or(AllocError::UseBeforeDef { vreg: u })?;
            let _ = dpos;
            let cur = end_pos[u.0 as usize].unwrap_or(pos);
            end_pos[u.0 as usize] = Some(cur.max(pos));
        }
    }

    let mut out = Vec::new();
    for v in 0..num_vregs {
        if let Some(s) = def_pos[v as usize] {
            let e = end_pos[v as usize].ok_or(AllocError::IntervalInternalBug)?;
            out.push(LiveInterval { vreg: VReg(v), start: s, end: e });
        }
    }
    Ok(out)
}

/// Compute live intervals from an IR function (blocks in order).
/// Useful for IR-level analysis. Phi nodes must be eliminated first.
pub fn compute_live_intervals(func: &IrFunction) -> Result<Vec<LiveInterval>, AllocError> {
    let num_vregs = func.vreg_types.len() as u32;
    let instrs = linearize_to_instr_info(func);
    build_intervals(num_vregs, &instrs, None)
}

fn op_to_instr_info(op: &IrOp) -> InstrInfo {
    let uses: Vec<VReg> = op.uses().iter().map(|v| VReg(v.0)).collect();
    let defs: Vec<VReg> = op.def().into_iter().map(|v| VReg(v.0)).collect();
    InstrInfo { uses, defs }
}

fn term_to_instr_info(term: &IrTerminator) -> Vec<InstrInfo> {
    match term {
        IrTerminator::Ret { value } => vec![InstrInfo {
            uses: vec![VReg(value.0)],
            defs: vec![],
        }],
        IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {
            vec![InstrInfo { uses: vec![], defs: vec![] }]
        }
        IrTerminator::JmpIf { cond, .. } => vec![
            InstrInfo {
                uses: vec![VReg(cond.0)],
                defs: vec![],
            },
            InstrInfo { uses: vec![], defs: vec![] },
        ],
        IrTerminator::SwitchKind { kind, cases, .. } => {
            let mut v = vec![InstrInfo {
                uses: vec![VReg(kind.0)],
                defs: vec![],
            }];
            for _ in cases {
                v.push(InstrInfo { uses: vec![], defs: vec![] });
            }
            v
        }
    }
}

/// Linearize an IR function to a vector of instruction information.
fn linearize_to_instr_info(func: &IrFunction) -> Vec<InstrInfo> {
    let mut out = Vec::new();
    for block in &func.blocks {
        for ins in &block.insns {
            out.push(op_to_instr_info(&ins.op));
        }
        out.extend(term_to_instr_info(&block.term));
    }
    out
}
