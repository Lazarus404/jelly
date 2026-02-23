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
// Insert SpillPush/SpillPop around instructions for spilled Dynamic vregs.
//
// The VM spill stack is LIFO. We push on def, pop on use. For multiple spilled
// uses in one instruction, we pop in reverse order of last-def (most recently
// pushed first). We use reserved spill-reload registers (Dynamic) for the
// pop target and for the def output before push.
use crate::jlyb::{Insn, Op};
use crate::regalloc::InstrInfo;

/// Virtual instruction with full vreg indices (u32). Avoids truncation when >256 vregs.
#[derive(Clone, Copy)]
pub struct VInsn {
    pub op: u8,
    pub a: u32,
    pub b: u32,
    pub c: u32,
    pub imm: u32,
}

/// Insert SpillPush/SpillPop into the instruction stream.
///
/// - `vinsns`: virtual instructions (operands are vreg indices, u32 for >256 vregs)
/// - `infos`: per-instruction uses/defs (global vreg indices)
/// - `vreg_to_reg`: maps global vreg -> physical reg (for spilled vregs, this
///   is the spill-reload reg we use; caller must set it)
/// - `vreg_to_spill`: maps global vreg -> spill slot index (None if not spilled)
/// - `spill_reload_regs`: the two reserved spill-reload registers (both `Dynamic`)
/// - `last_def_pc`: for each vreg, the PC of its last definition (used to order pops)
///
/// When multiple spilled uses appear in one instruction, we pop into `spill_reload_regs`
/// in LIFO order. At most 3 spilled uses per instruction are supported.
pub fn insert_spill_ops(
    vinsns: &[VInsn],
    infos: &[InstrInfo],
    vreg_to_reg: &[u8],
    vreg_to_spill: &[Option<u32>],
    spill_reload_regs: [u8; 3],
    last_def_pc: &[u32],
    f64_escape_per_param: Option<&[u8]>,
) -> Vec<Insn> {
    let mut out: Vec<Insn> = Vec::new();
    for (pc, (vi, info)) in vinsns.iter().zip(infos.iter()).enumerate() {
        let _pc = pc;

        // Collect spilled uses, sorted by last_def_pc descending (pop most recent first)
        let mut spilled_uses: Vec<(u32, u32)> = Vec::new(); // (vreg, slot)
        for &u in &info.uses {
            let gv = u.0;
            if let Some(slot) = vreg_to_spill.get(gv as usize).and_then(|x| *x) {
                spilled_uses.push((gv, slot));
            }
        }
        // Deduplicate by vreg (same vreg might appear twice in uses - e.g AddI32 a,a,b)
        let mut seen: std::collections::HashSet<u32> = std::collections::HashSet::new();
        spilled_uses.retain(|(gv, _)| seen.insert(*gv));
        // Sort by last_def descending so we pop in correct LIFO order
        spilled_uses.sort_by_key(|(gv, _)| {
            std::cmp::Reverse(last_def_pc.get(*gv as usize).copied().unwrap_or(0))
        });

        let mut spilled_use_reg: std::collections::HashMap<u32, u8> =
            std::collections::HashMap::new();
        for (i, &(gv, _)) in spilled_uses.iter().enumerate() {
            assert!(
                i < 3,
                "spilling currently supports at most 3 spilled uses per instruction"
            );
            spilled_use_reg.insert(gv, spill_reload_regs[i]);
        }

        for &(gv, _) in spilled_uses.iter() {
            let reload_reg = spilled_use_reg[&gv];
            out.push(Insn {
                op: Op::SpillPop as u8,
                a: reload_reg,
                b: 0,
                c: 0,
                imm: 0,
            });
        }

        // Map operands: spilled uses -> reload reg, others -> vreg_to_reg
        let map_reg = |gv: u32| -> u8 {
            if let Some(&r) = spilled_use_reg.get(&gv) {
                r
            } else {
                vreg_to_reg.get(gv as usize).copied().unwrap_or(gv as u8)
            }
        };

        let (a, b, c) = match vi.op {
            x if x == Op::ConstBool as u8
                || x == Op::ConstI32 as u8
                || x == Op::ConstI8Imm as u8
                || x == Op::ConstF16 as u8
                || x == Op::ConstF32 as u8
                || x == Op::ConstI64 as u8
                || x == Op::ConstF64 as u8
                || x == Op::ConstBytes as u8
                || x == Op::ConstNull as u8
                || x == Op::ConstFun as u8 =>
            {
                // Preserve `c`: e.g. ConstBool and ConstI8Imm encode their immediate in `c`.
                (map_reg(vi.a), 0, vi.c as u8)
            }
            x if x == Op::Ret as u8 || x == Op::Throw as u8 => (map_reg(vi.a), 0, 0),
            x if x == Op::Mov as u8
                || x == Op::NegI32 as u8
                || x == Op::NegI64 as u8
                || x == Op::NegF32 as u8
                || x == Op::NegF64 as u8
                || x == Op::NotBool as u8
                || x == Op::Kindof as u8
                || x == Op::BytesNew as u8
                || x == Op::BytesLen as u8
                || x == Op::ListHead as u8
                || x == Op::ListTail as u8
                || x == Op::ListIsNil as u8
                || x == Op::ArrayNew as u8
                || x == Op::ArrayLen as u8
                || x == Op::ToDyn as u8
                || x == Op::FromDynI8 as u8
                || x == Op::FromDynI16 as u8
                || x == Op::SextI16 as u8
                || x == Op::TruncI8 as u8
                || x == Op::TruncI16 as u8
                || x == Op::F16FromF32 as u8
                || x == Op::F32FromF16 as u8
                || x == Op::F64FromF32 as u8
                || x == Op::F32FromF64 as u8
                || x == Op::F32FromI32 as u8
                || x == Op::F64FromI32 as u8
                || x == Op::F32FromI64 as u8
                || x == Op::F64FromI64 as u8
                || x == Op::I32FromF32 as u8
                || x == Op::I64FromF32 as u8
                || x == Op::I32FromF64 as u8
                || x == Op::I64FromF64 as u8
                || x == Op::I32FromI64 as u8
                || x == Op::SextI64 as u8
                || x == Op::FromDynI32 as u8
                || x == Op::FromDynI64 as u8
                || x == Op::FromDynF16 as u8
                || x == Op::FromDynF32 as u8
                || x == Op::FromDynF64 as u8
                || x == Op::FromDynBool as u8
                || x == Op::FromDynPtr as u8 =>
            {
                (map_reg(vi.a), map_reg(vi.b), 0)
            }
            x if x == Op::AddI32 as u8
                || x == Op::SubI32 as u8
                || x == Op::MulI32 as u8
                || x == Op::DivI32 as u8
                || x == Op::EqI32 as u8
                || x == Op::EqI64 as u8
                || x == Op::EqF32 as u8
                || x == Op::EqF64 as u8
                || x == Op::LtI32 as u8
                || x == Op::LtI64 as u8
                || x == Op::LtF32 as u8
                || x == Op::LtF64 as u8
                || x == Op::AddF16 as u8
                || x == Op::SubF16 as u8
                || x == Op::MulF16 as u8
                || x == Op::AddF32 as u8
                || x == Op::SubF32 as u8
                || x == Op::MulF32 as u8
                || x == Op::DivF32 as u8
                || x == Op::AddI64 as u8
                || x == Op::SubI64 as u8
                || x == Op::MulI64 as u8
                || x == Op::DivI64 as u8
                || x == Op::Physeq as u8
                || x == Op::BytesConcat2 as u8
                || x == Op::ListCons as u8
                || x == Op::BytesGetU8 as u8
                || x == Op::BytesSetU8 as u8
                || x == Op::ArrayGet as u8
                || x == Op::ArraySet as u8 =>
            {
                // F64 arith dst: use escape reg when Object param (can't hold F64)
                let dst_reg = if x == Op::AddF64 as u8
                    || x == Op::SubF64 as u8
                    || x == Op::MulF64 as u8
                    || x == Op::DivF64 as u8
                {
                    f64_escape_per_param
                        .and_then(|esc| esc.get(vi.a as usize).copied())
                        .filter(|&r| r != 0)
                        .unwrap_or_else(|| map_reg(vi.a))
                } else {
                    map_reg(vi.a)
                };
                (dst_reg, map_reg(vi.b), map_reg(vi.c))
            }
            x if x == Op::BytesConcatMany as u8 => (map_reg(vi.a), map_reg(vi.b), 0),
            x if x == Op::ObjNew as u8 || x == Op::ListNil as u8 || x == Op::ConstAtom as u8 => {
                (map_reg(vi.a), 0, 0)
            }
            x if x == Op::ObjGetAtom as u8
                || x == Op::ObjHasAtom as u8
                || x == Op::ObjSetAtom as u8 =>
            {
                (map_reg(vi.a), map_reg(vi.b), 0)
            }
            x if x == Op::ObjGet as u8 || x == Op::ObjSet as u8 => {
                (map_reg(vi.a), map_reg(vi.b), map_reg(vi.c))
            }
            x if x == Op::Closure as u8 => (map_reg(vi.a), map_reg(vi.b), vi.c as u8), // c = ncaps
            x if x == Op::BindThis as u8 => (map_reg(vi.a), map_reg(vi.b), map_reg(vi.c)),
            x if x == Op::JmpIf as u8 => (map_reg(vi.a), 0, 0),
            x if x == Op::Try as u8 => (map_reg(vi.a), vi.b as u8, 0), // b = trap_only
            x if x == Op::SwitchKind as u8 => (map_reg(vi.a), vi.b as u8, 0), // b = ncases, not a reg
            x if x == Op::CallR as u8 || x == Op::TailCallR as u8 => {
                let imm = if (vi.imm as usize) < vreg_to_reg.len() {
                    vreg_to_reg[vi.imm as usize] as u32
                } else {
                    vi.imm
                };
                out.push(Insn {
                    op: vi.op,
                    a: map_reg(vi.a),
                    b: map_reg(vi.b),
                    c: vi.c as u8,
                    imm,
                });
                continue;
            }
            x if x == Op::Call as u8 => (map_reg(vi.a), map_reg(vi.b), vi.c as u8), // c = nargs
            x if x == Op::Jmp as u8 || x == Op::EndTry as u8 || x == Op::CaseKind as u8 => {
                // Data-only entries; preserve fields (e.g. CaseKind.a is the kind code).
                (vi.a as u8, vi.b as u8, vi.c as u8)
            }
            _ => (map_reg(vi.a), map_reg(vi.b), map_reg(vi.c)),
        };
        out.push(Insn {
            op: vi.op,
            a,
            b,
            c,
            imm: vi.imm,
        });

        // Emit SpillPush for each spilled def
        for &d in &info.defs {
            let gv = d.0;
            if vreg_to_spill.get(gv as usize).and_then(|x| *x).is_some() {
                let r = vreg_to_reg.get(gv as usize).copied().unwrap_or(0);
                out.push(Insn {
                    op: Op::SpillPush as u8,
                    a: r,
                    b: 0,
                    c: 0,
                    imm: 0,
                });
            }
        }
    }
    out
}

#[cfg(test)]
mod tests;
