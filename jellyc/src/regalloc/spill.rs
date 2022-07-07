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
/// - `vreg_types`: type id per vreg (used to pick correct spill-reload reg per type)
/// - `spill_reload_per_tid`: for each type that spills, [reg0, reg1] for that type
/// - `last_def_pc`: for each vreg, the PC of its last definition (used to order pops)
///
/// When multiple spilled uses appear in one instruction, we assign regs per type so F64
/// and I32 never share the same reload reg (avoids mov type mismatch). Within a type,
/// we use regs[count % 2] so at most 2 spilled uses per type per instruction.
pub fn insert_spill_ops(
    vinsns: &[VInsn],
    infos: &[InstrInfo],
    vreg_to_reg: &[u8],
    vreg_to_spill: &[Option<u32>],
    vreg_types: &[u32],
    spill_reload_per_tid: &[(u32, [u8; 2])],
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

        // Assign spill-reload regs per type so F64 and I32 never share (avoids mov type mismatch).
        // Within a type, use regs[count % 2] for up to 2 spilled uses per instruction.
        let mut tid_count: std::collections::HashMap<u32, usize> = std::collections::HashMap::new();
        let mut spilled_use_reg: std::collections::HashMap<u32, u8> = std::collections::HashMap::new();
        for &(gv, _) in spilled_uses.iter() {
            let tid = vreg_types.get(gv as usize).copied().unwrap_or(0);
            let r = spill_reload_per_tid
                .iter()
                .find(|(t, _)| *t == tid)
                .and_then(|(_, regs)| {
                    let c = tid_count.entry(tid).or_insert(0);
                    let idx = *c % 2;
                    *c += 1;
                    Some(if idx == 0 { regs[0] } else { regs[1] })
                })
                .unwrap_or_else(|| vreg_to_reg.get(gv as usize).copied().unwrap_or(0));
            spilled_use_reg.insert(gv, r);
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
                (map_reg(vi.a), 0, 0)
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
            x if x == Op::ObjNew as u8
                || x == Op::ListNil as u8
                || x == Op::ConstAtom as u8 =>
            {
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
            x if x == Op::Closure as u8 || x == Op::BindThis as u8 => {
                (map_reg(vi.a), map_reg(vi.b), map_reg(vi.c))
            }
            x if x == Op::JmpIf as u8 => (map_reg(vi.a), 0, 0),
            x if x == Op::Try as u8 => (map_reg(vi.a), vi.b as u8, 0), // b = trap_only
            x if x == Op::SwitchKind as u8 => (map_reg(vi.a), vi.b as u8, 0), // b = ncases, not a reg
            x if x == Op::CallR as u8 => {
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
            x if x == Op::Jmp as u8
                || x == Op::EndTry as u8
                || x == Op::CaseKind as u8 =>
            {
                (0, 0, 0)
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
mod tests {
    use super::*;
    use crate::regalloc::VReg;

    #[test]
    fn insert_spill_push_pop_around_def_and_use() {
        // v0 = ConstI32 123; v1 = ToDyn v0; SpillPush v1; SpillPop v1; Ret v1
        // Simulate: v0 in r0, v1 spilled (slot 0), reload into r1
        let vinsns = vec![
            VInsn { op: Op::ConstI32 as u8, a: 0, b: 0, c: 0, imm: 123 },
            VInsn { op: Op::ToDyn as u8, a: 1, b: 0, c: 0, imm: 0 },
            VInsn { op: Op::Ret as u8, a: 1, b: 0, c: 0, imm: 0 },
        ];
        let infos = vec![
            InstrInfo { uses: vec![], defs: vec![VReg(0)] },
            InstrInfo { uses: vec![VReg(0)], defs: vec![VReg(1)] },
            InstrInfo { uses: vec![VReg(1)], defs: vec![] },
        ];
        let vreg_to_reg = vec![0, 1]; // v0->r0, v1->r1 (spill reload)
        let vreg_to_spill = vec![None, Some(0)]; // v1 spilled
        let vreg_types = vec![4, 10]; // v0=I32, v1=Dynamic (T_DYNAMIC=10)
        let spill_reload_per_tid = vec![(10u32, [1u8, 1u8])]; // Dynamic spills to r1
        let last_def_pc = vec![0, 1]; // v0 def at 0, v1 def at 1

        let out = insert_spill_ops(
            &vinsns,
            &infos,
            &vreg_to_reg,
            &vreg_to_spill,
            &vreg_types,
            &spill_reload_per_tid,
            &last_def_pc,
            None,
        );

        // Expect: ConstI32; ToDyn; SpillPush r1; SpillPop r1; Ret r1
        assert!(out.len() >= 5);
        assert_eq!(out[0].op, Op::ConstI32 as u8);
        assert_eq!(out[1].op, Op::ToDyn as u8);
        assert_eq!(out[2].op, Op::SpillPush as u8);
        assert_eq!(out[2].a, 1);
        assert_eq!(out[3].op, Op::SpillPop as u8);
        assert_eq!(out[3].a, 1);
        assert_eq!(out[4].op, Op::Ret as u8);
        assert_eq!(out[4].a, 1);
    }
}
