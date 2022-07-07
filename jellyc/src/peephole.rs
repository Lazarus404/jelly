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

/// Post-allocation peephole cleanup.
/// Removes redundant moves (mov rX, rX), dead jumps, same-target branches,
/// and updates jump targets.

use crate::jlyb::{Insn, Op};

const OP_MOV: u8 = Op::Mov as u8;
const OP_JMP: u8 = Op::Jmp as u8;
const OP_JMPIF: u8 = Op::JmpIf as u8;
const OP_TRY: u8 = Op::Try as u8;
const OP_SWITCH_KIND: u8 = Op::SwitchKind as u8;
const OP_CASE_KIND: u8 = Op::CaseKind as u8;
const OP_CONST_I32: u8 = Op::ConstI32 as u8;
const OP_ADD_I32: u8 = Op::AddI32 as u8;
const OP_SUB_I32: u8 = Op::SubI32 as u8;
const OP_MUL_I32: u8 = Op::MulI32 as u8;
const OP_EQ_I32: u8 = Op::EqI32 as u8;
const OP_LT_I32: u8 = Op::LtI32 as u8;
const OP_ADD_I32_IMM: u8 = Op::AddI32Imm as u8;
const OP_SUB_I32_IMM: u8 = Op::SubI32Imm as u8;
const OP_MUL_I32_IMM: u8 = Op::MulI32Imm as u8;
const OP_EQ_I32_IMM: u8 = Op::EqI32Imm as u8;
const OP_LT_I32_IMM: u8 = Op::LtI32Imm as u8;

const OP_RET: u8 = Op::Ret as u8;
const OP_THROW: u8 = Op::Throw as u8;
const OP_ASSERT: u8 = Op::Assert as u8;
const OP_ENDTRY: u8 = Op::EndTry as u8;
const OP_CONST_FUN: u8 = Op::ConstFun as u8;
const OP_CALL: u8 = Op::Call as u8;
const OP_CALLR: u8 = Op::CallR as u8;

const OP_BYTES_SET_U8: u8 = Op::BytesSetU8 as u8;
const OP_ARRAY_SET: u8 = Op::ArraySet as u8;
const OP_OBJ_SET_ATOM: u8 = Op::ObjSetAtom as u8;
const OP_OBJ_SET: u8 = Op::ObjSet as u8;
const OP_SPILL_PUSH: u8 = Op::SpillPush as u8;

/// Remove redundant moves, dead jumps, same-target JmpIf, fix up jump deltas,
/// and fold ConstI32 + Arith into ArithImm when the constant fits in i8.
pub fn peephole(insns: &[Insn]) -> Vec<Insn> {
    let mut keep: Vec<bool> = insns.iter().map(|i| !is_redundant_mov(i)).collect();
    let mut replacement: Vec<Option<Insn>> = vec![None; insns.len()];

    fn writes_a(op: u8) -> bool {
        // Most ops write `a` as the destination. Keep this conservative:
        // if we're not sure, treat it as writing (it only reduces folding).
        match op {
            OP_RET | OP_JMP | OP_JMPIF | OP_THROW | OP_ASSERT | OP_ENDTRY | OP_SWITCH_KIND | OP_CASE_KIND | OP_TRY => false,
            OP_BYTES_SET_U8 | OP_ARRAY_SET | OP_OBJ_SET_ATOM | OP_OBJ_SET | OP_SPILL_PUSH => false,
            _ => true,
        }
    }

    fn reads_a(op: u8) -> bool {
        // Ops where `a` is an input operand (not just a destination).
        // Keep conservative: if unsure, return true (fewer folds, never wrong).
        match op {
            OP_RET | OP_THROW | OP_ASSERT | OP_JMPIF => true,
            OP_BYTES_SET_U8 | OP_ARRAY_SET | OP_OBJ_SET_ATOM | OP_OBJ_SET | OP_SPILL_PUSH => true,
            _ => false,
        }
    }

    // Fold: CONST_FUN rF, imm=fi  ...  CALLR dst, rF, nargs, imm=arg_base  ->  CALL dst, b=arg_base, nargs, imm=fi
    //
    // This is post-allocation, so we're operating on physical regs. It mainly helps typed code
    // that materializes constant function values but then calls them indirectly (eg recursion).
    {
        let mut const_fun_for_reg: Vec<Option<u32>> = vec![None; 256];
        for (pc, ins) in insns.iter().enumerate() {
            if writes_a(ins.op) {
                const_fun_for_reg[ins.a as usize] = None;
            }
            if ins.op == OP_CONST_FUN {
                const_fun_for_reg[ins.a as usize] = Some(ins.imm);
                continue;
            }
            if ins.op == OP_CALLR {
                let callee_r = ins.b as usize;
                if let Some(fi) = const_fun_for_reg.get(callee_r).and_then(|x| *x) {
                    let arg_base = ins.imm as u8;
                    replacement[pc] = Some(Insn {
                        op: OP_CALL,
                        a: ins.a,
                        b: arg_base,
                        c: ins.c,
                        imm: fi,
                    });
                }
            }
        }
    }

    // Fold ConstI32 + Arith -> ArithImm when imm fits in -128..127 and const reg is only used by the arith.
    let n = insns.len();
    for pc in 0..n.saturating_sub(1) {
        let c = &insns[pc];
        let a = &insns[pc + 1];
        if c.op != OP_CONST_I32 || c.b != 0 || c.c != 0 {
            continue;
        }
        let imm_i32 = c.imm as i32;
        if imm_i32 < -128 || imm_i32 > 127 {
            continue;
        }
        let const_reg = c.a;
        // Arith uses (a=dst, b=src1, c=src2). Imm form: (a=dst, b=src, c=imm8).
        // LtI32Imm computes b < imm. So we can only fold when const is the right operand (a.c).
        // Folding when const is left (a.b) would turn "0 < x" into "x < 0" which is wrong.
        let (imm_op, src_reg, imm_byte) = match a.op {
            OP_ADD_I32 => {
                let (sr, ib) = if a.b == const_reg { (a.c, imm_i32 as u8) } else if a.c == const_reg { (a.b, imm_i32 as u8) } else { continue };
                (OP_ADD_I32_IMM, sr, ib)
            }
            OP_SUB_I32 => {
                let (sr, ib) = if a.b == const_reg { (a.c, imm_i32 as u8) } else if a.c == const_reg { (a.b, imm_i32 as u8) } else { continue };
                (OP_SUB_I32_IMM, sr, ib)
            }
            OP_MUL_I32 => {
                let (sr, ib) = if a.b == const_reg { (a.c, imm_i32 as u8) } else if a.c == const_reg { (a.b, imm_i32 as u8) } else { continue };
                (OP_MUL_I32_IMM, sr, ib)
            }
            OP_EQ_I32 => {
                let (sr, ib) = if a.b == const_reg { (a.c, imm_i32 as u8) } else if a.c == const_reg { (a.b, imm_i32 as u8) } else { continue };
                (OP_EQ_I32_IMM, sr, ib)
            }
            OP_LT_I32 => {
                if a.c == const_reg {
                    (OP_LT_I32_IMM, a.b, imm_i32 as u8)
                } else {
                    continue; // const in left operand: "0 < x" cannot fold to LtI32Imm (which is "x < 0")
                }
            }
            _ => continue,
        };
        // Ensure const_reg is not used elsewhere after the arith.
        let mut used_elsewhere = false;
        for i in (pc + 2)..n {
            let ins = &insns[i];
            // Only treat *reads* as a use. A later write to `a == const_reg` is fine (it overwrites).
            if ins.b == const_reg || ins.c == const_reg || (ins.a == const_reg && reads_a(ins.op)) {
                used_elsewhere = true;
                break;
            }
            // If the const reg is overwritten, it cannot be used later as that constant.
            if ins.a == const_reg && writes_a(ins.op) {
                break;
            }
        }
        if used_elsewhere {
            continue;
        }
        keep[pc] = false;
        replacement[pc + 1] = Some(Insn {
            op: imm_op,
            a: a.a,
            b: src_reg,
            c: imm_byte,
            imm: 0,
        });
    }


    // Mark dead jumps (Jmp to next instruction).
    let n = insns.len();
    for (pc, ins) in insns.iter().enumerate() {
        if !keep[pc] {
            continue;
        }
        if ins.op == OP_JMP {
            let target = (pc as i32 + 1) + (ins.imm as i32);
            if target >= 0 && (target as usize) < n && target as usize == pc + 1 {
                keep[pc] = false; // jump to next instruction is dead
            }
        }
    }
    let n = keep.len();

    // old_pc -> new_pc
    let mut old_to_new: Vec<usize> = vec![0; n + 1];
    let mut new_pc = 0;
    for (old_pc, &k) in keep.iter().enumerate() {
        old_to_new[old_pc] = new_pc;
        if k {
            new_pc += 1;
        }
    }
    old_to_new[n] = new_pc;

    let mut out: Vec<Insn> = Vec::with_capacity(new_pc);
    let mut last_switch: Option<(usize, usize, u32)> = None; // (old_pc, new_from, ncases)
    for (old_pc, ins) in insns.iter().enumerate() {
        if !keep[old_pc] {
            continue;
        }
        let mut i = *ins;
        let new_from = old_to_new[old_pc];

        // Combine: JmpIf with same then/else target -> Jmp (unconditional)
        // JmpIf at pc is followed by Jmp at pc+1; else_target = where that Jmp goes
        if i.op == OP_JMPIF && old_pc + 1 < n && keep[old_pc + 1] && insns[old_pc + 1].op == OP_JMP {
            let then_target = (old_pc as i32 + 1) + (i.imm as i32);
            let else_target = (old_pc as i32 + 2) + (insns[old_pc + 1].imm as i32);
            if then_target >= 0 && (then_target as usize) <= n && then_target == else_target {
                i.op = OP_JMP;
                i.a = 0;
                // imm stays as delta to target
            }
        }

        match i.op {
            x if x == OP_JMP => {
                let mut old_target = (old_pc as i32 + 1) + (i.imm as i32);
                // Branch threading: follow Jmp -> Jmp chains
                while old_target >= 0 && (old_target as usize) < n && keep[old_target as usize]
                    && insns[old_target as usize].op == OP_JMP
                {
                    let next = (old_target + 1) as i32 + (insns[old_target as usize].imm as i32);
                    old_target = next;
                }
                if old_target >= 0 && (old_target as usize) <= n {
                    let new_target = old_to_new[old_target as usize];
                    i.imm = ((new_target as i32) - ((new_from + 1) as i32)) as u32;
                }
            }
            x if x == OP_JMPIF => {
                let mut old_target = (old_pc as i32 + 1) + (i.imm as i32);
                while old_target >= 0 && (old_target as usize) < n && keep[old_target as usize]
                    && insns[old_target as usize].op == OP_JMP
                {
                    let next = (old_target + 1) as i32 + (insns[old_target as usize].imm as i32);
                    old_target = next;
                }
                if old_target >= 0 && (old_target as usize) <= n {
                    let new_target = old_to_new[old_target as usize];
                    i.imm = ((new_target as i32) - ((new_from + 1) as i32)) as u32;
                }
            }
            x if x == OP_TRY => {
                let mut old_target = (old_pc as i32 + 1) + (i.imm as i32);
                while old_target >= 0 && (old_target as usize) < n && keep[old_target as usize]
                    && insns[old_target as usize].op == OP_JMP
                {
                    let next = (old_target + 1) as i32 + (insns[old_target as usize].imm as i32);
                    old_target = next;
                }
                if old_target >= 0 && (old_target as usize) <= n {
                    let new_target = old_to_new[old_target as usize];
                    i.imm = ((new_target as i32) - ((new_from + 1) as i32)) as u32;
                }
            }
            x if x == OP_SWITCH_KIND => {
                let ncases = i.b as u32;
                let old_table_end = (old_pc as i32 + 1) + (ncases as i32);
                let new_table_end = (new_from as i32 + 1) + (ncases as i32);
                let old_def_target = old_table_end + (i.imm as i32);
                if old_def_target >= 0 && (old_def_target as usize) <= n {
                    let new_def_target = old_to_new[old_def_target as usize];
                    i.imm = ((new_def_target as i32) - new_table_end) as u32;
                }
                last_switch = Some((old_pc, new_from, ncases));
            }
            x if x == OP_CASE_KIND => {
                if let Some((old_sw_pc, new_sw_from, ncases)) = last_switch {
                    let old_table_end = (old_sw_pc as i32 + 1) + (ncases as i32);
                    let new_table_end = (new_sw_from as i32 + 1) + (ncases as i32);
                    let old_case_target = old_table_end + (i.imm as i32);
                    if old_case_target >= 0 && (old_case_target as usize) <= n {
                        let new_case_target = old_to_new[old_case_target as usize];
                        i.imm = ((new_case_target as i32) - new_table_end) as u32;
                    }
                }
            }
            _ => {}
        }
        let to_push = replacement[old_pc].unwrap_or(i);
        out.push(to_push);
    }
    out
}

fn is_redundant_mov(i: &Insn) -> bool {
    i.op == OP_MOV && i.a == i.b && i.c == 0 && i.imm == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn folds_const_i32_lt_to_imm_until_overwrite() {
        // Pattern like fib:
        //   r2 = 2
        //   r1 = (r0 < r2)
        //   ...
        //   r2 = <something else>   // overwrite -> later uses of r2 should not block folding
        let insns = vec![
            Insn { op: OP_CONST_I32, a: 2, b: 0, c: 0, imm: 2 },
            Insn { op: OP_LT_I32, a: 1, b: 0, c: 2, imm: 0 },
            Insn { op: OP_JMPIF, a: 1, b: 0, c: 0, imm: 1 },
            Insn { op: OP_JMP, a: 0, b: 0, c: 0, imm: 1 },
            Insn { op: OP_RET, a: 0, b: 0, c: 0, imm: 0 },
            // overwrite r2
            Insn { op: OP_CONST_I32, a: 2, b: 0, c: 0, imm: 123 },
            // later "use" of r2 after overwrite should not block folding
            Insn { op: OP_ADD_I32, a: 4, b: 2, c: 0, imm: 0 },
        ];
        let out = peephole(&insns);
        assert!(out.len() < insns.len());
        assert_eq!(out[0].op, OP_LT_I32_IMM);
        assert_eq!(out[0].a, 1);
        assert_eq!(out[0].b, 0);
        assert_eq!(out[0].c, 2);
    }
}
