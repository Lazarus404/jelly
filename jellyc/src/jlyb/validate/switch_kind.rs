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

use crate::jlyb::{Insn, Op};

use super::helpers::err;

pub(super) fn validate_switch_kind_tables(func_i: usize, insns: &[Insn]) -> Result<(), String> {
    let ninsns = insns.len() as i32;
    let mut pc: usize = 0;
    while pc < insns.len() {
        let ins = &insns[pc];
        if ins.op == Op::SwitchKind as u8 {
            let ncases = ins.b as usize;
            let table_first = pc + 1;
            let table_end = table_first + ncases;
            if table_end > insns.len() {
                return Err(err(func_i, pc, "switch_kind case table out of range"));
            }
            // Default delta is relative to end-of-table.
            let dd = ins.imm as i32;
            let dtgt = (table_end as i32) + dd;
            if dtgt < 0 || dtgt > ninsns {
                return Err(err(func_i, pc, "switch_kind default target out of range"));
            }
            for i in 0..ncases {
                let ci = &insns[table_first + i];
                if ci.op != Op::CaseKind as u8 {
                    return Err(err(func_i, pc, "switch_kind expects case_kind entries"));
                }
                let cd = ci.imm as i32;
                let ctgt = (table_end as i32) + cd;
                if ctgt < 0 || ctgt > ninsns {
                    return Err(err(
                        func_i,
                        table_first + i,
                        "case_kind target out of range",
                    ));
                }
            }
            pc = table_end;
            continue;
        }
        if ins.op == Op::CaseKind as u8 {
            return Err(err(func_i, pc, "case_kind without preceding switch_kind"));
        }
        pc += 1;
    }
    Ok(())
}
