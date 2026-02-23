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
