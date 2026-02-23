use crate::error::{CompileError, ErrorKind};
use crate::jlyb::{Insn, Op};
use crate::regalloc::spill;

use super::{Allocation, VirtualStream};

pub(super) fn emit_final_insns(vs: &VirtualStream, alloc: &Allocation) -> Result<Vec<Insn>, CompileError> {
    // Defensive check: CALLR encodes arg_base as a vreg index in `imm` during the virtual stream.
    for vi in vs.vinsns.iter().filter(|vi| vi.op == Op::CallR as u8) {
        if (vi.imm as usize) >= alloc.vreg_to_reg.len() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "CALLR arg_base vreg out of range",
            ));
        }
    }

    let insns: Vec<Insn> = spill::insert_spill_ops(
        &vs.vinsns,
        &vs.infos,
        &alloc.vreg_to_reg,
        &alloc.vreg_to_spill,
        alloc.spill_reload_regs,
        &alloc.last_def_pc,
        Some(&alloc.f64_escape_per_param),
    );

    Ok(crate::peephole::peephole(&insns))
}

