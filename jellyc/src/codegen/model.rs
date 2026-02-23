use crate::ir::VRegId;
use crate::regalloc::spill::VInsn;
use crate::regalloc::InstrInfo;

pub(super) fn reg(v: VRegId) -> u32 {
    v.0
}

pub(super) struct VirtualStream {
    // Virtual instruction stream; operands are global vreg indices (u32).
    pub(super) vinsns: Vec<VInsn>,
    // Per-instruction uses/defs for regalloc/spill insertion; global vreg indices.
    pub(super) infos: Vec<InstrInfo>,
    // (sig_id, arg_base_vreg, nargs): used to pin arg windows to ABI arg blocks.
    pub(super) call_windows: Vec<(u32, u32, u8)>,
}

pub(super) struct Allocation {
    pub(super) vreg_to_reg: Vec<u8>,
    pub(super) vreg_to_spill: Vec<Option<u32>>,
    pub(super) reg_types: Vec<u32>,
    pub(super) cap_start: u32,
    pub(super) spill_reload_regs: [u8; 3],
    pub(super) last_def_pc: Vec<u32>,
    pub(super) f64_escape_per_param: Vec<u8>,
}
