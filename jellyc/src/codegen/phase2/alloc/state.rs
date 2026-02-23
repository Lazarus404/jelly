pub(super) struct RegState {
    pub(super) vreg_to_reg: Vec<u8>,
    pub(super) vreg_to_spill: Vec<Option<u32>>,
    pub(super) reg_types: Vec<u32>,
    pub(super) base: u16,
    pub(super) cap_start: u32,
    pub(super) dyn_spill_reload_regs: Option<[u8; 3]>,
    pub(super) f64_escape_per_param: Vec<u8>,
}

impl RegState {
    pub(super) fn new(nvregs: usize) -> Self {
        Self {
            vreg_to_reg: vec![0; nvregs],
            vreg_to_spill: vec![None; nvregs],
            reg_types: Vec::new(),
            base: 0,
            cap_start: 0,
            dyn_spill_reload_regs: None,
            f64_escape_per_param: Vec::new(),
        }
    }
}
