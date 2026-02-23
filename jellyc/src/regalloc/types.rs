#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VReg(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PReg(pub u16);

#[derive(Clone, Debug)]
pub struct InstrInfo {
    pub uses: Vec<VReg>,
    pub defs: Vec<VReg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpillPolicy {
    Forbid,
    Allow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AllocError {
    TooManyPhysicalRegs,
    VRegOutOfRange { vreg: VReg, vregs: u32 },
    MultipleDefs { vreg: VReg },
    UsedButNeverDefined { vreg: VReg },
    OutOfPhysicalRegsNoSpill,
    IntervalInternalBug,
}

#[derive(Clone, Debug)]
pub struct Allocation {
    pub vreg_to_preg: Vec<Option<PReg>>,
    pub vreg_to_spill: Vec<Option<u32>>,
    pub used_pregs: u16,
    pub used_spill_slots: u32,
}
