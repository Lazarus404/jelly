use crate::ir::IrFunction;
use crate::regalloc::InstrInfo;

pub(super) fn compute_last_def_pc(f: &IrFunction, infos: &[InstrInfo]) -> Vec<u32> {
    // Compute last-def PC per vreg (for spill pop order)
    let mut last_def_pc: Vec<u32> = vec![0; f.vreg_types.len()];
    for (pc, info) in infos.iter().enumerate() {
        for &d in &info.defs {
            if (d.0 as usize) < last_def_pc.len() {
                last_def_pc[d.0 as usize] = pc as u32;
            }
        }
    }
    last_def_pc
}
