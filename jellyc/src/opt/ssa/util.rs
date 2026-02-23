use crate::ir::{IrInsn, IrOp, TypeId, VRegId};

pub(super) fn count_leading_phis(insns: &[IrInsn]) -> usize {
    insns
        .iter()
        .take_while(|ins| matches!(ins.op, IrOp::Phi { .. }))
        .count()
}

pub(super) fn new_vreg(vreg_types: &mut Vec<TypeId>, tid: TypeId) -> VRegId {
    let v = VRegId(vreg_types.len() as u32);
    vreg_types.push(tid);
    v
}
