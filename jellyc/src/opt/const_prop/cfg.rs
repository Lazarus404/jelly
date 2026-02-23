use crate::ir::{IrInsn, IrOp, IrTerminator};

pub(super) fn block_successors(term: &IrTerminator) -> Vec<usize> {
    match term {
        IrTerminator::Jmp { target } => vec![target.0 as usize],
        IrTerminator::JmpIf {
            then_tgt, else_tgt, ..
        } => vec![then_tgt.0 as usize, else_tgt.0 as usize],
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => vec![],
    }
}

pub(super) fn count_leading_phis(insns: &[IrInsn]) -> usize {
    insns
        .iter()
        .take_while(|ins| matches!(ins.op, IrOp::Phi { .. }))
        .count()
}
