use crate::ir::IrTerminator;

pub(super) fn block_successors(term: &IrTerminator) -> Vec<usize> {
    use crate::ir::IrTerminator::*;
    match term {
        Jmp { target } => vec![target.0 as usize],
        JmpIf {
            then_tgt, else_tgt, ..
        } => vec![then_tgt.0 as usize, else_tgt.0 as usize],
        SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        Ret { .. } | TailCall { .. } | Unreachable => vec![],
    }
}
