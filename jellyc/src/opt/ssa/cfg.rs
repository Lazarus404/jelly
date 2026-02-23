use crate::ir::{IrFunction, IrOp, IrTerminator};

pub(super) fn term_succs(term: &IrTerminator) -> Vec<usize> {
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

/// Terminator edges + conservative `Try` → `catch_block` edges.
pub(super) fn build_cfg(func: &IrFunction) -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
    let nb = func.blocks.len();
    let mut succs: Vec<Vec<usize>> = vec![Vec::new(); nb];
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nb];

    for (bi, blk) in func.blocks.iter().enumerate() {
        for s in term_succs(&blk.term) {
            if s < nb {
                succs[bi].push(s);
                preds[s].push(bi);
            }
        }
        for ins in &blk.insns {
            if let IrOp::Try { catch_block, .. } = ins.op {
                let s = catch_block.0 as usize;
                if s < nb {
                    succs[bi].push(s);
                    preds[s].push(bi);
                }
            }
        }
    }

    for s in &mut succs {
        s.sort_unstable();
        s.dedup();
    }
    for p in &mut preds {
        p.sort_unstable();
        p.dedup();
    }

    (succs, preds)
}
