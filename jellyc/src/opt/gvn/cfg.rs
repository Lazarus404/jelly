use crate::ir::{IrFunction, IrTerminator};

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

pub(super) fn block_order_rpo(func: &IrFunction) -> Vec<usize> {
    let n = func.blocks.len();
    if n == 0 {
        return vec![];
    }
    let entry = func.entry.0 as usize;
    let mut seen = vec![false; n];
    let mut post: Vec<usize> = Vec::with_capacity(n);
    fn dfs(f: &IrFunction, b: usize, seen: &mut [bool], post: &mut Vec<usize>) {
        if b >= seen.len() || seen[b] {
            return;
        }
        seen[b] = true;
        for s in term_succs(&f.blocks[b].term) {
            dfs(f, s, seen, post);
        }
        post.push(b);
    }
    dfs(func, entry, &mut seen, &mut post);
    post.reverse();
    post
}
