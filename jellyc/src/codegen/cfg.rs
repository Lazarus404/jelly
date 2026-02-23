use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrFunction, IrTerminator};

pub(super) fn term_size(term: &IrTerminator) -> u32 {
    match term {
        IrTerminator::JmpIf { .. } => 2,
        IrTerminator::SwitchKind { cases, .. } => 1 + (cases.len() as u32),
        IrTerminator::Jmp { .. } | IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } => 1,
        IrTerminator::Unreachable => 1,
    }
}

pub(super) fn delta(from_pc: u32, to_pc: u32) -> u32 {
    // VM uses signed deltas, encoded as u32.
    let d: i32 = (to_pc as i32) - ((from_pc + 1) as i32);
    d as u32
}

pub(super) fn blk_pc(block_start: &[u32], b: BlockId) -> Result<u32, CompileError> {
    block_start
        .get(b.0 as usize)
        .copied()
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad block id"))
}

pub(super) fn block_successors(term: &IrTerminator) -> Vec<BlockId> {
    match term {
        IrTerminator::Jmp { target } => vec![*target],
        IrTerminator::JmpIf {
            then_tgt, else_tgt, ..
        } => {
            // Note: DFS→postorder→reverse means successor visitation order is reversed in
            // the final RPO. Visit else first so "then" (eg loop bodies) tend to appear
            // earlier in the linear stream than exits.
            vec![*else_tgt, *then_tgt]
        }
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut out: Vec<BlockId> = Vec::with_capacity(cases.len() + 1);
            for &(_, b) in cases {
                out.push(b);
            }
            out.push(*default);
            out
        }
        IrTerminator::Ret { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => vec![],
    }
}

pub(super) fn block_order_rpo(f: &IrFunction) -> Vec<BlockId> {
    let n = f.blocks.len();
    if n == 0 {
        return vec![];
    }
    let mut seen: Vec<bool> = vec![false; n];
    let mut post: Vec<BlockId> = Vec::with_capacity(n);

    fn dfs(f: &IrFunction, b: BlockId, seen: &mut [bool], post: &mut Vec<BlockId>) {
        let bi = b.0 as usize;
        if bi >= seen.len() || seen[bi] {
            return;
        }
        seen[bi] = true;
        let term = &f.blocks[bi].term;
        for s in block_successors(term) {
            dfs(f, s, seen, post);
        }
        post.push(b);
    }

    // Entry is always block 0.
    dfs(f, BlockId(0), &mut seen, &mut post);
    post.reverse(); // reverse postorder

    // Append unreachable blocks deterministically.
    for (bi, _) in f.blocks.iter().enumerate() {
        if !seen[bi] {
            post.push(BlockId(bi as u32));
        }
    }
    post
}
