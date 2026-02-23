use crate::ir::IrFunction;

use super::super::term_size;

pub(super) fn compute_block_start(f: &IrFunction, order: &[crate::ir::BlockId]) -> Vec<u32> {
    let mut block_start: Vec<u32> = vec![0; f.blocks.len()];
    let mut pc: u32 = 0;
    for b in order.iter().copied() {
        let bi = b.0 as usize;
        let blk = &f.blocks[bi];
        block_start[bi] = pc;
        pc += blk.insns.len() as u32;
        pc += term_size(&blk.term);
    }
    block_start
}
