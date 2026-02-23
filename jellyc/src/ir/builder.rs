use crate::ast::Span;

use super::ids::{BlockId, TypeId, VRegId};
use super::model::{IrBlock, IrFunction, IrInsn};
use super::op::IrOp;
use super::terminator::IrTerminator;

#[derive(Debug)]
pub struct IrBuilder {
    pub func: IrFunction,
    cur: BlockId,
}

impl IrBuilder {
    pub fn new(name: Option<String>) -> Self {
        let entry = BlockId(0);
        Self {
            func: IrFunction {
                name,
                param_count: 0,
                cap_vregs: Vec::new(),
                entry,
                blocks: vec![IrBlock {
                    label: Some("entry".to_string()),
                    insns: Vec::new(),
                    term: IrTerminator::Unreachable,
                }],
                vreg_types: Vec::new(),
            },
            cur: entry,
        }
    }

    pub fn new_block(&mut self, label: Option<String>) -> BlockId {
        let id = BlockId(self.func.blocks.len() as u32);
        self.func.blocks.push(IrBlock {
            label,
            insns: Vec::new(),
            term: IrTerminator::Unreachable,
        });
        id
    }

    pub fn set_block(&mut self, b: BlockId) {
        self.cur = b;
    }

    pub fn cur_block(&self) -> BlockId {
        self.cur
    }

    pub fn is_open(&self) -> bool {
        matches!(
            self.func.blocks[self.cur.0 as usize].term,
            IrTerminator::Unreachable
        )
    }

    pub fn new_vreg(&mut self, tid: TypeId) -> VRegId {
        let id = VRegId(self.func.vreg_types.len() as u32);
        self.func.vreg_types.push(tid);
        id
    }

    pub fn emit(&mut self, span: Span, op: IrOp) {
        let b = &mut self.func.blocks[self.cur.0 as usize];
        b.insns.push(IrInsn { span, op });
    }

    pub fn term(&mut self, term: IrTerminator) {
        let b = &mut self.func.blocks[self.cur.0 as usize];
        b.term = term;
    }
}
