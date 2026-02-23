/*
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

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
