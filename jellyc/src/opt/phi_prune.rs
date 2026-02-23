/**
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

// Phi incoming pruning.
//
// Some CFG simplifications rewrite terminators (e.g. const-prop folding JmpIf -> Jmp),
// removing edges. SSA phi nodes still carry incoming entries for the old predecessor
// blocks. Phi elimination expects phi incomings to reference actual CFG predecessors.

use crate::ir::{BlockId, IrModule, IrOp, IrTerminator};

fn count_leading_phis(block: &[crate::ir::IrInsn]) -> usize {
    block
        .iter()
        .take_while(|ins| matches!(ins.op, IrOp::Phi { .. }))
        .count()
}

fn block_successors(term: &IrTerminator) -> Vec<usize> {
    match term {
        IrTerminator::Jmp { target } => vec![target.0 as usize],
        IrTerminator::JmpIf { then_tgt, else_tgt, .. } => vec![then_tgt.0 as usize, else_tgt.0 as usize],
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => vec![],
    }
}

pub fn prune_phi_incomings(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let nb = func.blocks.len();
        if nb == 0 {
            continue;
        }

        // Compute predecessor lists from normal CFG edges only.
        let mut preds: Vec<Vec<BlockId>> = vec![Vec::new(); nb];
        for (bi, blk) in func.blocks.iter().enumerate() {
            let from = BlockId(bi as u32);
            for s in block_successors(&blk.term) {
                if s < nb {
                    preds[s].push(from);
                }
            }
        }
        for p in &mut preds {
            p.sort_by_key(|b| b.0);
            p.dedup();
        }

        for (bi, blk) in func.blocks.iter_mut().enumerate() {
            let lead = count_leading_phis(&blk.insns);
            if lead == 0 {
                continue;
            }
            let pred_list = &preds[bi];
            for i in 0..lead {
                let IrOp::Phi { incomings, .. } = &mut blk.insns[i].op else {
                    continue;
                };
                let before = incomings.len();
                incomings.retain(|(p, _)| pred_list.binary_search_by_key(&p.0, |b| b.0).is_ok());
                if incomings.len() != before {
                    changed = true;
                }
            }
        }
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::ir::{IrBlock, IrFunction, IrInsn, IrTerminator, VRegId};

    #[test]
    fn phi_prune_removes_non_predecessor_incomings() {
        // b0 -> b2 (not a predecessor of b1)
        // b2 -> b1
        // b1 has phi with incoming from b0 and b2; pruning should remove b0 incoming.
        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![IrFunction {
                name: Some("f".to_string()),
                param_count: 0,
                cap_vregs: vec![],
                entry: BlockId(0),
                blocks: vec![
                    IrBlock {
                        label: Some("b0".to_string()),
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(0), imm: 1 },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(2) },
                    },
                    IrBlock {
                        label: Some("b1".to_string()),
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::Phi {
                                dst: VRegId(2),
                                incomings: vec![(BlockId(0), VRegId(0)), (BlockId(2), VRegId(1))],
                            },
                        }],
                        term: IrTerminator::Ret { value: VRegId(2) },
                    },
                    IrBlock {
                        label: Some("b2".to_string()),
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(1), imm: 2 },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(1) },
                    },
                ],
                vreg_types: vec![
                    crate::typectx::T_I32, // v0
                    crate::typectx::T_I32, // v1
                    crate::typectx::T_I32, // v2 phi
                ],
            }],
            entry: 0,
        };

        let changed = prune_phi_incomings(&mut m);
        assert!(changed);
        let b1 = &m.funcs[0].blocks[1];
        let IrOp::Phi { incomings, .. } = &b1.insns[0].op else {
            panic!("expected phi in b1");
        };
        assert_eq!(incomings.len(), 1);
        assert_eq!(incomings[0].0, BlockId(2));
        assert_eq!(incomings[0].1, VRegId(1));
    }
}

