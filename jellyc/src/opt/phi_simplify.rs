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

// Phi simplification: turn trivial Phi nodes into Mov.
//
// Phi nodes are used to represent the value of a variable at a join point in the control flow.
// For example, if a variable is defined in multiple places, we can use a Phi node to represent
// the value of the variable at the join point.
//
// This pass simplifies Phi nodes by replacing them with Mov nodes when the value is the same as
// the incoming value.
//
// This pass is separate from copy propagation because copy propagation canonicalizes the incoming
// values through known aliases, and then this pass collapses identity/single-input phis.

use crate::ir::{IrModule, IrOp};

/// Simplify trivial `Phi` nodes:
/// - If a phi has 0 incomings, leave it alone (should be unreachable / malformed, handled elsewhere).
/// - If a phi has 1 incoming, replace it with `Mov`.
/// - If all incoming values are identical, replace it with `Mov`.
pub fn simplify_phis(m: &mut IrModule) -> bool {
    let mut changed = false;
    for func in &mut m.funcs {
        for block in &mut func.blocks {
            for ins in &mut block.insns {
                let IrOp::Phi { dst, incomings } = &mut ins.op else {
                    continue;
                };
                if incomings.is_empty() {
                    continue;
                }
                let first = incomings[0].1;
                if incomings.len() == 1 || incomings.iter().all(|(_, v)| *v == first) {
                    ins.op = IrOp::Mov { dst: *dst, src: first };
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
    use crate::ir::{BlockId, IrBlock, IrFunction, IrInsn, IrModule, IrTerminator, VRegId};

    #[test]
    fn phi_simplify_replaces_identity_phi_with_mov() {
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
                blocks: vec![IrBlock {
                    label: None,
                    insns: vec![IrInsn {
                        span: Span::point(0),
                        op: IrOp::Phi {
                            dst: VRegId(2),
                            incomings: vec![(BlockId(0), VRegId(1)), (BlockId(1), VRegId(1))],
                        },
                    }],
                    term: IrTerminator::Ret { value: VRegId(2) },
                }],
                vreg_types: vec![crate::typectx::T_I32; 3],
            }],
            entry: 0,
        };

        let changed = simplify_phis(&mut m);
        assert!(changed);
        match m.funcs[0].blocks[0].insns[0].op {
            IrOp::Mov { dst, src } => {
                assert_eq!(dst, VRegId(2));
                assert_eq!(src, VRegId(1));
            }
            _ => panic!("expected Mov"),
        }
    }
}

