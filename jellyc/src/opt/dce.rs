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

// Dead code elimination: remove instructions whose result is never used.
//
// This pass is deliberately conservative: it never removes ops with side effects or ops that
// can trap even when their result is unused. (In Jelly, many "load-like" ops can trap.)

use crate::ir::{IrModule, IrOp, IrTerminator, VRegId};

/// True if the instruction has side effects and must never be removed.
/// Includes ops that can trap (control flow) even when their result is unused.
fn has_side_effects(op: &IrOp) -> bool {
    matches!(
        op,
        IrOp::Call { .. }
            | IrOp::Throw { .. }
            | IrOp::Try { .. }
            | IrOp::EndTry
            | IrOp::BytesSetU8 { .. }
            | IrOp::BytesGetU8 { .. }
            | IrOp::BytesLen { .. }
            | IrOp::BytesConcat2 { .. }
            | IrOp::BytesConcatMany { .. }
            | IrOp::ArraySet { .. }
            | IrOp::ArrayGet { .. }
            | IrOp::ArrayLen { .. }
            | IrOp::ListHead { .. }
            | IrOp::ListTail { .. }
            | IrOp::ListIsNil { .. }
            | IrOp::ObjSetAtom { .. }
            | IrOp::ObjGet { .. }
            | IrOp::ObjHasAtom { .. }
            | IrOp::ObjGetAtom { .. }
            | IrOp::FromDynI8 { .. }
            | IrOp::FromDynI16 { .. }
            | IrOp::FromDynI32 { .. }
            | IrOp::FromDynI64 { .. }
            | IrOp::FromDynF16 { .. }
            | IrOp::FromDynF32 { .. }
            | IrOp::FromDynF64 { .. }
            | IrOp::FromDynBool { .. }
            | IrOp::FromDynPtr { .. }
    )
}

/// Remove instructions whose defined vreg is never used.
/// Never removes instructions with side effects (Call, Throw, stores, etc.).
/// Returns true if any change was made.
pub fn dead_code_elimination(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let nregs = func.vreg_types.len();

        // Use counts for each vreg across the whole function (insn operands + terminators).
        let mut use_count: Vec<u32> = vec![0; nregs];
        for block in &func.blocks {
            for ins in &block.insns {
                for v in ins.op.uses() {
                    let i = v.0 as usize;
                    if i < use_count.len() {
                        use_count[i] = use_count[i].saturating_add(1);
                    }
                }
            }
            match &block.term {
                IrTerminator::Ret { value } => {
                    let i = value.0 as usize;
                    if i < use_count.len() {
                        use_count[i] = use_count[i].saturating_add(1);
                    }
                }
                IrTerminator::JmpIf { cond, .. } => {
                    let i = cond.0 as usize;
                    if i < use_count.len() {
                        use_count[i] = use_count[i].saturating_add(1);
                    }
                }
                IrTerminator::SwitchKind { kind, .. } => {
                    let i = kind.0 as usize;
                    if i < use_count.len() {
                        use_count[i] = use_count[i].saturating_add(1);
                    }
                }
                IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
            }
        }

        // Index instructions and record unique def sites per vreg.
        #[derive(Clone, Copy, Debug)]
        enum DefSite {
            None,
            One(usize), // insn id
            Multi,
        }

        #[derive(Clone, Debug)]
        struct InsnInfo {
            def: Option<VRegId>,
            uses: Vec<VRegId>,
            side_effects: bool,
        }

        let mut def_site: Vec<DefSite> = vec![DefSite::None; nregs];
        let mut infos: Vec<InsnInfo> = Vec::new();
        let mut block_insn_ids: Vec<Vec<usize>> = vec![Vec::new(); func.blocks.len()];

        for (bi, block) in func.blocks.iter().enumerate() {
            for ins in &block.insns {
                let id = infos.len();
                let def = ins.op.def();
                if let Some(d) = def {
                    let di = d.0 as usize;
                    if di < def_site.len() {
                        def_site[di] = match def_site[di] {
                            DefSite::None => DefSite::One(id),
                            DefSite::One(_) | DefSite::Multi => DefSite::Multi,
                        };
                    }
                }
                let uses = ins.op.uses();
                let side_effects = has_side_effects(&ins.op);
                infos.push(InsnInfo {
                    def,
                    uses,
                    side_effects,
                });
                block_insn_ids[bi].push(id);
            }
        }

        let mut removed: Vec<bool> = vec![false; infos.len()];
        let mut worklist: Vec<usize> = Vec::new();

        // Seed: any removable instruction whose def has use_count==0.
        for (id, info) in infos.iter().enumerate() {
            if info.side_effects {
                continue;
            }
            let Some(d) = info.def else { continue };
            let di = d.0 as usize;
            if di < use_count.len() && use_count[di] == 0 {
                worklist.push(id);
            }
        }

        while let Some(id) = worklist.pop() {
            if removed[id] {
                continue;
            }
            let info = &infos[id];
            if info.side_effects {
                continue;
            }
            let Some(d) = info.def else { continue };
            let di = d.0 as usize;
            if di >= use_count.len() || use_count[di] != 0 {
                continue;
            }

            removed[id] = true;
            changed = true;

            // Removing this instruction makes its operands used one fewer time.
            for &v in &info.uses {
                let vi = v.0 as usize;
                if vi >= use_count.len() {
                    continue;
                }
                if use_count[vi] > 0 {
                    use_count[vi] -= 1;
                }
                if use_count[vi] == 0 {
                    if let DefSite::One(def_id) = def_site[vi] {
                        let def_info = &infos[def_id];
                        if !def_info.side_effects {
                            worklist.push(def_id);
                        }
                    }
                }
            }
        }

        // Apply removals per block (keep instruction order stable).
        for (bi, block) in func.blocks.iter_mut().enumerate() {
            if block_insn_ids[bi].is_empty() {
                continue;
            }
            let old_insns = std::mem::take(&mut block.insns);
            let mut new_insns = Vec::with_capacity(old_insns.len());
            for (idx, ins) in old_insns.into_iter().enumerate() {
                let id = block_insn_ids[bi][idx];
                if !removed[id] {
                    new_insns.push(ins);
                }
            }
            block.insns = new_insns;
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
    fn dce_cascades_through_dead_phi_chain() {
        // Diamond with a dead phi result; removing phi should allow removing its incoming consts.
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
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstBool { dst: VRegId(0), imm: true },
                        }],
                        term: IrTerminator::JmpIf {
                            cond: VRegId(0),
                            then_tgt: BlockId(1),
                            else_tgt: BlockId(2),
                        },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(1), imm: 10 },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(3) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(2), imm: 20 },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(3) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::Phi {
                                    dst: VRegId(3),
                                    incomings: vec![(BlockId(1), VRegId(1)), (BlockId(2), VRegId(2))],
                                },
                            },
                            // Keep the program alive via returning the condition vreg.
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::Mov { dst: VRegId(4), src: VRegId(0) },
                            },
                        ],
                        term: IrTerminator::Ret { value: VRegId(4) },
                    },
                ],
                vreg_types: vec![
                    crate::typectx::T_BOOL, // v0
                    crate::typectx::T_I32,  // v1
                    crate::typectx::T_I32,  // v2
                    crate::typectx::T_I32,  // v3 phi
                    crate::typectx::T_BOOL, // v4
                ],
            }],
            entry: 0,
        };

        let changed = dead_code_elimination(&mut m);
        assert!(changed);
        let join = &m.funcs[0].blocks[3];
        assert!(
            join.insns.iter().all(|ins| !matches!(ins.op, IrOp::Phi { .. })),
            "phi should be removed"
        );
        // Incoming consts should also be removed (their only use was the phi).
        assert!(m.funcs[0].blocks[1].insns.is_empty(), "then const should be removed");
        assert!(m.funcs[0].blocks[2].insns.is_empty(), "else const should be removed");
    }
}
