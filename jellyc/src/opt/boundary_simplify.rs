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

// Simplify redundant Dynamic boundary ops.
//
// This pass is intentionally conservative: it only removes runtime checks when a `Dynamic`
// value is proven to have come from `ToDyn` of a typed value with the same static type as
// the `FromDyn*` destination.
//
// Unlike the original block-local version, we propagate provenance across basic blocks via a
// small forward dataflow. We only track provenance when the typed origin vreg is single-def,
// since provenance is keyed by vreg id (not SSA version).

use crate::ir::{IrModule, IrOp, VRegId};
use crate::typectx::T_DYNAMIC;
use std::collections::HashMap;

fn block_successors(term: &crate::ir::IrTerminator) -> Vec<usize> {
    use crate::ir::IrTerminator::*;
    match term {
        Jmp { target } => vec![target.0 as usize],
        JmpIf { then_tgt, else_tgt, .. } => vec![then_tgt.0 as usize, else_tgt.0 as usize],
        SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        Ret { .. } | Unreachable => vec![],
    }
}

/// Simplify redundant `ToDyn`/`FromDyn*` sequences.
/// Returns true if any change was made.
pub fn simplify_dynamic_boundaries(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        // Precompute def counts so we only track provenance from single-def typed vregs.
        let mut def_counts: Vec<u32> = vec![0; func.vreg_types.len()];
        for b in &func.blocks {
            for ins in &b.insns {
                if let Some(d) = ins.op.def() {
                    let i = d.0 as usize;
                    if i < def_counts.len() {
                        def_counts[i] = def_counts[i].saturating_add(1);
                    }
                }
            }
        }

        // Build predecessor lists from normal CFG edges only.
        // (We intentionally do NOT propagate facts along exception edges; catch blocks are treated as unknown.)
        let nb = func.blocks.len();
        let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nb];
        for (i, b) in func.blocks.iter().enumerate() {
            for succ in block_successors(&b.term) {
                if succ < nb {
                    preds[succ].push(i);
                }
            }
        }

        // Forward dataflow: dyn vreg -> typed origin vreg (from ToDyn).
        let mut out_maps: Vec<HashMap<VRegId, VRegId>> = vec![HashMap::new(); nb];

        let mut changed_this_func = true;
        while changed_this_func {
            changed_this_func = false;

            for bid in 0..nb {
                // Meet: keep only facts that are identical across all predecessors.
                let mut dyn_origin: HashMap<VRegId, VRegId> = HashMap::new();
                let mut first = true;
                for &p in &preds[bid] {
                    if first {
                        dyn_origin = out_maps[p].clone();
                        first = false;
                    } else {
                        let pred_map = &out_maps[p];
                        dyn_origin.retain(|k, v| pred_map.get(k) == Some(v));
                    }
                }
                if first {
                    dyn_origin.clear();
                }

                let block = &mut func.blocks[bid];
                for ins in &mut block.insns {
                    // If a vreg is redefined, any previous origin fact about it is invalid.
                    if let Some(d) = ins.op.def() {
                        if !matches!(ins.op, IrOp::ToDyn { .. } | IrOp::Mov { .. } | IrOp::Phi { .. }) {
                            dyn_origin.remove(&d);
                        }
                    }

                    match ins.op {
                        IrOp::ToDyn { dst, src } => {
                            let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                            let src_tid = func.vreg_types.get(src.0 as usize).copied().unwrap_or(T_DYNAMIC);

                            // `ToDyn` from an already-Dynamic src is just a copy.
                            if dst_tid == T_DYNAMIC && src_tid == T_DYNAMIC {
                                ins.op = IrOp::Mov { dst, src };
                                changed = true;

                                // Preserve any origin fact from src if present.
                                if let Some(&o) = dyn_origin.get(&src) {
                                    dyn_origin.insert(dst, o);
                                } else {
                                    dyn_origin.remove(&dst);
                                }
                                continue;
                            }

                            // Record provenance only for Dynamic dst, and only when the typed origin is single-def.
                            // (Params/captures have 0 explicit defs, which is still single-def.)
                            if dst_tid == T_DYNAMIC
                                && src_tid != T_DYNAMIC
                                && def_counts.get(src.0 as usize).copied().unwrap_or(0) <= 1
                            {
                                dyn_origin.insert(dst, src);
                            } else {
                                dyn_origin.remove(&dst);
                            }
                        }
                        IrOp::Mov { dst, src } => {
                            let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                            let src_tid = func.vreg_types.get(src.0 as usize).copied().unwrap_or(T_DYNAMIC);
                            if dst_tid == T_DYNAMIC && src_tid == T_DYNAMIC {
                                if let Some(&o) = dyn_origin.get(&src) {
                                    dyn_origin.insert(dst, o);
                                } else {
                                    dyn_origin.remove(&dst);
                                }
                            } else {
                                dyn_origin.remove(&dst);
                            }
                        }
                        IrOp::Phi { dst, .. } => {
                            let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                            if dst_tid != T_DYNAMIC {
                                dyn_origin.remove(&dst);
                                continue;
                            }
                            dyn_origin.remove(&dst);
                            let incomings = match &ins.op {
                                IrOp::Phi { incomings, .. } => incomings,
                                _ => unreachable!("phi arm must match a phi op"),
                            };
                            let Some((_, first)) = incomings.first() else {
                                continue;
                            };
                            let Some(&origin0) = dyn_origin.get(first) else {
                                continue;
                            };
                            if def_counts.get(origin0.0 as usize).copied().unwrap_or(0) > 1 {
                                continue;
                            }
                            let ok = incomings.iter().all(|(_, v)| dyn_origin.get(v) == Some(&origin0));
                            let origin_tid =
                                func.vreg_types.get(origin0.0 as usize).copied().unwrap_or(T_DYNAMIC);
                            if ok && origin_tid != T_DYNAMIC {
                                dyn_origin.insert(dst, origin0);
                            }
                        }
                        IrOp::FromDynI8 { dst, src }
                        | IrOp::FromDynI16 { dst, src }
                        | IrOp::FromDynI32 { dst, src }
                        | IrOp::FromDynI64 { dst, src }
                        | IrOp::FromDynF16 { dst, src }
                        | IrOp::FromDynF32 { dst, src }
                        | IrOp::FromDynF64 { dst, src }
                        | IrOp::FromDynBool { dst, src }
                        | IrOp::FromDynPtr { dst, src } => {
                            let Some(&origin) = dyn_origin.get(&src) else {
                                continue;
                            };
                            if def_counts.get(origin.0 as usize).copied().unwrap_or(0) > 1 {
                                continue;
                            }

                            let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                            let origin_tid = func.vreg_types.get(origin.0 as usize).copied().unwrap_or(T_DYNAMIC);

                            // Only eliminate when the unbox is a no-op w.r.t static types.
                            if dst_tid == origin_tid && dst_tid != T_DYNAMIC {
                                ins.op = IrOp::Mov { dst, src: origin };
                                changed = true;
                            }
                        }
                        _ => {}
                    }
                }

                if out_maps[bid] != dyn_origin {
                    out_maps[bid] = dyn_origin;
                    changed_this_func = true;
                }
            }
        }
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{IrBuilder, IrModule, IrTerminator};
    use crate::typectx::{T_BOOL, T_DYNAMIC, T_I32};
    use crate::ast::Span;

    #[test]
    fn eliminates_from_dyn_across_basic_blocks_when_proven() {
        let mut b = IrBuilder::new(Some("t".to_string()));
        let v0 = b.new_vreg(T_I32);
        let v1 = b.new_vreg(T_DYNAMIC);
        let v2 = b.new_vreg(T_I32);

        let bb1 = b.new_block(Some("bb1".to_string()));

        // entry:
        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 123 });
        b.emit(Span::point(0), IrOp::ToDyn { dst: v1, src: v0 });
        b.term(IrTerminator::Jmp { target: bb1 });

        // bb1:
        b.set_block(bb1);
        b.emit(Span::point(0), IrOp::FromDynI32 { dst: v2, src: v1 });
        b.term(IrTerminator::Ret { value: v2 });

        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![b.func],
            entry: 0,
        };

        let changed = simplify_dynamic_boundaries(&mut m);
        assert!(changed, "expected boundary simplify to change IR");

        let ins = &m.funcs[0].blocks[bb1.0 as usize].insns[0].op;
        assert!(
            matches!(ins, IrOp::Mov { dst, src } if *dst == v2 && *src == v0),
            "expected FromDynI32 to become Mov from typed origin, got {ins:?}"
        );
    }

    #[test]
    fn eliminates_from_dyn_across_phi_join_when_proven() {
        let mut b = IrBuilder::new(Some("t".to_string()));
        let v0 = b.new_vreg(T_I32);
        let vcond = b.new_vreg(T_BOOL);
        let v1 = b.new_vreg(T_DYNAMIC);
        let vphi = b.new_vreg(T_DYNAMIC);
        let v2 = b.new_vreg(T_I32);

        let bb_then = b.new_block(Some("then".to_string()));
        let bb_else = b.new_block(Some("else".to_string()));
        let bb_join = b.new_block(Some("join".to_string()));

        // entry:
        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 123 });
        b.emit(Span::point(0), IrOp::ToDyn { dst: v1, src: v0 });
        b.emit(Span::point(0), IrOp::ConstBool { dst: vcond, imm: true });
        b.term(IrTerminator::JmpIf {
            cond: vcond,
            then_tgt: bb_then,
            else_tgt: bb_else,
        });

        // then:
        b.set_block(bb_then);
        b.term(IrTerminator::Jmp { target: bb_join });

        // else:
        b.set_block(bb_else);
        b.term(IrTerminator::Jmp { target: bb_join });

        // join:
        b.set_block(bb_join);
        b.emit(
            Span::point(0),
            IrOp::Phi {
                dst: vphi,
                incomings: vec![(bb_then, v1), (bb_else, v1)],
            },
        );
        b.emit(Span::point(0), IrOp::FromDynI32 { dst: v2, src: vphi });
        b.term(IrTerminator::Ret { value: v2 });

        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![b.func],
            entry: 0,
        };

        let changed = simplify_dynamic_boundaries(&mut m);
        assert!(changed, "expected boundary simplify to change IR");

        let join = &m.funcs[0].blocks[bb_join.0 as usize];
        let ins = &join.insns[1].op;
        assert!(
            matches!(ins, IrOp::Mov { dst, src } if *dst == v2 && *src == v0),
            "expected FromDynI32 to become Mov from typed origin, got {ins:?}"
        );
    }
}

