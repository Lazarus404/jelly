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

// Copy propagation.
//
// This pass rewrites explicit operands through chains of `Mov {dst, src}`.
// It intentionally does NOT rewrite `Call.arg_base` or `Closure.cap_base` because those fields
// denote implicit contiguous vreg windows ([base..base+n)), and rewriting the base could break
// contiguity assumptions.

use crate::ir::{IrModule, IrOp, IrTerminator, VRegId};
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

pub fn copy_propagation(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let vreg_types = &func.vreg_types;

        // Precompute def counts so we only propagate aliases to single-def typed sources.
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

        let mut out_maps: Vec<HashMap<VRegId, VRegId>> = vec![HashMap::new(); nb];
        let mut changed_this_func = true;
        while changed_this_func {
            changed_this_func = false;
            for bid in 0..nb {
                // Meet: keep only alias facts that are identical across all predecessors.
                let mut alias: HashMap<VRegId, VRegId> = HashMap::new();
                let mut first = true;
                for &p in &preds[bid] {
                    if first {
                        alias = out_maps[p].clone();
                        first = false;
                    } else {
                        let pred_map = &out_maps[p];
                        alias.retain(|k, v| pred_map.get(k) == Some(v));
                    }
                }
                if first {
                    alias.clear();
                }

                let block = &mut func.blocks[bid];
                for ins in &mut block.insns {
                    changed |= rewrite_op_uses(&mut ins.op, &mut alias);

                    // Record copy aliases for subsequent instructions in this path.
                    if let IrOp::Mov { dst, src } = ins.op {
                        let src = resolve(src, &mut alias);
                        // Only propagate true copies. `Mov` is also used as a placeholder for certain
                        // type coercions (eg I16 -> I32) that are legalized later in codegen; those
                        // must NOT be treated as aliases here.
                        let dst_tid = vreg_types.get(dst.0 as usize).copied();
                        let src_tid = vreg_types.get(src.0 as usize).copied();
                        if src != dst
                            && dst_tid.is_some_and(|dt| Some(dt) == src_tid)
                            && def_counts.get(src.0 as usize).copied().unwrap_or(0) <= 1
                        {
                            alias.insert(dst, src);
                        } else {
                            alias.remove(&dst);
                        }
                    } else if let IrOp::Phi { dst, .. } = ins.op {
                        // If all incoming values are identical (after resolving through existing aliases),
                        // then this phi is a pure copy and we can treat it as an alias for subsequent uses.
                        let incomings = match &ins.op {
                            IrOp::Phi { incomings, .. } => incomings,
                            _ => unreachable!("phi arm must match a phi op"),
                        };
                        let Some((_, first_in)) = incomings.first() else {
                            alias.remove(&dst);
                            continue;
                        };
                        let cand = resolve(*first_in, &mut alias);
                        let all_same = incomings
                            .iter()
                            .all(|(_, v)| resolve(*v, &mut alias) == cand);

                        let dst_tid = vreg_types.get(dst.0 as usize).copied();
                        let cand_tid = vreg_types.get(cand.0 as usize).copied();
                        if all_same
                            && cand != dst
                            && dst_tid.is_some_and(|dt| Some(dt) == cand_tid)
                            && def_counts.get(cand.0 as usize).copied().unwrap_or(0) <= 1
                        {
                            alias.insert(dst, cand);
                        } else {
                            alias.remove(&dst);
                        }
                    } else if let Some(dst) = ins.op.def() {
                        alias.remove(&dst);
                    }
                }

                changed |= rewrite_term_uses(&mut block.term, &mut alias);

                if out_maps[bid] != alias {
                    out_maps[bid] = alias;
                    changed_this_func = true;
                }
            }
        }
    }

    changed
}

fn resolve(v: VRegId, alias: &mut HashMap<VRegId, VRegId>) -> VRegId {
    let mut cur = v;
    // Follow at most a bounded number of steps to avoid accidental cycles
    // (shouldn't happen in SSA-ish IR, but be defensive).
    for _ in 0..64 {
        let next = match alias.get(&cur).copied() {
            Some(n) => n,
            None => break,
        };
        if next == cur {
            break;
        }
        cur = next;
    }
    // Compress one step.
    if cur != v {
        alias.insert(v, cur);
    }
    cur
}

fn rewrite_term_uses(t: &mut IrTerminator, alias: &mut HashMap<VRegId, VRegId>) -> bool {
    match t {
        IrTerminator::Ret { value } => {
            let v2 = resolve(*value, alias);
            if v2 != *value {
                *value = v2;
                return true;
            }
            false
        }
        IrTerminator::JmpIf { cond, .. } => {
            let v2 = resolve(*cond, alias);
            if v2 != *cond {
                *cond = v2;
                return true;
            }
            false
        }
        IrTerminator::SwitchKind { kind, .. } => {
            let v2 = resolve(*kind, alias);
            if v2 != *kind {
                *kind = v2;
                return true;
            }
            false
        }
        IrTerminator::Jmp { .. } | IrTerminator::Unreachable => false,
    }
}

fn rewrite_op_uses(op: &mut IrOp, alias: &mut HashMap<VRegId, VRegId>) -> bool {
    use IrOp::*;
    let mut changed = false;
    match op {
        Mov { src, .. } => {
            let v2 = resolve(*src, alias);
            if v2 != *src {
                *src = v2;
                changed = true;
            }
        }
        AddI32 { a, b, .. }
        | SubI32 { a, b, .. }
        | MulI32 { a, b, .. }
        | EqI32 { a, b, .. }
        | LtI32 { a, b, .. }
        | AddI64 { a, b, .. }
        | SubI64 { a, b, .. }
        | MulI64 { a, b, .. }
        | EqI64 { a, b, .. }
        | LtI64 { a, b, .. }
        | AddF16 { a, b, .. }
        | SubF16 { a, b, .. }
        | MulF16 { a, b, .. }
        | AddF32 { a, b, .. }
        | SubF32 { a, b, .. }
        | MulF32 { a, b, .. }
        | DivF32 { a, b, .. }
        | EqF32 { a, b, .. }
        | LtF32 { a, b, .. }
        | AddF64 { a, b, .. }
        | SubF64 { a, b, .. }
        | MulF64 { a, b, .. }
        | DivF64 { a, b, .. }
        | EqF64 { a, b, .. }
        | LtF64 { a, b, .. }
        | Physeq { a, b, .. }
        | BytesConcat2 { a, b, .. }
        | ListCons { head: a, tail: b, .. } => {
            let a2 = resolve(*a, alias);
            let b2 = resolve(*b, alias);
            if a2 != *a {
                *a = a2;
                changed = true;
            }
            if b2 != *b {
                *b = b2;
                changed = true;
            }
        }
        NegI32 { src, .. }
        | NegI64 { src, .. }
        | NegF32 { src, .. }
        | NegF64 { src, .. }
        | NotBool { src, .. }
        | Assert { cond: src }
        | Kindof { src, .. }
        | SextI64 { src, .. }
        | SextI16 { src, .. }
        | TruncI8 { src, .. }
        | TruncI16 { src, .. }
        | F16FromF32 { src, .. }
        | F32FromF16 { src, .. }
        | F32FromI32 { src, .. }
        | F64FromI32 { src, .. }
        | F64FromI64 { src, .. }
        | F64FromF32 { src, .. }
        | F32FromF64 { src, .. }
        | F32FromI64 { src, .. }
        | I64FromF32 { src, .. }
        | I32FromI64 { src, .. }
        | I32FromF64 { src, .. }
        | I64FromF64 { src, .. }
        | I32FromF32 { src, .. }
        | F16FromI32 { src, .. }
        | I32FromF16 { src, .. }
        | BytesNew { len: src, .. }
        | BytesLen { bytes: src, .. }
        | BytesConcatMany { parts: src, .. }
        | ListHead { list: src, .. }
        | ListTail { list: src, .. }
        | ListIsNil { list: src, .. }
        | ArrayNew { len: src, .. }
        | ArrayLen { arr: src, .. }
        | ToDyn { src, .. }
        | FromDynI8 { src, .. }
        | FromDynI16 { src, .. }
        | FromDynI32 { src, .. }
        | FromDynI64 { src, .. }
        | FromDynF16 { src, .. }
        | FromDynF32 { src, .. }
        | FromDynF64 { src, .. }
        | FromDynBool { src, .. }
        | FromDynPtr { src, .. }
        | Throw { payload: src }
        | ObjHasAtom { obj: src, .. }
        | ObjGetAtom { obj: src, .. } => {
            let v2 = resolve(*src, alias);
            if v2 != *src {
                *src = v2;
                changed = true;
            }
        }
        BytesGetU8 { bytes, index, .. } | ArrayGet { arr: bytes, index, .. } => {
            let b2 = resolve(*bytes, alias);
            let i2 = resolve(*index, alias);
            if b2 != *bytes {
                *bytes = b2;
                changed = true;
            }
            if i2 != *index {
                *index = i2;
                changed = true;
            }
        }
        BytesSetU8 { bytes, index, value }
        | ArraySet {
            arr: bytes,
            index,
            value,
        } => {
            let b2 = resolve(*bytes, alias);
            let i2 = resolve(*index, alias);
            let v2 = resolve(*value, alias);
            if b2 != *bytes {
                *bytes = b2;
                changed = true;
            }
            if i2 != *index {
                *index = i2;
                changed = true;
            }
            if v2 != *value {
                *value = v2;
                changed = true;
            }
        }
        ObjSetAtom { obj, value, .. } => {
            let o2 = resolve(*obj, alias);
            let v2 = resolve(*value, alias);
            if o2 != *obj {
                *obj = o2;
                changed = true;
            }
            if v2 != *value {
                *value = v2;
                changed = true;
            }
        }
        ObjGet { obj, atom, .. } => {
            let o2 = resolve(*obj, alias);
            let a2 = resolve(*atom, alias);
            if o2 != *obj {
                *obj = o2;
                changed = true;
            }
            if a2 != *atom {
                *atom = a2;
                changed = true;
            }
        }
        ObjSet { obj, atom, value } => {
            let o2 = resolve(*obj, alias);
            let a2 = resolve(*atom, alias);
            let v2 = resolve(*value, alias);
            if o2 != *obj {
                *obj = o2;
                changed = true;
            }
            if a2 != *atom {
                *atom = a2;
                changed = true;
            }
            if v2 != *value {
                *value = v2;
                changed = true;
            }
        }
        BindThis { func, this, .. } => {
            let f2 = resolve(*func, alias);
            let t2 = resolve(*this, alias);
            if f2 != *func {
                *func = f2;
                changed = true;
            }
            if t2 != *this {
                *this = t2;
                changed = true;
            }
        }
        // NOTE: Do not rewrite `arg_base` (implicit window); do rewrite callee.
        Call { callee, .. } => {
            let c2 = resolve(*callee, alias);
            if c2 != *callee {
                *callee = c2;
                changed = true;
            }
        }
        // NOTE: Do not rewrite `cap_base` (implicit window).
        Closure { .. } => {}
        Phi { incomings, .. } => {
            for (_, v) in incomings.iter_mut() {
                let v2 = resolve(*v, alias);
                if v2 != *v {
                    *v = v2;
                    changed = true;
                }
            }
        }
        ConstI32 { .. }
        | ConstI8Imm { .. }
        | ConstF16 { .. }
        | ConstI64 { .. }
        | ConstF32 { .. }
        | ConstF64 { .. }
        | ConstBool { .. }
        | ConstNull { .. }
        | ConstBytes { .. }
        | ConstAtom { .. }
        | ConstFun { .. }
        | ListNil { .. }
        | ObjNew { .. }
        | Try { .. }
        | EndTry => {}
    }
    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::ir::{BlockId, IrBlock, IrBuilder, IrFunction, IrInsn, IrModule, IrTerminator};
    use crate::typectx::{T_BOOL, T_I32};

    #[test]
    fn copy_prop_rewrites_operands_through_movs() {
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
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(0), imm: 1 },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::Mov { dst: VRegId(1), src: VRegId(0) },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::Mov { dst: VRegId(2), src: VRegId(1) },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::AddI32 {
                                dst: VRegId(3),
                                a: VRegId(2),
                                b: VRegId(2),
                            },
                        },
                    ],
                    term: IrTerminator::Ret { value: VRegId(3) },
                }],
                vreg_types: vec![crate::typectx::T_I32; 4],
            }],
            entry: 0,
        };

        let changed = copy_propagation(&mut m);
        assert!(changed, "expected copy propagation to rewrite some operands");

        let b0 = &m.funcs[0].blocks[0];
        match &b0.insns[3].op {
            IrOp::AddI32 { a, b, .. } => {
                assert_eq!(*a, VRegId(0));
                assert_eq!(*b, VRegId(0));
            }
            _ => panic!("expected AddI32"),
        }
    }

    #[test]
    fn copy_prop_can_propagate_across_basic_blocks() {
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
                        insns: vec![
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::ConstI32 { dst: VRegId(0), imm: 1 },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::Mov { dst: VRegId(1), src: VRegId(0) },
                            },
                        ],
                        term: IrTerminator::Jmp { target: BlockId(1) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::AddI32 {
                                dst: VRegId(2),
                                a: VRegId(1),
                                b: VRegId(1),
                            },
                        }],
                        term: IrTerminator::Ret { value: VRegId(2) },
                    },
                ],
                vreg_types: vec![crate::typectx::T_I32; 3],
            }],
            entry: 0,
        };

        let changed = copy_propagation(&mut m);
        assert!(changed, "expected copy propagation to rewrite some operands");
        match &m.funcs[0].blocks[1].insns[0].op {
            IrOp::AddI32 { a, b, .. } => {
                assert_eq!(*a, VRegId(0));
                assert_eq!(*b, VRegId(0));
            }
            _ => panic!("expected AddI32"),
        }
    }

    #[test]
    fn copy_prop_is_conservative_when_predecessors_disagree() {
        // Diamond CFG where v1 aliases v0 only on one path; join must not rewrite v1 -> v0.
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
                        insns: vec![
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::ConstBool { dst: VRegId(3), imm: true },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::ConstI32 { dst: VRegId(0), imm: 1 },
                            },
                        ],
                        term: IrTerminator::JmpIf {
                            cond: VRegId(3),
                            then_tgt: BlockId(1),
                            else_tgt: BlockId(2),
                        },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::Mov { dst: VRegId(1), src: VRegId(0) },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(3) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(1), imm: 2 },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(3) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::AddI32 {
                                dst: VRegId(2),
                                a: VRegId(1),
                                b: VRegId(1),
                            },
                        }],
                        term: IrTerminator::Ret { value: VRegId(2) },
                    },
                ],
                vreg_types: vec![
                    crate::typectx::T_I32,  // v0
                    crate::typectx::T_I32,  // v1
                    crate::typectx::T_I32,  // v2
                    crate::typectx::T_BOOL, // v3 cond
                ],
            }],
            entry: 0,
        };

        let _changed = copy_propagation(&mut m);
        match &m.funcs[0].blocks[3].insns[0].op {
            IrOp::AddI32 { a, b, .. } => {
                assert_eq!(*a, VRegId(1));
                assert_eq!(*b, VRegId(1));
            }
            _ => panic!("expected AddI32"),
        }
    }

    #[test]
    fn copy_prop_can_alias_through_identity_phi() {
        let mut b = IrBuilder::new(Some("t".to_string()));
        let v0 = b.new_vreg(T_I32);
        let vcond = b.new_vreg(T_BOOL);
        let vphi = b.new_vreg(T_I32);
        let vout = b.new_vreg(T_I32);

        let bb_then = b.new_block(Some("then".to_string()));
        let bb_else = b.new_block(Some("else".to_string()));
        let bb_join = b.new_block(Some("join".to_string()));

        // entry:
        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 7 });
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
                incomings: vec![(bb_then, v0), (bb_else, v0)],
            },
        );
        b.emit(Span::point(0), IrOp::Mov { dst: vout, src: vphi });
        b.term(IrTerminator::Ret { value: vout });

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
        let changed = copy_propagation(&mut m);
        assert!(changed, "expected copy propagation to rewrite some operands");

        let f = &m.funcs[0];
        let join = &f.blocks[bb_join.0 as usize];
        match join.insns[1].op {
            IrOp::Mov { src, .. } => assert_eq!(src, v0),
            _ => panic!("expected Mov"),
        }
    }
}

