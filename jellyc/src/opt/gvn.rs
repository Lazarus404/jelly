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

// Global value numbering (GVN) over SSA.
//
// This pass eliminates redundant computations across basic blocks by reusing a dominating
// equivalent expression. Because Jelly IR is in SSA at this point, we can treat vregs as
// immutable values and avoid alias invalidation by restricting GVN to pure, non-trapping ops.

use crate::ir::{IrBlock, IrFunction, IrModule, IrOp, IrTerminator, VRegId};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Key {
    AddI32(u32, u32),
    SubI32(u32, u32),
    MulI32(u32, u32),
    EqI32(u32, u32),
    LtI32(u32, u32),
    AddI64(u32, u32),
    SubI64(u32, u32),
    MulI64(u32, u32),
    EqI64(u32, u32),
    LtI64(u32, u32),
    AddF16(u32, u32),
    SubF16(u32, u32),
    MulF16(u32, u32),
    AddF32(u32, u32),
    SubF32(u32, u32),
    MulF32(u32, u32),
    DivF32(u32, u32),
    EqF32(u32, u32),
    LtF32(u32, u32),
    AddF64(u32, u32),
    SubF64(u32, u32),
    MulF64(u32, u32),
    DivF64(u32, u32),
    EqF64(u32, u32),
    LtF64(u32, u32),
    Physeq(u32, u32),
    NegI32(u32),
    NegI64(u32),
    NegF32(u32),
    NegF64(u32),
    NotBool(u32),
    SextI64(u32),
    SextI16(u32),
    TruncI8(u32),
    TruncI16(u32),
    F16FromF32(u32),
    F32FromF16(u32),
    F32FromI32(u32),
    F64FromI32(u32),
    F64FromI64(u32),
    F64FromF32(u32),
    F32FromF64(u32),
    F32FromI64(u32),
    I64FromF32(u32),
    I32FromI64(u32),
    I32FromF64(u32),
    I64FromF64(u32),
    I32FromF32(u32),
    F16FromI32(u32),
    I32FromF16(u32),
    // Dynamic boundary ops are pure and safe to GVN under dominance (a dominating check
    // either traps first or proves the same fact for all dominated uses).
    ToDyn(u32),
    FromDynI8(u32),
    FromDynI16(u32),
    FromDynI32(u32),
    FromDynI64(u32),
    FromDynF16(u32),
    FromDynF32(u32),
    FromDynF64(u32),
    FromDynBool(u32),
    // Destination type matters for pointer-kind unboxing (Array<T> vs Array<U> are different typed values).
    FromDynPtr(u32, u32),
    // Pure container queries (length does not change under mutation; safe under SSA).
    BytesLen(u32),
    ArrayLen(u32),
    // Pure tag query.
    Kindof(u32),
}

fn key_for_op(op: &IrOp, vreg_types: &[u32]) -> Option<Key> {
    use IrOp::*;
    match op {
        AddI32 { a, b, .. } => Some(Key::AddI32(a.0.min(b.0), a.0.max(b.0))),
        SubI32 { a, b, .. } => Some(Key::SubI32(a.0, b.0)),
        MulI32 { a, b, .. } => Some(Key::MulI32(a.0.min(b.0), a.0.max(b.0))),
        EqI32 { a, b, .. } => Some(Key::EqI32(a.0.min(b.0), a.0.max(b.0))),
        LtI32 { a, b, .. } => Some(Key::LtI32(a.0, b.0)),
        Physeq { a, b, .. } => Some(Key::Physeq(a.0.min(b.0), a.0.max(b.0))),
        NegI32 { src, .. } => Some(Key::NegI32(src.0)),
        NegI64 { src, .. } => Some(Key::NegI64(src.0)),
        NegF32 { src, .. } => Some(Key::NegF32(src.0)),
        NegF64 { src, .. } => Some(Key::NegF64(src.0)),
        NotBool { src, .. } => Some(Key::NotBool(src.0)),
        AddI64 { a, b, .. } => Some(Key::AddI64(a.0.min(b.0), a.0.max(b.0))),
        SubI64 { a, b, .. } => Some(Key::SubI64(a.0, b.0)),
        MulI64 { a, b, .. } => Some(Key::MulI64(a.0.min(b.0), a.0.max(b.0))),
        EqI64 { a, b, .. } => Some(Key::EqI64(a.0.min(b.0), a.0.max(b.0))),
        LtI64 { a, b, .. } => Some(Key::LtI64(a.0, b.0)),
        AddF16 { a, b, .. } => Some(Key::AddF16(a.0.min(b.0), a.0.max(b.0))),
        SubF16 { a, b, .. } => Some(Key::SubF16(a.0, b.0)),
        MulF16 { a, b, .. } => Some(Key::MulF16(a.0.min(b.0), a.0.max(b.0))),
        AddF32 { a, b, .. } => Some(Key::AddF32(a.0.min(b.0), a.0.max(b.0))),
        SubF32 { a, b, .. } => Some(Key::SubF32(a.0, b.0)),
        MulF32 { a, b, .. } => Some(Key::MulF32(a.0.min(b.0), a.0.max(b.0))),
        DivF32 { a, b, .. } => Some(Key::DivF32(a.0, b.0)),
        EqF32 { a, b, .. } => Some(Key::EqF32(a.0.min(b.0), a.0.max(b.0))),
        LtF32 { a, b, .. } => Some(Key::LtF32(a.0, b.0)),
        AddF64 { a, b, .. } => Some(Key::AddF64(a.0.min(b.0), a.0.max(b.0))),
        SubF64 { a, b, .. } => Some(Key::SubF64(a.0, b.0)),
        MulF64 { a, b, .. } => Some(Key::MulF64(a.0.min(b.0), a.0.max(b.0))),
        DivF64 { a, b, .. } => Some(Key::DivF64(a.0, b.0)),
        EqF64 { a, b, .. } => Some(Key::EqF64(a.0.min(b.0), a.0.max(b.0))),
        LtF64 { a, b, .. } => Some(Key::LtF64(a.0, b.0)),
        SextI64 { src, .. } => Some(Key::SextI64(src.0)),
        SextI16 { src, .. } => Some(Key::SextI16(src.0)),
        TruncI8 { src, .. } => Some(Key::TruncI8(src.0)),
        TruncI16 { src, .. } => Some(Key::TruncI16(src.0)),
        F16FromF32 { src, .. } => Some(Key::F16FromF32(src.0)),
        F32FromF16 { src, .. } => Some(Key::F32FromF16(src.0)),
        F32FromI32 { src, .. } => Some(Key::F32FromI32(src.0)),
        F64FromI32 { src, .. } => Some(Key::F64FromI32(src.0)),
        F64FromI64 { src, .. } => Some(Key::F64FromI64(src.0)),
        F64FromF32 { src, .. } => Some(Key::F64FromF32(src.0)),
        F32FromF64 { src, .. } => Some(Key::F32FromF64(src.0)),
        F32FromI64 { src, .. } => Some(Key::F32FromI64(src.0)),
        I64FromF32 { src, .. } => Some(Key::I64FromF32(src.0)),
        I32FromI64 { src, .. } => Some(Key::I32FromI64(src.0)),
        I32FromF64 { src, .. } => Some(Key::I32FromF64(src.0)),
        I64FromF64 { src, .. } => Some(Key::I64FromF64(src.0)),
        I32FromF32 { src, .. } => Some(Key::I32FromF32(src.0)),
        F16FromI32 { src, .. } => Some(Key::F16FromI32(src.0)),
        I32FromF16 { src, .. } => Some(Key::I32FromF16(src.0)),
        ToDyn { src, .. } => Some(Key::ToDyn(src.0)),
        FromDynI8 { src, .. } => Some(Key::FromDynI8(src.0)),
        FromDynI16 { src, .. } => Some(Key::FromDynI16(src.0)),
        FromDynI32 { src, .. } => Some(Key::FromDynI32(src.0)),
        FromDynI64 { src, .. } => Some(Key::FromDynI64(src.0)),
        FromDynF16 { src, .. } => Some(Key::FromDynF16(src.0)),
        FromDynF32 { src, .. } => Some(Key::FromDynF32(src.0)),
        FromDynF64 { src, .. } => Some(Key::FromDynF64(src.0)),
        FromDynBool { src, .. } => Some(Key::FromDynBool(src.0)),
        FromDynPtr { dst, src } => {
            let dst_tid = vreg_types.get(dst.0 as usize).copied().unwrap_or(0);
            Some(Key::FromDynPtr(src.0, dst_tid))
        }
        BytesLen { bytes, .. } => Some(Key::BytesLen(bytes.0)),
        ArrayLen { arr, .. } => Some(Key::ArrayLen(arr.0)),
        Kindof { src, .. } => Some(Key::Kindof(src.0)),
        _ => None,
    }
}

fn term_succs(term: &IrTerminator) -> Vec<usize> {
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

fn block_order_rpo(func: &IrFunction) -> Vec<usize> {
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

fn compute_idom(func: &IrFunction, rpo: &[usize]) -> Vec<Option<usize>> {
    let n = func.blocks.len();
    let entry = func.entry.0 as usize;
    let mut preds: Vec<Vec<usize>> = vec![Vec::new(); n];
    let mut reachable = vec![false; n];
    for &b in rpo {
        reachable[b] = true;
    }
    for &b in rpo {
        for s in term_succs(&func.blocks[b].term) {
            if s < n && reachable[s] {
                preds[s].push(b);
            }
        }
    }
    for p in &mut preds {
        p.sort_unstable();
        p.dedup();
    }

    let mut rpo_index: Vec<usize> = vec![usize::MAX; n];
    for (i, &b) in rpo.iter().enumerate() {
        rpo_index[b] = i;
    }

    fn intersect(mut b1: usize, mut b2: usize, idom: &[Option<usize>], rpo_index: &[usize]) -> usize {
        while b1 != b2 {
            while rpo_index[b1] > rpo_index[b2] {
                b1 = idom[b1].expect("idom must be set");
            }
            while rpo_index[b2] > rpo_index[b1] {
                b2 = idom[b2].expect("idom must be set");
            }
        }
        b1
    }

    let mut idom: Vec<Option<usize>> = vec![None; n];
    idom[entry] = Some(entry);

    let mut changed = true;
    while changed {
        changed = false;
        for &b in rpo.iter().skip(1) {
            let mut it = preds[b].iter().copied().filter(|&p| idom[p].is_some());
            let Some(mut new_idom) = it.next() else {
                continue;
            };
            for p in it {
                new_idom = intersect(p, new_idom, &idom, &rpo_index);
            }
            if idom[b] != Some(new_idom) {
                idom[b] = Some(new_idom);
                changed = true;
            }
        }
    }

    idom
}

pub fn global_value_numbering(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let rpo = block_order_rpo(func);
        if rpo.is_empty() {
            continue;
        }
        let idom = compute_idom(func, &rpo);
        let n = func.blocks.len();
        let entry = func.entry.0 as usize;

        let mut children: Vec<Vec<usize>> = vec![Vec::new(); n];
        for b in 0..n {
            if b == entry {
                continue;
            }
            if let Some(p) = idom[b] {
                if p != b {
                    children[p].push(b);
                }
            }
        }
        for ch in &mut children {
            ch.sort_unstable();
        }

        fn dfs_gvn(
            bid: usize,
            blocks: &mut [IrBlock],
            children: &[Vec<usize>],
            vreg_types: &[u32],
            env: &mut HashMap<Key, VRegId>,
            changed: &mut bool,
        ) {
            let mut pushed: Vec<Key> = Vec::new();

            for ins in &mut blocks[bid].insns {
                // Skip phis; they are SSA plumbing and should be handled by copy/phi passes.
                if matches!(ins.op, IrOp::Phi { .. }) {
                    continue;
                }

                let Some(dst) = ins.op.def() else {
                    continue;
                };
                let Some(k) = key_for_op(&ins.op, vreg_types) else {
                    continue;
                };

                if let Some(&existing) = env.get(&k) {
                    if existing != dst {
                        ins.op = IrOp::Mov { dst, src: existing };
                        *changed = true;
                    }
                    continue;
                }

                env.insert(k, dst);
                pushed.push(k);
            }

            for &c in &children[bid] {
                dfs_gvn(c, blocks, children, vreg_types, env, changed);
            }

            for k in pushed.into_iter().rev() {
                env.remove(&k);
            }
        }

        let mut env: HashMap<Key, VRegId> = HashMap::new();
        dfs_gvn(entry, &mut func.blocks, &children, &func.vreg_types, &mut env, &mut changed);
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::ir::{BlockId, IrBlock, IrFunction, IrInsn, IrModule};
    use crate::typectx::{T_ARRAY_I32, T_DYNAMIC, T_I32};

    #[test]
    fn gvn_eliminates_redundant_pure_op_across_blocks() {
        // entry:
        //   v0 = const 1
        //   v1 = const 2
        //   v2 = add v0 v1
        //   jmp b1
        // b1:
        //   v3 = add v0 v1   ; redundant, dominated by v2
        //   ret v3
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
                                op: IrOp::ConstI32 { dst: VRegId(1), imm: 2 },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::AddI32 {
                                    dst: VRegId(2),
                                    a: VRegId(0),
                                    b: VRegId(1),
                                },
                            },
                        ],
                        term: IrTerminator::Jmp { target: BlockId(1) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::AddI32 {
                                dst: VRegId(3),
                                a: VRegId(0),
                                b: VRegId(1),
                            },
                        }],
                        term: IrTerminator::Ret { value: VRegId(3) },
                    },
                ],
                vreg_types: vec![
                    T_I32,
                    T_I32,
                    T_I32,
                    T_I32,
                ],
            }],
            entry: 0,
        };

        let changed = global_value_numbering(&mut m);
        assert!(changed);
        let b1 = &m.funcs[0].blocks[1];
        assert!(
            matches!(b1.insns[0].op, IrOp::Mov { dst: VRegId(3), src: VRegId(2) }),
            "expected redundant add to be replaced by Mov"
        );
    }

    #[test]
    fn gvn_eliminates_redundant_from_dyn_across_blocks() {
        // entry:
        //   v0:I32 = const 123
        //   v1:Any = to_dyn v0
        //   v2:I32 = from_dyn_i32 v1
        //   jmp b1
        // b1:
        //   v3:I32 = from_dyn_i32 v1   ; redundant, dominated by v2
        //   ret v3
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
                                op: IrOp::ConstI32 { dst: VRegId(0), imm: 123 },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::ToDyn { dst: VRegId(1), src: VRegId(0) },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::FromDynI32 { dst: VRegId(2), src: VRegId(1) },
                            },
                        ],
                        term: IrTerminator::Jmp { target: BlockId(1) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::FromDynI32 { dst: VRegId(3), src: VRegId(1) },
                        }],
                        term: IrTerminator::Ret { value: VRegId(3) },
                    },
                ],
                vreg_types: vec![T_I32, T_DYNAMIC, T_I32, T_I32],
            }],
            entry: 0,
        };

        let changed = global_value_numbering(&mut m);
        assert!(changed);
        let b1 = &m.funcs[0].blocks[1];
        assert!(
            matches!(b1.insns[0].op, IrOp::Mov { dst: VRegId(3), src: VRegId(2) }),
            "expected redundant FromDynI32 to be replaced by Mov"
        );
    }

    #[test]
    fn gvn_eliminates_redundant_array_len_across_blocks() {
        // entry:
        //   v0:Array<I32> = ...
        //   v1:I32 = array_len v0
        //   jmp b1
        // b1:
        //   v2:I32 = array_len v0   ; redundant, dominated by v1
        //   ret v2
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
                                op: IrOp::ConstI32 { dst: VRegId(10), imm: 3 },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::ArrayNew {
                                    dst: VRegId(0),
                                    len: VRegId(10),
                                },
                            },
                            IrInsn {
                                span: Span::point(0),
                                op: IrOp::ArrayLen { dst: VRegId(1), arr: VRegId(0) },
                            },
                        ],
                        term: IrTerminator::Jmp { target: BlockId(1) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ArrayLen { dst: VRegId(2), arr: VRegId(0) },
                        }],
                        term: IrTerminator::Ret { value: VRegId(2) },
                    },
                ],
                // Types don't matter for GVN itself, but keep them consistent.
                vreg_types: {
                    let mut v = vec![T_ARRAY_I32, T_I32, T_I32];
                    // pad up to v10
                    while v.len() < 11 {
                        v.push(T_DYNAMIC);
                    }
                    v[10] = T_I32;
                    v
                },
            }],
            entry: 0,
        };

        let changed = global_value_numbering(&mut m);
        assert!(changed);
        let b1 = &m.funcs[0].blocks[1];
        assert!(
            matches!(b1.insns[0].op, IrOp::Mov { dst: VRegId(2), src: VRegId(1) }),
            "expected redundant ArrayLen to be replaced by Mov"
        );
    }
}

