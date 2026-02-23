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

// SSA (Static Single Assignment) conversion for multi-def vregs.
//
// This pass:
// - identifies vregs that are defined multiple times (typically due to `assign` lowering)
// - inserts Phi nodes at dominance frontiers
// - renames uses/defs so each original vreg has a single definition (per SSA)
//
// Exceptions:
// - Jelly's `Try`/`EndTry` are VM-level handlers: on trap/throw, control jumps directly to the
//   `catch_block` with the *current register file state* preserved.
// - That means values observed in the catch region are *not* produced via normal CFG edge moves.
// - For SSA conversion, we therefore:
//   - add conservative CFG edges from blocks containing `Try` to their `catch_block` so catch blocks
//     participate in dominator traversal/renaming (reachability), and
//   - **pin** vregs used in the catch region to avoid SSA-renaming variables whose values must be
//     readable at catch entry (since phi-moves cannot run on exception edges).

use crate::ir::{BlockId, IrInsn, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use std::collections::{HashMap, HashSet};
use std::collections::BTreeSet;

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

fn count_leading_phis(block: &[IrInsn]) -> usize {
    block
        .iter()
        .take_while(|ins| matches!(ins.op, IrOp::Phi { .. }))
        .count()
}

fn new_vreg(vreg_types: &mut Vec<TypeId>, tid: TypeId) -> VRegId {
    let v = VRegId(vreg_types.len() as u32);
    vreg_types.push(tid);
    v
}

fn compute_dom(preds: &[Vec<usize>], entry: usize) -> Vec<Vec<bool>> {
    let n = preds.len();
    let mut dom: Vec<Vec<bool>> = vec![vec![true; n]; n];
    for i in 0..n {
        if i == entry {
            dom[i] = vec![false; n];
            dom[i][entry] = true;
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for b in 0..n {
            if b == entry {
                continue;
            }
            let mut new = vec![true; n];
            if preds[b].is_empty() {
                // Treat "no preds" blocks as separate roots; should be unreachable after cfg_simplify.
                new = vec![false; n];
            } else {
                for &p in &preds[b] {
                    for i in 0..n {
                        new[i] &= dom[p][i];
                    }
                }
            }
            new[b] = true;
            if new != dom[b] {
                dom[b] = new;
                changed = true;
            }
        }
    }
    dom
}

fn compute_idom(dom: &[Vec<bool>], entry: usize) -> Vec<Option<usize>> {
    let n = dom.len();
    let mut idom: Vec<Option<usize>> = vec![None; n];
    idom[entry] = Some(entry);
    for b in 0..n {
        if b == entry {
            continue;
        }
        let strict: Vec<usize> = (0..n).filter(|&d| d != b && dom[b][d]).collect();
        if strict.is_empty() {
            idom[b] = Some(entry);
            continue;
        }
        let mut best = strict[0];
        let mut best_sz = dom[best].iter().filter(|x| **x).count();
        for &d in &strict[1..] {
            let sz = dom[d].iter().filter(|x| **x).count();
            if sz > best_sz {
                best = d;
                best_sz = sz;
            }
        }
        idom[b] = Some(best);
    }
    idom
}

fn rename_use(v: VRegId, is_multi: &[bool], stacks: &[Vec<VRegId>]) -> VRegId {
    let i = v.0 as usize;
    if i < is_multi.len() && is_multi[i] {
        stacks[i].last().copied().unwrap_or(v)
    } else {
        v
    }
}

fn map_op_uses(op: &mut IrOp, mut f: impl FnMut(VRegId) -> VRegId) {
    use IrOp::*;
    match op {
        Mov { src, .. } => *src = f(*src),
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
        | Physeq { a, b, .. } => {
            *a = f(*a);
            *b = f(*b);
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
        | ToDyn { src, .. }
        | FromDynI8 { src, .. }
        | FromDynI16 { src, .. }
        | FromDynI32 { src, .. }
        | FromDynI64 { src, .. }
        | FromDynF16 { src, .. }
        | FromDynF32 { src, .. }
        | FromDynF64 { src, .. }
        | FromDynBool { src, .. }
        | FromDynPtr { src, .. } => *src = f(*src),
        BytesNew { len, .. } | ArrayNew { len, .. } => *len = f(*len),
        BytesLen { bytes, .. } => *bytes = f(*bytes),
        BytesGetU8 { bytes, index, .. } => {
            *bytes = f(*bytes);
            *index = f(*index);
        }
        BytesSetU8 { bytes, index, value } => {
            *bytes = f(*bytes);
            *index = f(*index);
            *value = f(*value);
        }
        BytesConcat2 { a, b, .. } => {
            *a = f(*a);
            *b = f(*b);
        }
        BytesConcatMany { parts, .. } => *parts = f(*parts),
        ListCons { head, tail, .. } => {
            *head = f(*head);
            *tail = f(*tail);
        }
        ListHead { list, .. } | ListTail { list, .. } | ListIsNil { list, .. } => *list = f(*list),
        ArrayLen { arr, .. } => *arr = f(*arr),
        ArrayGet { arr, index, .. } => {
            *arr = f(*arr);
            *index = f(*index);
        }
        ArraySet { arr, index, value } => {
            *arr = f(*arr);
            *index = f(*index);
            *value = f(*value);
        }
        ObjHasAtom { obj, .. } | ObjGetAtom { obj, .. } => *obj = f(*obj),
        ObjSetAtom { obj, value, .. } => {
            *obj = f(*obj);
            *value = f(*value);
        }
        ObjGet { obj, atom, .. } => {
            *obj = f(*obj);
            *atom = f(*atom);
        }
        ObjSet { obj, atom, value } => {
            *obj = f(*obj);
            *atom = f(*atom);
            *value = f(*value);
        }
        Throw { payload } => *payload = f(*payload),
        // NOTE: do not rewrite arg_base/cap_base (implicit windows); rewrite callee / captured values are implicit.
        Call { callee, .. } => *callee = f(*callee),
        Closure { .. } => {}
        BindThis { func, this, .. } => {
            *func = f(*func);
            *this = f(*this);
        }
        Phi { incomings, .. } => {
            for (_, v) in incomings.iter_mut() {
                *v = f(*v);
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
}

fn set_op_def(op: &mut IrOp, new_dst: VRegId) {
    use IrOp::*;
    match op {
        Try { catch_dst, .. } => *catch_dst = new_dst,
        ConstI32 { dst, .. }
        | ConstI8Imm { dst, .. }
        | ConstF16 { dst, .. }
        | ConstI64 { dst, .. }
        | ConstF32 { dst, .. }
        | ConstF64 { dst, .. }
        | ConstBool { dst, .. }
        | ConstNull { dst }
        | ConstBytes { dst, .. }
        | ConstAtom { dst, .. }
        | ConstFun { dst, .. }
        | Mov { dst, .. }
        | AddI32 { dst, .. }
        | SubI32 { dst, .. }
        | MulI32 { dst, .. }
        | NegI32 { dst, .. }
        | EqI32 { dst, .. }
        | LtI32 { dst, .. }
        | AddI64 { dst, .. }
        | SubI64 { dst, .. }
        | MulI64 { dst, .. }
        | NegI64 { dst, .. }
        | EqI64 { dst, .. }
        | LtI64 { dst, .. }
        | AddF16 { dst, .. }
        | SubF16 { dst, .. }
        | MulF16 { dst, .. }
        | AddF32 { dst, .. }
        | SubF32 { dst, .. }
        | MulF32 { dst, .. }
        | DivF32 { dst, .. }
        | NegF32 { dst, .. }
        | EqF32 { dst, .. }
        | LtF32 { dst, .. }
        | AddF64 { dst, .. }
        | SubF64 { dst, .. }
        | MulF64 { dst, .. }
        | DivF64 { dst, .. }
        | NegF64 { dst, .. }
        | EqF64 { dst, .. }
        | LtF64 { dst, .. }
        | NotBool { dst, .. }
        | Physeq { dst, .. }
        | SextI64 { dst, .. }
        | SextI16 { dst, .. }
        | TruncI8 { dst, .. }
        | TruncI16 { dst, .. }
        | F16FromF32 { dst, .. }
        | F32FromF16 { dst, .. }
        | F32FromI32 { dst, .. }
        | F64FromI32 { dst, .. }
        | F64FromI64 { dst, .. }
        | F64FromF32 { dst, .. }
        | F32FromF64 { dst, .. }
        | F32FromI64 { dst, .. }
        | I64FromF32 { dst, .. }
        | I32FromI64 { dst, .. }
        | I32FromF64 { dst, .. }
        | I64FromF64 { dst, .. }
        | I32FromF32 { dst, .. }
        | F16FromI32 { dst, .. }
        | I32FromF16 { dst, .. }
        | Kindof { dst, .. }
        | BytesNew { dst, .. }
        | BytesLen { dst, .. }
        | BytesGetU8 { dst, .. }
        | BytesConcat2 { dst, .. }
        | BytesConcatMany { dst, .. }
        | ListNil { dst }
        | ListCons { dst, .. }
        | ListHead { dst, .. }
        | ListTail { dst, .. }
        | ListIsNil { dst, .. }
        | ArrayNew { dst, .. }
        | ArrayLen { dst, .. }
        | ArrayGet { dst, .. }
        | ObjNew { dst }
        | ObjHasAtom { dst, .. }
        | ObjGetAtom { dst, .. }
        | ObjGet { dst, .. }
        | ToDyn { dst, .. }
        | FromDynI8 { dst, .. }
        | FromDynI16 { dst, .. }
        | FromDynI32 { dst, .. }
        | FromDynI64 { dst, .. }
        | FromDynF16 { dst, .. }
        | FromDynF32 { dst, .. }
        | FromDynF64 { dst, .. }
        | FromDynBool { dst, .. }
        | FromDynPtr { dst, .. }
        | Closure { dst, .. }
        | BindThis { dst, .. }
        | Call { dst, .. }
        | Phi { dst, .. } => *dst = new_dst,
        BytesSetU8 { .. } | ArraySet { .. } | ObjSetAtom { .. } | ObjSet { .. } | Throw { .. } | Assert { .. } | EndTry => {}
    }
}

fn map_term_uses(term: &mut IrTerminator, mut f: impl FnMut(VRegId) -> VRegId) {
    match term {
        IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
        IrTerminator::JmpIf { cond, .. } => *cond = f(*cond),
        IrTerminator::SwitchKind { kind, .. } => *kind = f(*kind),
        IrTerminator::Ret { value } => *value = f(*value),
    }
}

pub fn convert_to_ssa(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let nb = func.blocks.len();
        if nb == 0 {
            continue;
        }

        // Pin vregs that belong to implicit windows (Call args, Closure captures).
        let mut pinned: Vec<bool> = vec![false; func.vreg_types.len()];
        for blk in &func.blocks {
            for ins in &blk.insns {
                match ins.op {
                    IrOp::Call { arg_base, nargs, .. } => {
                        for i in 0..(nargs as u32) {
                            let vi = (arg_base.0 + i) as usize;
                            if vi < pinned.len() {
                                pinned[vi] = true;
                            }
                        }
                    }
                    IrOp::Closure { cap_base, ncaps, .. } => {
                        for i in 0..(ncaps as u32) {
                            let vi = (cap_base.0 + i) as usize;
                            if vi < pinned.len() {
                                pinned[vi] = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Also pin vregs used in the catch region(s).
        //
        // Rationale: the VM transfers control to `catch_block` without executing any IR-level moves.
        // If we SSA-rename a variable that's read in catch, we'd need phi-moves on exception edges
        // to select the "current" version; we don't have a place to run those moves. Pinning keeps
        // these variables in stable vregs across the try region.
        let mut catch_roots: Vec<usize> = Vec::new();
        for blk in &func.blocks {
            for ins in &blk.insns {
                if let IrOp::Try { catch_block, .. } = ins.op {
                    catch_roots.push(catch_block.0 as usize);
                }
            }
        }
        if !catch_roots.is_empty() {
            catch_roots.sort_unstable();
            catch_roots.dedup();
            let mut in_catch_region: Vec<bool> = vec![false; nb];
            let mut work: Vec<usize> = catch_roots.clone();
            while let Some(bi) = work.pop() {
                if bi >= nb || in_catch_region[bi] {
                    continue;
                }
                in_catch_region[bi] = true;
                for s in term_succs(&func.blocks[bi].term) {
                    work.push(s);
                }
            }

            for (bi, blk) in func.blocks.iter().enumerate() {
                if !in_catch_region[bi] {
                    continue;
                }
                for ins in &blk.insns {
                    for v in ins.op.uses() {
                        let vi = v.0 as usize;
                        if vi < pinned.len() {
                            pinned[vi] = true;
                        }
                    }
                }
                match &blk.term {
                    IrTerminator::Ret { value } => {
                        let vi = value.0 as usize;
                        if vi < pinned.len() {
                            pinned[vi] = true;
                        }
                    }
                    IrTerminator::JmpIf { cond, .. } => {
                        let vi = cond.0 as usize;
                        if vi < pinned.len() {
                            pinned[vi] = true;
                        }
                    }
                    IrTerminator::SwitchKind { kind, .. } => {
                        let vi = kind.0 as usize;
                        if vi < pinned.len() {
                            pinned[vi] = true;
                        }
                    }
                    IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
                }
            }
        }

        // Def counts.
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

        let mut is_multi: Vec<bool> = vec![false; func.vreg_types.len()];
        let mut multi_vars: Vec<VRegId> = Vec::new();
        for (i, &c) in def_counts.iter().enumerate() {
            if c > 1 && !pinned.get(i).copied().unwrap_or(false) {
                is_multi[i] = true;
                multi_vars.push(VRegId(i as u32));
            }
        }
        if multi_vars.is_empty() {
            continue;
        }

        // Build a vreg -> multi-var index map for fast membership testing.
        let mut multi_index: Vec<Option<usize>> = vec![None; func.vreg_types.len()];
        for (idx, v) in multi_vars.iter().enumerate() {
            let vi = v.0 as usize;
            if vi < multi_index.len() {
                multi_index[vi] = Some(idx);
            }
        }

        // CFG preds/succs.
        //
        // Note: in addition to terminator edges, we add conservative edges for `Try` handlers so
        // catch blocks are reachable for renaming/dominators.
        let mut succs: Vec<Vec<usize>> = vec![Vec::new(); nb];
        let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nb];
        for (bi, blk) in func.blocks.iter().enumerate() {
            for s in term_succs(&blk.term) {
                if s < nb {
                    succs[bi].push(s);
                    preds[s].push(bi);
                }
            }
            for ins in &blk.insns {
                if let IrOp::Try { catch_block, .. } = ins.op {
                    let s = catch_block.0 as usize;
                    if s < nb {
                        succs[bi].push(s);
                        preds[s].push(bi);
                    }
                }
            }
        }
        for s in &mut succs {
            s.sort_unstable();
            s.dedup();
        }
        for p in &mut preds {
            p.sort_unstable();
            p.dedup();
        }

        let entry = func.entry.0 as usize;
        let dom = compute_dom(&preds, entry);
        let idom = compute_idom(&dom, entry);

        let mut children: Vec<Vec<usize>> = vec![Vec::new(); nb];
        for b in 0..nb {
            if b == entry {
                continue;
            }
            if let Some(p) = idom[b] {
                if p != b {
                    children[p].push(b);
                }
            }
        }

        let df = {
            let mut df: Vec<HashSet<usize>> = vec![HashSet::new(); nb];
            for b in 0..nb {
                for &s in &succs[b] {
                    if idom[s] != Some(b) {
                        df[b].insert(s);
                    }
                }
            }
            // Post-order over dom tree.
            fn post(b: usize, children: &[Vec<usize>], out: &mut Vec<usize>) {
                for &c in &children[b] {
                    post(c, children, out);
                }
                out.push(b);
            }
            let mut order = Vec::new();
            post(entry, &children, &mut order);
            for &b in &order {
                for &c in &children[b] {
                    let from_child: Vec<usize> = df[c].iter().copied().collect();
                    for w in from_child {
                        if idom[w] != Some(b) {
                            df[b].insert(w);
                        }
                    }
                }
            }
            df
        };

        // Liveness (pruned SSA): only insert Phi for a variable if it's live-in at the join.
        //
        // We compute liveness only for `multi_vars` and we intentionally ignore Phi nodes while
        // building per-block use/def, since Phi uses are edge-associated SSA plumbing (and can
        // form self-sustaining cycles that we don't want to treat as "real" liveness).
        let live_in_multi: Vec<Vec<u64>> = {
            let nm = multi_vars.len();
            let nwords = (nm + 63) / 64;
            let mut use_bits: Vec<Vec<u64>> = vec![vec![0; nwords]; nb];
            let mut def_bits: Vec<Vec<u64>> = vec![vec![0; nwords]; nb];

            fn set_bit(bs: &mut [u64], idx: usize) {
                let w = idx / 64;
                let b = idx % 64;
                bs[w] |= 1u64 << b;
            }
            fn test_bit(bs: &[u64], idx: usize) -> bool {
                let w = idx / 64;
                let b = idx % 64;
                (bs[w] >> b) & 1u64 == 1u64
            }

            for (bi, blk) in func.blocks.iter().enumerate() {
                let mut defined: Vec<u64> = vec![0; nwords];
                for ins in &blk.insns {
                    if matches!(ins.op, IrOp::Phi { .. }) {
                        continue;
                    }
                    for u in ins.op.uses() {
                        if let Some(mi) = multi_index.get(u.0 as usize).and_then(|x| *x) {
                            if !test_bit(&defined, mi) {
                                set_bit(&mut use_bits[bi], mi);
                            }
                        }
                    }
                    if let Some(d) = ins.op.def() {
                        if let Some(mi) = multi_index.get(d.0 as usize).and_then(|x| *x) {
                            set_bit(&mut def_bits[bi], mi);
                            set_bit(&mut defined, mi);
                        }
                    }
                }
                // Terminator uses.
                let mut term_uses: Vec<VRegId> = Vec::new();
                match &blk.term {
                    IrTerminator::Ret { value } => term_uses.push(*value),
                    IrTerminator::JmpIf { cond, .. } => term_uses.push(*cond),
                    IrTerminator::SwitchKind { kind, .. } => term_uses.push(*kind),
                    IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
                }
                for u in term_uses {
                    if let Some(mi) = multi_index.get(u.0 as usize).and_then(|x| *x) {
                        if !test_bit(&defined, mi) {
                            set_bit(&mut use_bits[bi], mi);
                        }
                    }
                }
            }

            let mut live_in: Vec<Vec<u64>> = vec![vec![0; nwords]; nb];
            let mut live_out: Vec<Vec<u64>> = vec![vec![0; nwords]; nb];

            let mut changed_live = true;
            while changed_live {
                changed_live = false;
                for bi in (0..nb).rev() {
                    // out = union of live_in[succ]
                    let mut out: Vec<u64> = vec![0; nwords];
                    for &s in &succs[bi] {
                        for w in 0..nwords {
                            out[w] |= live_in[s][w];
                        }
                    }

                    // in = use ∪ (out - def)
                    let mut inb: Vec<u64> = use_bits[bi].clone();
                    for w in 0..nwords {
                        inb[w] |= out[w] & !def_bits[bi][w];
                    }

                    if inb != live_in[bi] || out != live_out[bi] {
                        live_in[bi] = inb;
                        live_out[bi] = out;
                        changed_live = true;
                    }
                }
            }

            live_in
        };

        // Def blocks per multi var.
        let mut def_blocks: HashMap<VRegId, HashSet<usize>> = HashMap::new();
        for (bi, blk) in func.blocks.iter().enumerate() {
            for ins in &blk.insns {
                if let Some(d) = ins.op.def() {
                    if is_multi.get(d.0 as usize).copied().unwrap_or(false) {
                        def_blocks.entry(d).or_default().insert(bi);
                    }
                }
            }
        }

        // Phi insertion.
        //
        // NOTE: lowering already emits Phi nodes for loops (eg. while variables). Those Phis
        // conceptually define a new SSA name for some underlying variable v, but they don't
        // come with an explicit mapping. If we ignore them, the renamer won't push their `dst`
        // on the variable stack, which can leave stale uses of the pre-SSA vreg after we rename
        // away its defs.
        //
        // We treat any existing Phi where *all incomings are the same vreg `v`* as "the phi
        // for variable v" and reuse it instead of inserting a duplicate phi for v.
        let mut phi_dst_to_var: HashMap<VRegId, VRegId> = HashMap::new();
        for &v in &multi_vars {
            let defs = def_blocks.get(&v).cloned().unwrap_or_default();
            let mut has_phi: Vec<bool> = vec![false; nb];
            let v_mi = multi_index[v.0 as usize].expect("multi var must have index");

            // Seed `has_phi` from existing leading phis that correspond to this var.
            for (bi, blk) in func.blocks.iter().enumerate() {
                let lead = count_leading_phis(&blk.insns);
                for i in 0..lead {
                    let IrOp::Phi { dst, incomings } = &blk.insns[i].op else {
                        continue;
                    };
                    if incomings.is_empty() {
                        continue;
                    }
                    if incomings.iter().all(|(_, vv)| *vv == v) {
                        has_phi[bi] = true;
                        phi_dst_to_var.insert(*dst, v);
                    }
                }
            }

            let mut work: BTreeSet<usize> = defs.iter().copied().collect();
            while let Some(x) = work.pop_first() {
                let mut ys: Vec<usize> = df[x].iter().copied().collect();
                ys.sort_unstable();
                for y in ys {
                    // Pruned SSA: only place phi if v is live-in to y.
                    if preds[y].len() < 2 {
                        continue;
                    }
                    let w = v_mi / 64;
                    let b = v_mi % 64;
                    if ((live_in_multi[y][w] >> b) & 1u64) == 0u64 {
                        continue;
                    }
                    if !has_phi[y] {
                        has_phi[y] = true;
                        let tid = func.vreg_types[v.0 as usize];
                        let phi_dst = new_vreg(&mut func.vreg_types, tid);
                        phi_dst_to_var.insert(phi_dst, v);

                        let mut incomings: Vec<(BlockId, VRegId)> = Vec::new();
                        for &p in &preds[y] {
                            incomings.push((BlockId(p as u32), v));
                        }

                        let idx = count_leading_phis(&func.blocks[y].insns);
                        func.blocks[y].insns.insert(
                            idx,
                            IrInsn {
                                span: crate::ast::Span::point(0),
                                op: IrOp::Phi { dst: phi_dst, incomings },
                            },
                        );
                        changed = true;

                        if !defs.contains(&y) {
                            work.insert(y);
                        }
                    }
                }
            }
        }

        // Rename.
        let mut stacks: Vec<Vec<VRegId>> = vec![Vec::new(); func.vreg_types.len()];
        for &v in &multi_vars {
            stacks[v.0 as usize].push(v);
        }

        fn rename_block(
            bid: usize,
            blocks: &mut [crate::ir::IrBlock],
            vreg_types: &mut Vec<TypeId>,
            succs: &[Vec<usize>],
            children: &[Vec<usize>],
            is_multi: &[bool],
            stacks: &mut Vec<Vec<VRegId>>,
            multi_vars: &[VRegId],
            phi_dst_to_var: &HashMap<VRegId, VRegId>,
        ) {
            // Save stack lengths for restoration.
            let saved: Vec<(usize, usize)> = multi_vars
                .iter()
                .map(|v| (v.0 as usize, stacks[v.0 as usize].len()))
                .collect();

            // Process leading phi defs (only inserted phis affect multi-var stacks).
            let lead = count_leading_phis(&blocks[bid].insns);
            for i in 0..lead {
                if let IrOp::Phi { dst, .. } = blocks[bid].insns[i].op {
                    if let Some(&var) = phi_dst_to_var.get(&dst) {
                        stacks[var.0 as usize].push(dst);
                    }
                }
            }

            // Rename within block (skip leading phis; phi incomings are handled per-edge).
            for ins in blocks[bid].insns.iter_mut().skip(lead) {
                map_op_uses(&mut ins.op, |v| rename_use(v, is_multi, stacks));

                if let Some(d) = ins.op.def() {
                    let di = d.0 as usize;
                    if di < is_multi.len() && is_multi[di] {
                        let tid = vreg_types[di];
                        let newd = new_vreg(vreg_types, tid);
                        set_op_def(&mut ins.op, newd);
                        stacks[di].push(newd);
                    }
                }
            }

            // Rename terminator uses.
            map_term_uses(&mut blocks[bid].term, |v| rename_use(v, is_multi, stacks));

            // Fill phi operands in successors for this predecessor.
            let this_bid = BlockId(bid as u32);
            for &sid in &succs[bid] {
                let succ_block = &mut blocks[sid];
                let lead_phi = count_leading_phis(&succ_block.insns);
                for i in 0..lead_phi {
                    if let IrOp::Phi { incomings, .. } = &mut succ_block.insns[i].op {
                        for (pb, v) in incomings.iter_mut() {
                            if *pb == this_bid {
                                *v = rename_use(*v, is_multi, stacks);
                            }
                        }
                    }
                }
            }

            // Recurse dom children.
            for &c in &children[bid] {
                rename_block(
                    c,
                    blocks,
                    vreg_types,
                    succs,
                    children,
                    is_multi,
                    stacks,
                    multi_vars,
                    phi_dst_to_var,
                );
            }

            // Restore stacks.
            for (vi, len) in saved {
                stacks[vi].truncate(len);
            }
        }

        rename_block(
            entry,
            &mut func.blocks,
            &mut func.vreg_types,
            &succs,
            &children,
            &is_multi,
            &mut stacks,
            &multi_vars,
            &phi_dst_to_var,
        );
    }

    changed
}

