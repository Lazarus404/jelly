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

use crate::ir::{IrFunction, IrOp, IrTerminator, VRegId};

pub(super) fn compute_live_in_multi(
    func: &IrFunction,
    succs: &[Vec<usize>],
    multi_vars: &[VRegId],
    multi_index: &[Option<usize>],
) -> Vec<Vec<u64>> {
    let nb = func.blocks.len();
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
            IrTerminator::TailCall {
                callee,
                arg_base,
                nargs,
                ..
            } => {
                term_uses.push(*callee);
                for i in 0..(*nargs as u32) {
                    term_uses.push(crate::ir::VRegId(arg_base.0 + i));
                }
            }
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
}
