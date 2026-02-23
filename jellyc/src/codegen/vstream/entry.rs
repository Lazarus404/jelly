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

use crate::error::CompileError;
use crate::ir::{IrFunction, IrModule};
use crate::regalloc::spill::VInsn;
use crate::regalloc::InstrInfo;

use super::super::{block_order_rpo, VirtualStream};
use super::const_fun_phi;
use super::emit;
use super::layout;
use super::liveness;
use super::term;

pub(super) fn build_virtual_stream(
    ir: &IrModule,
    f: &IrFunction,
    map_func_index: &impl Fn(u32) -> u32,
) -> Result<VirtualStream, CompileError> {
    let order = block_order_rpo(f);
    let block_start = layout::compute_block_start(f, &order);

    let mut vinsns: Vec<VInsn> = Vec::new(); // operands are vreg indices (u32)
    let mut infos: Vec<InstrInfo> = Vec::new();
    // Track call arg windows so we can allocate/pin them contiguously.
    let mut call_windows: Vec<(u32, u32, u8)> = Vec::new(); // (sig_id, arg_base_vreg, nargs)
                                                            // Track which vregs are known `ConstFun(func_index)` so we can emit direct CALL.
    let const_fun_at_exit = const_fun_phi::compute_const_fun_at_exit(f, map_func_index, &order);
    let const_fun_at_entry =
        const_fun_phi::compute_const_fun_at_entry(f, &const_fun_at_exit, &order);

    let mut const_fun_of_vreg: Vec<Option<u32>> = vec![None; f.vreg_types.len()];

    for b in order.iter().copied() {
        let bi = b.0 as usize;
        let blk = &f.blocks[bi];
        for (v, val) in const_fun_at_entry[bi].iter().enumerate() {
            if v < const_fun_of_vreg.len() {
                const_fun_of_vreg[v] = *val;
            }
        }
        for (ii, ins) in blk.insns.iter().enumerate() {
            let this_pc = block_start[bi] + (ii as u32);
            emit::emit_insn(
                ir,
                f,
                map_func_index,
                ins,
                &block_start,
                this_pc,
                &mut vinsns,
                &mut infos,
                &mut call_windows,
                &mut const_fun_of_vreg,
            )?;
        }

        let this_pc = block_start[bi] + (blk.insns.len() as u32);
        term::emit_terminator(
            &block_start,
            &blk.term,
            this_pc,
            &mut vinsns,
            &mut infos,
            &mut call_windows,
            map_func_index,
            &const_fun_of_vreg,
        )?;
    }

    let vs = emit::into_stream(vinsns, infos, call_windows);
    Ok(liveness::finish(f, &block_start, vs))
}
