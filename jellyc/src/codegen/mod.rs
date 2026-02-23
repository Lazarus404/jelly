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

// ## Codegen ABI / register layout (compiler ↔ VM contract)
//
// Jelly bytecode functions execute in a **typed** register frame with **≤256 physical regs**.
// `jellyc`’s codegen is responsible for producing a `reg_types[]` table and an instruction stream
// that satisfies the ISA rules in `docs/ISA.md` and the compiler↔VM layout invariants in
// `docs/call_abi.md`.
//
// ### Fixed / pinned windows
//
// Some vregs are **pinned** to ABI-defined physical register windows and therefore must never be
// moved or spilled:
//
// - **Callee params**: pregs `0..param_count-1` (fixed mapping).
// - **Callee capture slots**: immediately after params; `cap_start` is set accordingly (or `0`
//   when there are no captures).
// - **Call arg windows**: each distinct call signature gets one contiguous physical arg block; at
//   each call site, the IR’s contiguous arg window vregs are pinned into that block.
// - **Closure capture windows** (`CLOSURE a b c imm`): the capture source window `rB..r(B+c-1)`
//   must be contiguous and is treated as a pinned window (so spills cannot corrupt capture args).
//
// Additional invariants:
//
// - **CALLR callee never spills**: the closure/function object in `rB:function` for `CALLR` is
//   excluded from spilling (spilling/reloading can corrupt the callee object).
// - **Spill is boxed-only**: spill ops operate only on `Dynamic` regs; codegen reserves 3 dedicated
//   `Dynamic` spill-reload regs that never overlap ABI windows.
// - **Typed-slot invariant**: for each live vreg, `vreg_to_reg[v]` must point at a preg whose
//   `reg_types[preg]` exactly matches the vreg’s type.
//
// Enforcement of these rules lives primarily in `allocate.rs` (layout + invariants) and
// `finalize.rs` (final mapping + spill insertion).

mod call_abi;
mod cfg;
mod entry;
mod model;
mod allocate;
mod finalize;
mod vstream;

#[cfg(test)]
mod tests;

pub use entry::emit_ir_module;

pub(in crate::codegen) use call_abi::pin_call_arg_blocks_and_validate;
pub(in crate::codegen) use cfg::{blk_pc, block_order_rpo, block_successors, delta, term_size};
pub(in crate::codegen) use model::{reg, Allocation, VirtualStream};
