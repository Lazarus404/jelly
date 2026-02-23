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
mod cfg;
mod convert;
mod dom;
mod liveness;
mod phi;
mod pin;
mod rename;
mod rewrite;
mod util;

pub use convert::convert_to_ssa;
