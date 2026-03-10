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

use crate::ast::Span;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

/// Marshal args into a contiguous window. When `tail_call` is true and args are params
/// in order, returns (VRegId(0), nargs) to use the param range directly (avoids Mov +
/// arg-block pinning that can corrupt closure tail calls).
pub(super) fn marshal_args_window(
    span: Span,
    arg_tids: &[TypeId],
    arg_vals: &[VRegId],
    b: &mut IrBuilder,
    tail_call: bool,
) -> (VRegId, u8) {
    debug_assert_eq!(
        arg_tids.len(),
        arg_vals.len(),
        "argument types/values must match"
    );

    let nargs = arg_vals.len() as u8;
    if nargs == 0 {
        return (VRegId(0), 0);
    }

    if tail_call
        && arg_vals
            .iter()
            .enumerate()
            .all(|(i, &v)| v.0 == i as u32)
    {
        return (VRegId(0), nargs);
    }

    let base = b.new_vreg(arg_tids[0]);
    b.emit(
        span,
        IrOp::Mov {
            dst: base,
            src: arg_vals[0],
        },
    );

    let mut prev = base;
    for i in 1..(nargs as usize) {
        let v = b.new_vreg(arg_tids[i]);
        debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
        b.emit(
            span,
            IrOp::Mov {
                dst: v,
                src: arg_vals[i],
            },
        );
        prev = v;
    }

    (base, nargs)
}
