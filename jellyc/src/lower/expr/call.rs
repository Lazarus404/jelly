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

// Call expression lowering (builtins + general calls).
use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{IrBuilder, TypeId, VRegId};

use super::LowerCtx;

mod direct;
mod marshal;
mod method;
mod module_ns;

pub fn lower_call_expr(
    e: &crate::ast::Expr,
    callee: &crate::ast::Expr,
    type_args: &[crate::ast::Ty],
    args: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let out_tid = ctx
        .sem_expr_types
        .get(&NodeId(e.span))
        .copied()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "missing semantic type for call",
            )
        })?;

    // Builtins are handled in a shared module so inference + lowering stay consistent.
    if let Some(out) = super::builtins::try_lower_builtin_call(e, callee, type_args, args, ctx, b)?
    {
        return Ok(out);
    }

    if !type_args.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "generic type arguments are only supported for builtins (for now)",
        ));
    }

    // General function call.
    // Module namespace call: `Mod.f(args...)` does NOT bind `this`.
    if let Some(out) = module_ns::try_lower_module_namespace_call(e, callee, args, ctx, b)? {
        return Ok(out);
    }

    // Method call sugar: `obj.m(args...)` binds `this=obj` using `BIND_THIS`.
    if let Some(out) = method::try_lower_method_sugar_call(e, callee, args, out_tid, ctx, b)? {
        return Ok(out);
    }

    direct::lower_direct_call_expr(e, callee, args, ctx, b)
}

/// Prepare a tail call: lower callee and args, return (callee, sig_id, arg_base, nargs).
/// Used when lowering `return foo(...)` for non-self tail calls.
pub use direct::prepare_tail_call;
