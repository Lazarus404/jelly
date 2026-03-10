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

use crate::ast::{Expr, ExprKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

use super::{is_numeric, TypeChecker};

mod builtins;
mod method;

pub(super) fn type_call(
    tc: &mut TypeChecker,
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    // Builtin namespaces can be shadowed by imports/lets (e.g. `import m as Bytes`).
    // If shadowed, treat this as a normal call; don't apply builtin typing rules.
    let shadowed_ns = match &callee.node {
        ExprKind::Member { base, .. } => match &base.node {
            ExprKind::Var(ns) => tc.lookup(ns).is_some(),
            _ => false,
        },
        _ => false,
    };

    // Builtins + special-cased builtins (Object.set receiver type behavior).
    if let Some(ret) =
        builtins::try_type_builtin_like_call(tc, e, callee, type_args, args, expect, shadowed_ns)?
    {
        return Ok(ret);
    }

    // Method call sugar and module namespace calls are handled by typing the callee span.
    //
    // Method call sugar: `obj.m(args...)` where `obj` is an Object value.
    // We treat `obj.m` as a bound function value `(A...) -> R` for the purpose of typing the call.
    // Lowering is responsible for emitting `ObjGetAtom` + `BindThis` + `Call`.
    if let Some(ret) = method::try_type_method_sugar_call(tc, e, callee, args, expect)? {
        return Ok(ret);
    }

    // Otherwise, typecheck callee to discover a function type (possibly via module export).
    let callee_tid = tc.check_expr(callee, None)?;
    let (sig_args, sig_ret) = tc.fun_sig(callee_tid, e.span)?;
    if sig_args.len() != args.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "call arity mismatch",
        ));
    }
    for (i, a) in args.iter().enumerate() {
        let _ = tc.check_expr(a, Some(sig_args[i]))?;
    }
    Ok(sig_ret)
}

fn expect_object_kind(tc: &mut TypeChecker, recv: &Expr) -> Result<TypeId, CompileError> {
    let t0 = tc.check_expr(recv, None)?;
    let t = if t0 == T_DYNAMIC {
        tc.check_expr(recv, Some(T_OBJECT))?
    } else {
        t0
    };
    Ok(t)
}

fn is_numeric_tid(t: TypeId) -> bool {
    is_numeric(t)
}
