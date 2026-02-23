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

use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::T_DYNAMIC;

use super::super::TypeChecker;

pub(super) fn try_type_method_sugar_call(
    tc: &mut TypeChecker,
    _e: &Expr,
    callee: &Expr,
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<Option<TypeId>, CompileError> {
    let ExprKind::Member { base, name: _ } = &callee.node else {
        return Ok(None);
    };

    // Don't steal module namespace calls (`Mod.f(...)`), which are typed via exports.
    if let ExprKind::Var(alias) = &base.node {
        if tc.is_module_alias(alias.as_str()) {
            return Ok(None);
        }
    }

    let tb = super::expect_object_kind(tc, base)?;
    if !tc.is_object_kind(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "method receiver must be Object",
        ));
    }

    let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
    for a in args {
        arg_tids.push(tc.check_expr(a, None)?);
    }

    // Return type comes from context when available; otherwise Dynamic.
    let ret_tid = expect.unwrap_or(T_DYNAMIC);
    let fun_tid = tc.intern_fun_type(ret_tid, &arg_tids);
    tc.record_expr(callee.span, fun_tid);
    Ok(Some(ret_tid))
}
