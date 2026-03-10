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

use crate::ast::{Expr, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::T_DYNAMIC;

use super::TypeChecker;

pub(super) fn type_fn(
    tc: &mut TypeChecker,
    e: &Expr,
    params: &[(String, Option<Ty>)],
    body: &[Stmt],
    tail: &Option<Box<Expr>>,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let fun_tid = if let Some(et) = expect {
        et
    } else {
        let arg_tids = vec![T_DYNAMIC; params.len()];
        tc.intern_fun_type(T_DYNAMIC, &arg_tids)
    };
    let (sig_args, sig_ret) = tc.fun_sig(fun_tid, e.span)?;
    if sig_args.len() != params.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "fn param count does not match expected function type",
        ));
    }

    // Function literals see outer bindings (captures, self-recursion sugar).
    tc.with_scope(|tc| {
        tc.push_ret_tid(sig_ret);
        let res = (|| {
            for (i, (pn, ann)) in params.iter().enumerate() {
                let tid = if let Some(t) = ann {
                    tc.resolve_ann_tid(t)?
                } else {
                    sig_args[i]
                };
                tc.bind_local(pn, tid);
            }
            for st in body {
                tc.check_stmt(st)?;
            }
            if let Some(t) = tail {
                let got = tc.check_expr(t, Some(sig_ret))?;
                let coerced = tc.coerce_type(got, sig_ret, t.span)?;
                tc.record_expr(t.span, coerced);
            }
            Ok(())
        })();
        tc.pop_ret_tid();
        res
    })?;
    Ok(fun_tid)
}
