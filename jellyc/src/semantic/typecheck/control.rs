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

use crate::ast::{Expr, Stmt};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::T_DYNAMIC;

use super::TypeChecker;

pub(super) fn type_if(
    tc: &mut TypeChecker,
    e: &Expr,
    cond: &Expr,
    then_br: &Expr,
    else_br: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let _ = tc.check_expr(cond, None)?;
    let tt = tc.check_expr(then_br, expect)?;
    let te = tc
        .check_expr(else_br, Some(tt))
        .or_else(|_| tc.check_expr(else_br, expect))?;
    if tt != te {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "if branches must have same type",
        ));
    }
    Ok(tt)
}

pub(super) fn type_block(
    tc: &mut TypeChecker,
    stmts: &[Stmt],
    expr: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    tc.with_scope(|tc| {
        for s in stmts {
            tc.check_stmt(s)?;
        }
        tc.check_expr(expr, expect)
    })
}

pub(super) fn type_try(
    tc: &mut TypeChecker,
    e: &Expr,
    body: &Expr,
    catch_name: &Option<String>,
    catch_body: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let tb = tc.check_expr(body, expect)?;
    let tc_t = tc.with_scope(|tcx| {
        if let Some(n) = catch_name {
            tcx.bind_local(n, T_DYNAMIC);
        }
        tcx.check_expr(catch_body, Some(tb))
            .or_else(|_| tcx.check_expr(catch_body, expect))
    })?;
    if tb != tc_t {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "try/catch branches must have same type",
        ));
    }
    Ok(tb)
}
