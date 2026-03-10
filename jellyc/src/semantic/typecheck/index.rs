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

use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_I32};

use super::TypeChecker;

pub(super) fn type_index(
    tc: &mut TypeChecker,
    e: &Expr,
    base: &Expr,
    index: &Expr,
) -> Result<TypeId, CompileError> {
    let tb = tc.check_expr(base, None)?;
    let ti = tc.check_expr(index, Some(T_I32))?;
    let ti = tc.coerce_type(ti, T_I32, index.span)?;
    // Record the coerced type so lowering doesn't need contextual guessing.
    tc.record_expr(index.span, ti);
    match tb {
        T_ARRAY_I32 => Ok(T_I32),
        T_ARRAY_BYTES => Ok(T_BYTES),
        T_BYTES => Ok(T_I32),
        _ => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "indexing not supported for this type yet",
        )),
    }
}

pub(super) fn type_index_assign(
    tc: &mut TypeChecker,
    e: &Expr,
    base: &Expr,
    index: &Expr,
    expr: &Expr,
) -> Result<TypeId, CompileError> {
    let tb = tc.check_expr(base, None)?;
    let ti = tc.check_expr(index, Some(T_I32))?;
    let ti = tc.coerce_type(ti, T_I32, index.span)?;
    tc.record_expr(index.span, ti);
    let elem_tid = match tb {
        T_ARRAY_I32 => T_I32,
        T_ARRAY_BYTES => T_BYTES,
        T_BYTES => T_I32,
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "index assignment not supported for this type yet",
            ))
        }
    };
    let _ = tc.check_expr(expr, Some(elem_tid))?;
    Ok(elem_tid)
}
