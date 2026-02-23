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

use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_DYNAMIC, T_OBJECT};

use super::TypeChecker;

pub(super) fn type_array_lit(
    tc: &mut TypeChecker,
    e: &Expr,
    elems: &[Expr],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    if elems.is_empty() {
        let Some(et) = expect else {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "empty array literal requires a type annotation",
            ));
        };
        if et != T_ARRAY_I32 && et != T_ARRAY_BYTES {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "empty array literal requires Array<I32> or Array<Bytes>",
            ));
        }
        return Ok(et);
    }
    let t0 = tc.check_expr(&elems[0], None)?;
    for el in &elems[1..] {
        let t = tc.check_expr(el, Some(t0))?;
        if t != t0 {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "array literal elements must have same type",
            ));
        }
    }
    match t0 {
        crate::typectx::T_I32 => Ok(T_ARRAY_I32),
        T_BYTES => Ok(T_ARRAY_BYTES),
        _ => Ok(T_DYNAMIC),
    }
}

pub(super) fn type_tuple_lit(tc: &mut TypeChecker, elems: &[Expr]) -> Result<TypeId, CompileError> {
    let mut ts: Vec<TypeId> = Vec::with_capacity(elems.len());
    for el in elems {
        ts.push(tc.check_expr(el, None)?);
    }
    Ok(tc.intern_tuple_type(&ts))
}

pub(super) fn type_obj_lit(
    tc: &mut TypeChecker,
    fields: &[(String, Expr)],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    for (_k, v) in fields {
        let _ = tc.check_expr(v, None)?;
    }
    if let Some(et) = expect {
        if tc.is_object_kind(et) && !tc.is_tuple_type(et) {
            return Ok(et);
        }
    }
    Ok(T_OBJECT)
}
