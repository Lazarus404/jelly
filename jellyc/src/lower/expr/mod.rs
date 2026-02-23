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
mod access;
mod builtins;
pub mod call;
mod control;
mod dispatch;
mod fn_;
mod lit;
mod r#match;
mod new_;
mod numeric;
mod op;
mod truthy;
mod tuple_eq;

use crate::ast::{Expr, ExprKind, Span};
use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{IrBuilder, TypeId, VRegId};

use super::{
    ensure_capture_binding, intern_atom, is_object_kind, lookup_var, LowerCtx, T_ARRAY_BYTES,
    T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64,
    T_I8, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

pub(crate) use numeric::{coerce_numeric, f32_to_f16_bits, is_narrowing_numeric};

pub(super) fn is_numeric(t: TypeId) -> bool {
    numeric::is_numeric(t)
}

pub(super) fn type_name(tid: TypeId) -> &'static str {
    numeric::type_name(tid)
}

fn elem_tid_for_array(arr_tid: TypeId) -> Option<TypeId> {
    match arr_tid {
        T_ARRAY_I32 => Some(T_I32),
        T_ARRAY_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn elem_tid_for_list(list_tid: TypeId) -> Option<TypeId> {
    match list_tid {
        T_LIST_I32 => Some(T_I32),
        T_LIST_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn emit_bytes_eq(
    span: Span,
    dst: VRegId,
    a: VRegId,
    b2: VRegId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    tuple_eq::emit_bytes_eq(span, dst, a, b2, ctx, b)
}

fn lower_tuple_eq(
    span: Span,
    a: VRegId,
    b2: VRegId,
    tup_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    tuple_eq::lower_tuple_eq(span, a, b2, tup_tid, ctx, b)
}

// Lower the truthiness of a value.
pub(crate) fn lower_truthy(
    span: Span,
    v: VRegId,
    tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    truthy::lower_truthy(span, v, tid, ctx, b)
}

pub fn lower_expr(
    e: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    lower_expr_expect(e, ctx, b)
}

pub(super) fn lower_expr_expect(
    e: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Semantic analysis is the source of truth for expression types.
    // Lowering should never guess a type from a contextual `expect`.
    let sem_t = ctx
        .sem_expr_types
        .get(&NodeId(e.span))
        .copied()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "missing semantic type for expression",
            )
        })?;
    let res = dispatch::lower_expr_expect_impl(e, Some(sem_t), ctx, b)?;
    // If this fails, lowering made a type-directed decision that disagrees with semantic analysis.
    if res.1 != sem_t {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            format!(
                "lowering produced type {:?}, but semantic analysis says {:?}",
                res.1, sem_t
            ),
        ));
    }
    Ok(res)
}
