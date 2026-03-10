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

// Integer ops (%, <<, >>): accept numeric operands. Floats are coerced to int (checked).
use crate::ast::{Expr, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::super::{coerce_numeric, is_numeric, lower_expr, LowerCtx};
use super::super::super::{T_I16, T_I32, T_I64, T_I8};

fn is_integer(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64)
}

fn integer_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        _ => 255,
    }
}

fn join_integer(a: TypeId, b: TypeId) -> TypeId {
    if integer_rank(a) >= integer_rank(b) {
        a
    } else {
        b
    }
}

/// Result type for integer ops: i64 if any operand is float, else join_integer.
fn out_type(ta: TypeId, tb: TypeId) -> TypeId {
    if !is_integer(ta) || !is_integer(tb) {
        T_I64
    } else {
        join_integer(ta, tb)
    }
}

fn lower_int_bin(
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    bld: &mut IrBuilder,
    emit_op: impl FnOnce(Span, VRegId, VRegId, VRegId, TypeId, &mut IrBuilder) -> Result<(), CompileError>,
) -> Result<(VRegId, TypeId), CompileError> {
    let (va, ta) = lower_expr(a, ctx, bld)?;
    let (vb, tb) = lower_expr(b, ctx, bld)?;
    if !is_numeric(ta) || !is_numeric(tb) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "integer operator expects numeric operands",
        ));
    }
    let out_t = expect.unwrap_or_else(|| out_type(ta, tb));
    if !is_integer(out_t) {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "integer operator result must be integer type",
        ));
    }
    let va2 = coerce_numeric(e.span, va, ta, out_t, bld)?;
    let vb2 = coerce_numeric(e.span, vb, tb, out_t, bld)?;
    let out = bld.new_vreg(out_t);
    emit_op(e.span, out, va2, vb2, out_t, bld)?;
    Ok((out, out_t))
}

fn emit_mod(span: Span, dst: VRegId, a: VRegId, b: VRegId, t: TypeId, bld: &mut IrBuilder) -> Result<(), CompileError> {
    match t {
        T_I8 | T_I16 | T_I32 => {
            bld.emit(span, IrOp::ModI32S { dst, a, b });
            Ok(())
        }
        T_I64 => {
            bld.emit(span, IrOp::ModI64S { dst, a, b });
            Ok(())
        }
        _ => Err(CompileError::new(ErrorKind::Internal, span, "bad mod type")),
    }
}

fn emit_shl(span: Span, dst: VRegId, a: VRegId, b: VRegId, t: TypeId, bld: &mut IrBuilder) -> Result<(), CompileError> {
    match t {
        T_I8 | T_I16 | T_I32 => {
            bld.emit(span, IrOp::ShlI32 { dst, a, b });
            Ok(())
        }
        T_I64 => {
            bld.emit(span, IrOp::ShlI64 { dst, a, b });
            Ok(())
        }
        _ => Err(CompileError::new(ErrorKind::Internal, span, "bad shl type")),
    }
}

fn emit_shr(span: Span, dst: VRegId, a: VRegId, b: VRegId, t: TypeId, bld: &mut IrBuilder) -> Result<(), CompileError> {
    match t {
        T_I8 | T_I16 | T_I32 => {
            bld.emit(span, IrOp::ShrI32S { dst, a, b });
            Ok(())
        }
        T_I64 => {
            bld.emit(span, IrOp::ShrI64S { dst, a, b });
            Ok(())
        }
        _ => Err(CompileError::new(ErrorKind::Internal, span, "bad shr type")),
    }
}

pub(super) fn lower_mod_expr(
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    bld: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    lower_int_bin(e, a, b, expect, ctx, bld, emit_mod)
}

pub(super) fn lower_shl_expr(
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    bld: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    lower_int_bin(e, a, b, expect, ctx, bld, emit_shl)
}

pub(super) fn lower_shr_expr(
    e: &Expr,
    a: &Expr,
    b: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    bld: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    lower_int_bin(e, a, b, expect, ctx, bld, emit_shr)
}
