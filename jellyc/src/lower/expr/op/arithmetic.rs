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
use crate::error::CompileError;
use crate::ir::{IrBuilder, TypeId, VRegId};

use super::super::LowerCtx;

mod add;
mod div;
mod mul;
mod neg;
mod sub;
mod int_div_mod_shift;

pub(super) fn lower_add_expr(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    add::lower_add_expr(e, expect, ctx, b)
}

pub(super) fn lower_sub_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    sub::lower_sub_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_mul_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    mul::lower_mul_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_div_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    div::lower_div_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_mod_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    int_div_mod_shift::lower_mod_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_shl_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    int_div_mod_shift::lower_shl_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_shr_expr(
    e: &Expr,
    a: &Expr,
    bb: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    int_div_mod_shift::lower_shr_expr(e, a, bb, expect, ctx, b)
}

pub(super) fn lower_neg_expr(
    e: &Expr,
    inner: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    neg::lower_neg_expr(e, inner, ctx, b)
}
