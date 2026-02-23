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

use super::*;

mod index;
mod member;
mod var;

pub(super) fn lower_var_expr(
    e: &Expr,
    name: &str,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    var::lower_var_expr(e, name, expect, ctx, b)
}

pub(super) fn lower_member_expr(
    e: &Expr,
    base: &Expr,
    name: &str,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    member::lower_member_expr(e, base, name, expect, ctx, b)
}

pub(super) fn lower_index_assign_expr(
    e: &Expr,
    base: &Expr,
    index: &Expr,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    index::lower_index_assign_expr(e, base, index, expr0, ctx, b)
}

pub(super) fn lower_index_expr(
    e: &Expr,
    base: &Expr,
    index: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    index::lower_index_expr(e, base, index, ctx, b)
}
