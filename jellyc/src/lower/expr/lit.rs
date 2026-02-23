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

// Literal expression lowering (array, tuple).

use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::{intern_atom, lower_expr, LowerCtx};
use super::{T_ARRAY_BYTES, T_ARRAY_I32, T_I32, T_BYTES};

pub fn lower_array_lit(
    e: &crate::ast::Expr,
    elems: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    if elems.is_empty() {
        let et = ctx
            .sem_expr_types
            .get(&NodeId(e.span))
            .copied()
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing semantic type for array literal"))?;
        if et != T_ARRAY_I32 && et != T_ARRAY_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "array literal has non-array semantic type",
            ));
        }
        let v0 = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ConstI32 { dst: v0, imm: 0 });
        let out = b.new_vreg(et);
        b.emit(e.span, IrOp::ArrayNew { dst: out, len: v0 });
        return Ok((out, et));
    }

    let mut vs: Vec<(VRegId, TypeId)> = Vec::with_capacity(elems.len());
    for el in elems {
        vs.push(lower_expr(el, ctx, b)?);
    }
    let t0 = vs[0].1;
    for &(_, tt) in &vs[1..] {
        if tt != t0 {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "array literal elements must have same type",
            ));
        }
    }
    let arr_tid = match t0 {
        T_I32 => T_ARRAY_I32,
        T_BYTES => T_ARRAY_BYTES,
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "array literal only supports i32/bytes elements for now",
            ))
        }
    };

    let v_n = b.new_vreg(T_I32);
    b.emit(e.span, IrOp::ConstI32 { dst: v_n, imm: vs.len() as i32 });
    let v_arr = b.new_vreg(arr_tid);
    b.emit(e.span, IrOp::ArrayNew { dst: v_arr, len: v_n });
    for (i, (vp, _)) in vs.iter().enumerate() {
        let v_i = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ConstI32 { dst: v_i, imm: i as i32 });
        b.emit(e.span, IrOp::ArraySet { arr: v_arr, index: v_i, value: *vp });
    }
    Ok((v_arr, arr_tid))
}

pub fn lower_tuple_lit(
    e: &crate::ast::Expr,
    elems: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let mut vs: Vec<(VRegId, TypeId)> = Vec::with_capacity(elems.len());
    let mut elem_tids: Vec<TypeId> = Vec::with_capacity(elems.len());
    for el in elems {
        let (v, t) = lower_expr(el, ctx, b)?;
        vs.push((v, t));
        elem_tids.push(t);
    }

    let tup_tid = ctx.type_ctx.intern_tuple_type(&elem_tids);
    let out = b.new_vreg(tup_tid);
    b.emit(e.span, IrOp::ObjNew { dst: out });

    for (i, (v, _t)) in vs.iter().enumerate() {
        let key = i.to_string();
        let atom_id = intern_atom(&key, ctx);
        b.emit(
            e.span,
            IrOp::ObjSetAtom {
                obj: out,
                atom_id,
                value: *v,
            },
        );
    }
    Ok((out, tup_tid))
}
