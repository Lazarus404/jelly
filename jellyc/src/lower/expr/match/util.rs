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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{lookup_var, LowerCtx};

use super::super::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_I16, T_I32, T_I8,
    T_OBJECT,
};

pub(super) fn elem_tid_for_array(arr_tid: TypeId) -> Option<TypeId> {
    super::super::elem_tid_for_array(arr_tid)
}

pub(super) fn elem_tid_for_list(list_tid: TypeId) -> Option<TypeId> {
    super::super::elem_tid_for_list(list_tid)
}

pub(super) fn lower_pin_as(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    name: &str,
    expect_tid: TypeId,
    span: Span,
) -> Result<(VRegId, TypeId), CompileError> {
    let bd = lookup_var(ctx, name, span)?;
    if bd.tid == expect_tid {
        return Ok((bd.v, bd.tid));
    }
    if bd.tid != T_DYNAMIC {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "pinned name has incompatible type",
        ));
    }
    let out = b.new_vreg(expect_tid);
    match expect_tid {
        T_I8 => b.emit(
            span,
            IrOp::FromDynI8 {
                dst: out,
                src: bd.v,
            },
        ),
        T_I16 => b.emit(
            span,
            IrOp::FromDynI16 {
                dst: out,
                src: bd.v,
            },
        ),
        T_I32 => b.emit(
            span,
            IrOp::FromDynI32 {
                dst: out,
                src: bd.v,
            },
        ),
        T_F16 => b.emit(
            span,
            IrOp::FromDynF16 {
                dst: out,
                src: bd.v,
            },
        ),
        T_F32 => b.emit(
            span,
            IrOp::FromDynF32 {
                dst: out,
                src: bd.v,
            },
        ),
        T_BOOL => b.emit(
            span,
            IrOp::FromDynBool {
                dst: out,
                src: bd.v,
            },
        ),
        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(
            span,
            IrOp::FromDynPtr {
                dst: out,
                src: bd.v,
            },
        ),
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "unsupported pinned conversion",
            ))
        }
    }
    Ok((out, expect_tid))
}

pub(super) fn chain_check(
    b: &mut IrBuilder,
    next_check: BlockId,
    cond_v: VRegId,
    label: String,
) -> BlockId {
    let pass_b = b.new_block(Some(label));
    b.term(IrTerminator::JmpIf {
        cond: cond_v,
        then_tgt: pass_b,
        else_tgt: next_check,
    });
    b.set_block(pass_b);
    pass_b
}

pub(super) fn emit_const_i32(b: &mut IrBuilder, span: Span, imm: i32) -> VRegId {
    let v = b.new_vreg(T_I32);
    b.emit(span, IrOp::ConstI32 { dst: v, imm });
    v
}

pub(super) fn emit_array_get_at(
    b: &mut IrBuilder,
    span: Span,
    arr: VRegId,
    idx: usize,
    elem_tid: TypeId,
) -> VRegId {
    let v_i = emit_const_i32(b, span, idx as i32);
    let v_el = b.new_vreg(elem_tid);
    b.emit(
        span,
        IrOp::ArrayGet {
            dst: v_el,
            arr,
            index: v_i,
        },
    );
    v_el
}

pub(super) fn emit_list_drop(
    b: &mut IrBuilder,
    span: Span,
    list_tid: TypeId,
    list: VRegId,
    n: usize,
) -> VRegId {
    let mut v_cur = list;
    for _ in 0..n {
        let v_next = b.new_vreg(list_tid);
        b.emit(
            span,
            IrOp::ListTail {
                dst: v_next,
                list: v_cur,
            },
        );
        v_cur = v_next;
    }
    v_cur
}

pub(super) fn emit_list_nth_head(
    b: &mut IrBuilder,
    span: Span,
    list_tid: TypeId,
    list: VRegId,
    idx: usize,
    elem_tid: TypeId,
) -> VRegId {
    let v_cur = emit_list_drop(b, span, list_tid, list, idx);
    let v_el = b.new_vreg(elem_tid);
    b.emit(
        span,
        IrOp::ListHead {
            dst: v_el,
            list: v_cur,
        },
    );
    v_el
}
