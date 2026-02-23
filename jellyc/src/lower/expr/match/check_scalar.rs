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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{lookup_var, LowerCtx};
use crate::typectx::{T_BOOL, T_DYNAMIC, T_I16, T_I32, T_I8};

pub(super) fn lower_check_wildcard(b: &mut IrBuilder, bind_b: BlockId) {
    b.term(IrTerminator::Jmp { target: bind_b });
}

pub(super) fn lower_check_bind(b: &mut IrBuilder, bind_b: BlockId) {
    // Always matches; binding is established in the bind block.
    b.term(IrTerminator::Jmp { target: bind_b });
}

pub(super) fn lower_check_bool_lit(
    span: Span,
    imm: bool,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    if t_subj != T_BOOL {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "match bool pattern requires bool subject",
        ));
    }
    let v_pat = b.new_vreg(T_BOOL);
    b.emit(span, IrOp::ConstBool { dst: v_pat, imm });
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(
        span,
        IrOp::Physeq {
            dst: v_cond,
            a: v_subj,
            b: v_pat,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}

pub(super) fn lower_check_i32_lit(
    span: Span,
    imm: i32,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    if t_subj != T_I32 {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "match i32 pattern requires i32 subject",
        ));
    }
    let v_pat = b.new_vreg(T_I32);
    b.emit(span, IrOp::ConstI32 { dst: v_pat, imm });
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(
        span,
        IrOp::EqI32 {
            dst: v_cond,
            a: v_subj,
            b: v_pat,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}

pub(super) fn lower_check_i8_lit(
    span: Span,
    imm: i32,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    if t_subj != T_I8 {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "match i8 pattern requires i8 subject",
        ));
    }
    let v_pat = b.new_vreg(T_I8);
    let imm = (imm as i16).to_le_bytes()[0];
    b.emit(span, IrOp::ConstI8Imm { dst: v_pat, imm });
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(
        span,
        IrOp::EqI32 {
            dst: v_cond,
            a: v_subj,
            b: v_pat,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}

pub(super) fn lower_check_i16_lit(
    span: Span,
    imm: i32,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    if t_subj != T_I16 {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "match i16 pattern requires i16 subject",
        ));
    }
    let v_pat = b.new_vreg(T_I16);
    b.emit(span, IrOp::ConstI32 { dst: v_pat, imm });
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(
        span,
        IrOp::EqI32 {
            dst: v_cond,
            a: v_subj,
            b: v_pat,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}

pub(super) fn lower_check_pin(
    span: Span,
    name: &str,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    // Match subject against an existing name.
    let bd = lookup_var(ctx, name, span)?;
    if bd.tid == T_DYNAMIC {
        let v_subj_dyn = b.new_vreg(T_DYNAMIC);
        b.emit(
            span,
            IrOp::ToDyn {
                dst: v_subj_dyn,
                src: v_subj,
            },
        );
        let v_cond = b.new_vreg(T_BOOL);
        b.emit(
            span,
            IrOp::Physeq {
                dst: v_cond,
                a: v_subj_dyn,
                b: bd.v,
            },
        );
        b.term(IrTerminator::JmpIf {
            cond: v_cond,
            then_tgt: bind_b,
            else_tgt: next_check,
        });
        Ok(())
    } else {
        if bd.tid != t_subj {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "pinned name has incompatible type",
            ));
        }
        let v_cond = b.new_vreg(T_BOOL);
        match t_subj {
            T_I32 => b.emit(
                span,
                IrOp::EqI32 {
                    dst: v_cond,
                    a: v_subj,
                    b: bd.v,
                },
            ),
            _ => b.emit(
                span,
                IrOp::Physeq {
                    dst: v_cond,
                    a: v_subj,
                    b: bd.v,
                },
            ),
        }
        b.term(IrTerminator::JmpIf {
            cond: v_cond,
            then_tgt: bind_b,
            else_tgt: next_check,
        });
        Ok(())
    }
}
