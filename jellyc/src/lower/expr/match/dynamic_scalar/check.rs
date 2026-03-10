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
use crate::error::CompileError;
use crate::ir::{IrBuilder, IrOp, IrTerminator, VRegId};
use crate::lower::{lookup_var, LowerCtx};

use crate::lower::{T_BOOL, T_DYNAMIC, T_I16, T_I32, T_I8};

fn emit_pin_check_dynamic_subject(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    subj_dyn: VRegId,
    span: Span,
    name: &str,
    next_check: crate::ir::BlockId,
    bind_b: crate::ir::BlockId,
) -> Result<(), CompileError> {
    let bd = lookup_var(ctx, name, span)?;
    let v_rhs_dyn = if bd.tid == T_DYNAMIC {
        bd.v
    } else {
        let out = b.new_vreg(T_DYNAMIC);
        b.emit(
            span,
            IrOp::ToDyn {
                dst: out,
                src: bd.v,
            },
        );
        out
    };
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(
        span,
        IrOp::Physeq {
            dst: v_cond,
            a: subj_dyn,
            b: v_rhs_dyn,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}

pub(super) fn emit_check_bool_arm(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    arm: &crate::ast::MatchArm,
    bind_b: crate::ir::BlockId,
    next_bool: crate::ir::BlockId,
) -> Result<(), CompileError> {
    match &arm.pat.node {
        crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
            b.term(IrTerminator::Jmp { target: bind_b });
        }
        crate::ast::PatternKind::Pin(name) => {
            emit_pin_check_dynamic_subject(
                ctx,
                b,
                v_subj,
                arm.pat.span,
                name.as_str(),
                next_bool,
                bind_b,
            )?;
        }
        crate::ast::PatternKind::BoolLit(p) => {
            let v_subj_b = b.new_vreg(T_BOOL);
            b.emit(
                arm.pat.span,
                IrOp::FromDynBool {
                    dst: v_subj_b,
                    src: v_subj,
                },
            );
            let v_pat = b.new_vreg(T_BOOL);
            b.emit(
                arm.pat.span,
                IrOp::ConstBool {
                    dst: v_pat,
                    imm: *p,
                },
            );
            let v_cond = b.new_vreg(T_BOOL);
            b.emit(
                arm.pat.span,
                IrOp::Physeq {
                    dst: v_cond,
                    a: v_subj_b,
                    b: v_pat,
                },
            );
            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: bind_b,
                else_tgt: next_bool,
            });
        }
        _ => b.term(IrTerminator::Jmp { target: next_bool }),
    }
    Ok(())
}

pub(super) fn emit_check_i32_arm(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    arm: &crate::ast::MatchArm,
    bind_b: crate::ir::BlockId,
    next_i32: crate::ir::BlockId,
) -> Result<(), CompileError> {
    match &arm.pat.node {
        crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
            b.term(IrTerminator::Jmp { target: bind_b });
        }
        crate::ast::PatternKind::Pin(name) => {
            emit_pin_check_dynamic_subject(
                ctx,
                b,
                v_subj,
                arm.pat.span,
                name.as_str(),
                next_i32,
                bind_b,
            )?;
        }
        crate::ast::PatternKind::I32Lit(p) => {
            let v_subj_i = b.new_vreg(T_I32);
            b.emit(
                arm.pat.span,
                IrOp::FromDynI32 {
                    dst: v_subj_i,
                    src: v_subj,
                },
            );
            let v_pat = b.new_vreg(T_I32);
            b.emit(
                arm.pat.span,
                IrOp::ConstI32 {
                    dst: v_pat,
                    imm: *p,
                },
            );
            let v_cond = b.new_vreg(T_BOOL);
            b.emit(
                arm.pat.span,
                IrOp::EqI32 {
                    dst: v_cond,
                    a: v_subj_i,
                    b: v_pat,
                },
            );
            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: bind_b,
                else_tgt: next_i32,
            });
        }
        crate::ast::PatternKind::I8Lit(p) => {
            let v_subj_i = b.new_vreg(T_I8);
            b.emit(
                arm.pat.span,
                IrOp::FromDynI8 {
                    dst: v_subj_i,
                    src: v_subj,
                },
            );
            let v_pat = b.new_vreg(T_I8);
            let imm = (*p as i16).to_le_bytes()[0];
            b.emit(arm.pat.span, IrOp::ConstI8Imm { dst: v_pat, imm });
            let v_cond = b.new_vreg(T_BOOL);
            b.emit(
                arm.pat.span,
                IrOp::EqI32 {
                    dst: v_cond,
                    a: v_subj_i,
                    b: v_pat,
                },
            );
            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: bind_b,
                else_tgt: next_i32,
            });
        }
        crate::ast::PatternKind::I16Lit(p) => {
            let v_subj_i = b.new_vreg(T_I16);
            b.emit(
                arm.pat.span,
                IrOp::FromDynI16 {
                    dst: v_subj_i,
                    src: v_subj,
                },
            );
            let v_pat = b.new_vreg(T_I16);
            b.emit(
                arm.pat.span,
                IrOp::ConstI32 {
                    dst: v_pat,
                    imm: *p,
                },
            );
            let v_cond = b.new_vreg(T_BOOL);
            b.emit(
                arm.pat.span,
                IrOp::EqI32 {
                    dst: v_cond,
                    a: v_subj_i,
                    b: v_pat,
                },
            );
            b.term(IrTerminator::JmpIf {
                cond: v_cond,
                then_tgt: bind_b,
                else_tgt: next_i32,
            });
        }
        _ => b.term(IrTerminator::Jmp { target: next_i32 }),
    }
    Ok(())
}

pub(super) fn emit_check_other_arm(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    arm: &crate::ast::MatchArm,
    bind_b: crate::ir::BlockId,
    next_other: crate::ir::BlockId,
) -> Result<(), CompileError> {
    match &arm.pat.node {
        crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
            b.term(IrTerminator::Jmp { target: bind_b });
        }
        crate::ast::PatternKind::Pin(name) => {
            emit_pin_check_dynamic_subject(
                ctx,
                b,
                v_subj,
                arm.pat.span,
                name.as_str(),
                next_other,
                bind_b,
            )?;
        }
        _ => b.term(IrTerminator::Jmp { target: next_other }),
    }
    Ok(())
}
