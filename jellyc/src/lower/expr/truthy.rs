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
use crate::error::CompileError;
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use super::super::{LowerCtx, T_BOOL, T_DYNAMIC, T_I32};

pub(super) fn lower_truthy(
    span: Span,
    v: VRegId,
    tid: TypeId,
    _ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    match tid {
        T_BOOL => Ok(v),
        T_DYNAMIC => {
            // Truthiness for Dynamic:
            // - null => false
            // - bool => itself
            // - everything else => true
            let v_kind = b.new_vreg(T_I32);
            b.emit(
                span,
                IrOp::Kindof {
                    dst: v_kind,
                    src: v,
                },
            );

            let v0 = b.new_vreg(T_I32);
            b.emit(span, IrOp::ConstI32 { dst: v0, imm: 0 });
            let v_is_null = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::EqI32 {
                    dst: v_is_null,
                    a: v_kind,
                    b: v0,
                },
            );

            let b_null = b.new_block(Some("truthy_null".to_string()));
            let b_not_null = b.new_block(Some("truthy_not_null".to_string()));
            let b_join = b.new_block(Some("truthy_join".to_string()));
            b.term(IrTerminator::JmpIf {
                cond: v_is_null,
                then_tgt: b_null,
                else_tgt: b_not_null,
            });

            // null -> false
            b.set_block(b_null);
            let v_false = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::ConstBool {
                    dst: v_false,
                    imm: false,
                },
            );
            b.term(IrTerminator::Jmp { target: b_join });

            // not-null: if kind==bool then unbox, else true
            b.set_block(b_not_null);
            let v1 = b.new_vreg(T_I32);
            b.emit(span, IrOp::ConstI32 { dst: v1, imm: 1 });
            let v_is_bool = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::EqI32 {
                    dst: v_is_bool,
                    a: v_kind,
                    b: v1,
                },
            );

            let b_dyn_bool = b.new_block(Some("truthy_dyn_bool".to_string()));
            let b_other = b.new_block(Some("truthy_other".to_string()));
            b.term(IrTerminator::JmpIf {
                cond: v_is_bool,
                then_tgt: b_dyn_bool,
                else_tgt: b_other,
            });

            b.set_block(b_other);
            let v_true = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::ConstBool {
                    dst: v_true,
                    imm: true,
                },
            );
            b.term(IrTerminator::Jmp { target: b_join });

            b.set_block(b_dyn_bool);
            let v_unboxed = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::FromDynBool {
                    dst: v_unboxed,
                    src: v,
                },
            );
            b.term(IrTerminator::Jmp { target: b_join });

            b.set_block(b_join);
            let v_out = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::Phi {
                    dst: v_out,
                    incomings: vec![
                        (b_null, v_false),
                        (b_other, v_true),
                        (b_dyn_bool, v_unboxed),
                    ],
                },
            );
            Ok(v_out)
        }
        _ => {
            // Non-bool, non-null types are always truthy.
            let out = b.new_vreg(T_BOOL);
            b.emit(
                span,
                IrOp::ConstBool {
                    dst: out,
                    imm: true,
                },
            );
            Ok(out)
        }
    }
}
