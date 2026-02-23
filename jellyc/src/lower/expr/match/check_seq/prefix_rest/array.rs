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

use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::super::super::{T_BOOL, T_I32};
use super::super::super::util::{chain_check, lower_pin_as};

pub(super) fn lower_check_array_prefix_rest_array(
    i: usize,
    pat_span: Span,
    prefix: &[Pattern],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    _t_subj: TypeId,
    elem_tid: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    // --- Array prefix/rest: len >= prefix.len()
    let v_len = b.new_vreg(T_I32);
    b.emit(
        pat_span,
        IrOp::ArrayLen {
            dst: v_len,
            arr: v_subj,
        },
    );
    let v_n = b.new_vreg(T_I32);
    b.emit(
        pat_span,
        IrOp::ConstI32 {
            dst: v_n,
            imm: prefix.len() as i32,
        },
    );
    let v_too_short = b.new_vreg(T_BOOL);
    b.emit(
        pat_span,
        IrOp::LtI32 {
            dst: v_too_short,
            a: v_len,
            b: v_n,
        },
    );
    let v_ok = b.new_vreg(T_BOOL);
    b.emit(
        pat_span,
        IrOp::NotBool {
            dst: v_ok,
            src: v_too_short,
        },
    );
    let _ = chain_check(
        b,
        next_check,
        v_ok,
        format!("match_check{}_pass{}", i, b.func.blocks.len()),
    );

    for (idx, p) in prefix.iter().enumerate() {
        match &p.node {
            crate::ast::PatternKind::I32Lit(imm) => {
                if elem_tid != T_I32 {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        p.span,
                        "i32 element pattern requires Array<i32>",
                    ));
                }
                let v_i = b.new_vreg(T_I32);
                b.emit(
                    p.span,
                    IrOp::ConstI32 {
                        dst: v_i,
                        imm: idx as i32,
                    },
                );
                let v_el = b.new_vreg(T_I32);
                b.emit(
                    p.span,
                    IrOp::ArrayGet {
                        dst: v_el,
                        arr: v_subj,
                        index: v_i,
                    },
                );
                let v_pat = b.new_vreg(T_I32);
                b.emit(
                    p.span,
                    IrOp::ConstI32 {
                        dst: v_pat,
                        imm: *imm,
                    },
                );
                let v_eq = b.new_vreg(T_BOOL);
                b.emit(
                    p.span,
                    IrOp::EqI32 {
                        dst: v_eq,
                        a: v_el,
                        b: v_pat,
                    },
                );
                let _ = chain_check(
                    b,
                    next_check,
                    v_eq,
                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                );
            }
            crate::ast::PatternKind::Pin(name) => {
                let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                let v_i = b.new_vreg(T_I32);
                b.emit(
                    p.span,
                    IrOp::ConstI32 {
                        dst: v_i,
                        imm: idx as i32,
                    },
                );
                let v_el = b.new_vreg(elem_tid);
                b.emit(
                    p.span,
                    IrOp::ArrayGet {
                        dst: v_el,
                        arr: v_subj,
                        index: v_i,
                    },
                );
                let v_eq = b.new_vreg(T_BOOL);
                match t_pin {
                    T_I32 => b.emit(
                        p.span,
                        IrOp::EqI32 {
                            dst: v_eq,
                            a: v_el,
                            b: v_pin,
                        },
                    ),
                    _ => b.emit(
                        p.span,
                        IrOp::Physeq {
                            dst: v_eq,
                            a: v_el,
                            b: v_pin,
                        },
                    ),
                }
                let _ = chain_check(
                    b,
                    next_check,
                    v_eq,
                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                );
            }
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
            _ => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    p.span,
                    "nested patterns in arrays not supported yet",
                ))
            }
        }
    }
    b.term(IrTerminator::Jmp { target: bind_b });
    Ok(())
}
