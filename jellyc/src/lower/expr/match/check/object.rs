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
use crate::lower::{intern_atom, lookup_var, LowerCtx};

use super::super::super::{T_BOOL, T_DYNAMIC, T_I32, T_OBJECT};
use super::super::util::chain_check;

pub(super) fn lower_check_obj(
    i: usize,
    pat_span: Span,
    fields: &[(String, Pattern)],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    if t_subj != T_OBJECT {
        return Err(CompileError::new(
            ErrorKind::Type,
            pat_span,
            "object pattern requires Object subject",
        ));
    }
    for (k, vp) in fields {
        let atom_id = intern_atom(k.as_str(), ctx);
        let v_has = b.new_vreg(T_BOOL);
        b.emit(
            pat_span,
            IrOp::ObjHasAtom {
                dst: v_has,
                obj: v_subj,
                atom_id,
            },
        );
        let _ = chain_check(
            b,
            next_check,
            v_has,
            format!("match_check{}_pass{}", i, b.func.blocks.len()),
        );

        match &vp.node {
            crate::ast::PatternKind::I32Lit(imm) => {
                let v_f = b.new_vreg(T_I32);
                b.emit(
                    vp.span,
                    IrOp::ObjGetAtom {
                        dst: v_f,
                        obj: v_subj,
                        atom_id,
                    },
                );
                let v_pat = b.new_vreg(T_I32);
                b.emit(
                    vp.span,
                    IrOp::ConstI32 {
                        dst: v_pat,
                        imm: *imm,
                    },
                );
                let v_eq = b.new_vreg(T_BOOL);
                b.emit(
                    vp.span,
                    IrOp::EqI32 {
                        dst: v_eq,
                        a: v_f,
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
            crate::ast::PatternKind::BoolLit(imm) => {
                let v_f = b.new_vreg(T_BOOL);
                b.emit(
                    vp.span,
                    IrOp::ObjGetAtom {
                        dst: v_f,
                        obj: v_subj,
                        atom_id,
                    },
                );
                let v_pat = b.new_vreg(T_BOOL);
                b.emit(
                    vp.span,
                    IrOp::ConstBool {
                        dst: v_pat,
                        imm: *imm,
                    },
                );
                let v_eq = b.new_vreg(T_BOOL);
                b.emit(
                    vp.span,
                    IrOp::Physeq {
                        dst: v_eq,
                        a: v_f,
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
                let bd = lookup_var(ctx, name.as_str(), vp.span)?;
                if bd.tid == T_DYNAMIC {
                    let v_f = b.new_vreg(T_DYNAMIC);
                    b.emit(
                        vp.span,
                        IrOp::ObjGetAtom {
                            dst: v_f,
                            obj: v_subj,
                            atom_id,
                        },
                    );
                    let v_eq = b.new_vreg(T_BOOL);
                    b.emit(
                        vp.span,
                        IrOp::Physeq {
                            dst: v_eq,
                            a: v_f,
                            b: bd.v,
                        },
                    );
                    let _ = chain_check(
                        b,
                        next_check,
                        v_eq,
                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                    );
                } else {
                    let v_f = b.new_vreg(bd.tid);
                    b.emit(
                        vp.span,
                        IrOp::ObjGetAtom {
                            dst: v_f,
                            obj: v_subj,
                            atom_id,
                        },
                    );
                    let v_eq = b.new_vreg(T_BOOL);
                    match bd.tid {
                        T_I32 => b.emit(
                            vp.span,
                            IrOp::EqI32 {
                                dst: v_eq,
                                a: v_f,
                                b: bd.v,
                            },
                        ),
                        _ => b.emit(
                            vp.span,
                            IrOp::Physeq {
                                dst: v_eq,
                                a: v_f,
                                b: bd.v,
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
            }
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {}
            _ => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    vp.span,
                    "nested patterns in object fields not supported yet",
                ))
            }
        }
    }

    b.term(IrTerminator::Jmp { target: bind_b });
    Ok(())
}
