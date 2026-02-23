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

// Match expression lowering.

use std::collections::HashMap;

use crate::ast::{Expr, Span};
use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use crate::lower::{bind_local, intern_atom, lookup_var, lower_stmt, LowerCtx};
use super::{lower_expr_expect, T_BYTES, T_BOOL, T_DYNAMIC, T_I8, T_I16, T_I32, T_F16, T_F32, T_ARRAY_I32, T_ARRAY_BYTES, T_OBJECT, T_LIST_I32, T_LIST_BYTES};

fn elem_tid_for_array(arr_tid: TypeId) -> Option<TypeId> {
    match arr_tid {
        T_ARRAY_I32 => Some(T_I32),
        T_ARRAY_BYTES => Some(super::T_BYTES),
        _ => None,
    }
}

fn elem_tid_for_list(list_tid: TypeId) -> Option<TypeId> {
    match list_tid {
        T_LIST_I32 => Some(T_I32),
        T_LIST_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn lower_pin_as(
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
        T_I8 => b.emit(span, IrOp::FromDynI8 { dst: out, src: bd.v }),
        T_I16 => b.emit(span, IrOp::FromDynI16 { dst: out, src: bd.v }),
        T_I32 => b.emit(span, IrOp::FromDynI32 { dst: out, src: bd.v }),
        T_F16 => b.emit(span, IrOp::FromDynF16 { dst: out, src: bd.v }),
        T_F32 => b.emit(span, IrOp::FromDynF32 { dst: out, src: bd.v }),
        T_BOOL => b.emit(span, IrOp::FromDynBool { dst: out, src: bd.v }),
        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(span, IrOp::FromDynPtr { dst: out, src: bd.v }),
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

fn chain_check(
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
        b.emit(span, IrOp::ToDyn { dst: out, src: bd.v });
        out
    };
    let v_cond = b.new_vreg(T_BOOL);
    b.emit(span, IrOp::Physeq { dst: v_cond, a: subj_dyn, b: v_rhs_dyn });
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: bind_b,
        else_tgt: next_check,
    });
    Ok(())
}


pub fn lower_match_expr(
    e: &Expr,
    subject: &Expr,
    arms: &[crate::ast::MatchArm],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
            let out_tid = ctx
                .sem_expr_types
                .get(&NodeId(e.span))
                .copied()
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing semantic type for match"))?;
            if arms.is_empty() {
                return Err(CompileError::new(ErrorKind::Type, e.span, "match must have at least one arm"));
            }
            // For now, require the last arm to be `_ => { ...expr... }` so the match is exhaustive
            // and produces a value.
            if !matches!(arms.last().unwrap().pat.node, crate::ast::PatternKind::Wildcard) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "non-exhaustive match (last arm must be '_')",
                ));
            }
            if arms.last().unwrap().when.is_some() {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "last '_' match arm cannot have a when-guard",
                ));
            }
            if arms.last().unwrap().tail.is_none() {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "match must produce a value (last arm must have a value)",
                ));
            }

            if !b.is_open() {
                let nb = b.new_block(Some("cont".to_string()));
                b.set_block(nb);
            }

            let (v_subj, t_subj) = lower_expr_expect(subject, ctx, b)?;

            // Special case: matching on a Dynamic subject with scalar-only patterns.
            // This is the first place where `KINDOF` + `SWITCH_KIND` is a clear win.
            if t_subj == T_DYNAMIC {
                let scalar_ok = arms.iter().all(|a| {
                    matches!(
                        a.pat.node,
                        crate::ast::PatternKind::Wildcard
                            | crate::ast::PatternKind::Bind(_)
                            | crate::ast::PatternKind::Pin(_)
                            | crate::ast::PatternKind::BoolLit(_)
                            | crate::ast::PatternKind::I8Lit(_)
                            | crate::ast::PatternKind::I16Lit(_)
                            | crate::ast::PatternKind::I32Lit(_)
                    )
                });
                if scalar_ok {
                    return lower_match_dynamic_scalar(e, v_subj, arms, out_tid, ctx, b);
                }
            }

            let v_out: Option<VRegId> = Some(b.new_vreg(out_tid));

            // Pre-create check/bind/guard/body blocks.
            let mut check_bs = Vec::with_capacity(arms.len());
            let mut bind_bs = Vec::with_capacity(arms.len());
            let mut guard_bs: Vec<Option<crate::ir::BlockId>> = Vec::with_capacity(arms.len());
            let mut body_bs = Vec::with_capacity(arms.len());
            for i in 0..arms.len() {
                check_bs.push(b.new_block(Some(format!("match_check{}", i))));
                bind_bs.push(b.new_block(Some(format!("match_bind{}", i))));
                guard_bs.push(if arms[i].when.is_some() {
                    Some(b.new_block(Some(format!("match_when{}", i))))
                } else {
                    None
                });
                body_bs.push(b.new_block(Some(format!("match_body{}", i))));
            }

            // Create join last, so its uses can't precede defs.
            let join_b = b.new_block(Some("match_join".to_string()));

            // Jump into the first check.
            b.term(IrTerminator::Jmp { target: check_bs[0] });

            for i in 0..arms.len() {
                let next_check = if i + 1 < arms.len() { check_bs[i + 1] } else { bind_bs[i] };
                let body_b = body_bs[i];
                let guard_b = guard_bs[i];
                let bind_b = bind_bs[i];

                // --- check
                b.set_block(check_bs[i]);

                match &arms[i].pat.node {
                    crate::ast::PatternKind::Wildcard => {
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::BoolLit(p) => {
                        if t_subj != T_BOOL {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "match bool pattern requires bool subject",
                            ));
                        }
                        let v_pat = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::ConstBool { dst: v_pat, imm: *p });
                        let v_cond = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj, b: v_pat });
                        b.term(IrTerminator::JmpIf {
                            cond: v_cond,
                            then_tgt: bind_b,
                            else_tgt: next_check,
                        });
                    }
                    crate::ast::PatternKind::I32Lit(p) => {
                        if t_subj != T_I32 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "match i32 pattern requires i32 subject",
                            ));
                        }
                        let v_pat = b.new_vreg(T_I32);
                        b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_pat, imm: *p });
                        let v_cond = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj, b: v_pat });
                        b.term(IrTerminator::JmpIf {
                            cond: v_cond,
                            then_tgt: bind_b,
                            else_tgt: next_check,
                        });
                    }
                    crate::ast::PatternKind::I8Lit(p) => {
                        if t_subj != T_I8 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "match i8 pattern requires i8 subject",
                            ));
                        }
                        let v_pat = b.new_vreg(T_I8);
                        let imm = (*p as i16).to_le_bytes()[0];
                        b.emit(arms[i].pat.span, IrOp::ConstI8Imm { dst: v_pat, imm });
                        let v_cond = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj, b: v_pat });
                        b.term(IrTerminator::JmpIf {
                            cond: v_cond,
                            then_tgt: bind_b,
                            else_tgt: next_check,
                        });
                    }
                    crate::ast::PatternKind::I16Lit(p) => {
                        if t_subj != T_I16 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "match i16 pattern requires i16 subject",
                            ));
                        }
                        let v_pat = b.new_vreg(T_I16);
                        b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_pat, imm: *p });
                        let v_cond = b.new_vreg(T_BOOL);
                        b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj, b: v_pat });
                        b.term(IrTerminator::JmpIf {
                            cond: v_cond,
                            then_tgt: bind_b,
                            else_tgt: next_check,
                        });
                    }
                    crate::ast::PatternKind::Bind(_) => {
                        // Always matches; binding is established in the bind block.
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::Pin(name) => {
                        // Match subject against an existing name.
                        let bd = lookup_var(ctx, name.as_str(), arms[i].pat.span)?;
                        if bd.tid == T_DYNAMIC {
                            let v_subj_dyn = b.new_vreg(T_DYNAMIC);
                            b.emit(arms[i].pat.span, IrOp::ToDyn { dst: v_subj_dyn, src: v_subj });
                            let v_cond = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj_dyn, b: bd.v });
                            b.term(IrTerminator::JmpIf {
                                cond: v_cond,
                                then_tgt: bind_b,
                                else_tgt: next_check,
                            });
                        } else {
                            if bd.tid != t_subj {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    arms[i].pat.span,
                                    "pinned name has incompatible type",
                                ));
                            }
                            let v_cond = b.new_vreg(T_BOOL);
                            match t_subj {
                                T_I32 => b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj, b: bd.v }),
                                _ => b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj, b: bd.v }),
                            }
                            b.term(IrTerminator::JmpIf {
                                cond: v_cond,
                                then_tgt: bind_b,
                                else_tgt: next_check,
                            });
                        }
                    }
                    crate::ast::PatternKind::Obj(fields) => {
                        if t_subj != T_OBJECT {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "object pattern requires Object subject",
                            ));
                        }
                        for (k, vp) in fields {
                            let atom_id = intern_atom(k.as_str(), ctx);
                            let v_has = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::ObjHasAtom { dst: v_has, obj: v_subj, atom_id });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_has,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );

                            match &vp.node {
                                crate::ast::PatternKind::I32Lit(imm) => {
                                    let v_f = b.new_vreg(T_I32);
                                    b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_I32);
                                    b.emit(vp.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::EqI32 { dst: v_eq, a: v_f, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::BoolLit(imm) => {
                                    let v_f = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::ConstBool { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(vp.span, IrOp::Physeq { dst: v_eq, a: v_f, b: v_pat });
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
                                        b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(vp.span, IrOp::Physeq { dst: v_eq, a: v_f, b: bd.v });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    } else {
                                        let v_f = b.new_vreg(bd.tid);
                                        b.emit(vp.span, IrOp::ObjGetAtom { dst: v_f, obj: v_subj, atom_id });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match bd.tid {
                                            T_I32 => b.emit(vp.span, IrOp::EqI32 { dst: v_eq, a: v_f, b: bd.v }),
                                            _ => b.emit(vp.span, IrOp::Physeq { dst: v_eq, a: v_f, b: bd.v }),
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
                        // All checks passed.
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::TupleExact(elems) => {
                        if !ctx.type_ctx.is_tuple_type(t_subj) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "tuple pattern requires tuple subject",
                            ));
                        }
                        let elem_tids = ctx
                            .type_ctx
                            .tuple_elems(t_subj)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, arms[i].pat.span, "bad tuple type"))?
                            .to_vec();
                        if elem_tids.len() != elems.len() {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "tuple pattern arity mismatch",
                            ));
                        }

                        for (idx, p) in elems.iter().enumerate() {
                            let atom_id = intern_atom(idx.to_string().as_str(), ctx);
                            let elem_tid = elem_tids[idx];
                            match &p.node {
                                crate::ast::PatternKind::I32Lit(imm) => {
                                    if elem_tid != T_I32 {
                                        return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires I32 element"));
                                    }
                                    let v_el = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::BoolLit(imm) => {
                                    if elem_tid != T_BOOL {
                                        return Err(CompileError::new(ErrorKind::Type, p.span, "bool element pattern requires Bool element"));
                                    }
                                    let v_el = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                    let v_pat = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::ConstBool { dst: v_pat, imm: *imm });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pat });
                                    let _ = chain_check(
                                        b,
                                        next_check,
                                        v_eq,
                                        format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                    );
                                }
                                crate::ast::PatternKind::Pin(name) => {
                                    let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                    let v_el = b.new_vreg(elem_tid);
                                    b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                    let v_eq = b.new_vreg(T_BOOL);
                                    match t_pin {
                                        T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                        _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
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
                                        "nested patterns in tuples not supported yet",
                                    ))
                                }
                            }
                        }
                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::ArrayExact(elems) => {
                        if let Some(elem_tid) = elem_tid_for_array(t_subj) {
                            // --- Array exact match: len == elems.len()
                            let v_len = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });
                            let v_n = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_n, imm: elems.len() as i32 });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_ok, a: v_len, b: v_n });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );

                            for (idx, p) in elems.iter().enumerate() {
                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires Array<i32>"));
                                        }
                                        let v_i = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
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
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
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
                        } else if let Some(elem_tid) = elem_tid_for_list(t_subj) {
                            // --- List exact match: walk elems and ensure tail is nil at end.
                            let mut v_cur = v_subj;
                            for p in elems {
                                // require non-nil
                                let v_is_nil = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::ListIsNil { dst: v_is_nil, list: v_cur });
                                let v_ok = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::NotBool { dst: v_ok, src: v_is_nil });
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_ok,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );

                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires List<i32>"));
                                        }
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Pin(name) => {
                                        let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
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
                                            "nested patterns in lists not supported yet",
                                        ))
                                    }
                                }

                                let v_next = b.new_vreg(t_subj);
                                b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                v_cur = v_next;
                            }
                            // ensure nil at end
                            let v_is_nil = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::ListIsNil { dst: v_is_nil, list: v_cur });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_is_nil,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );
                            b.term(IrTerminator::Jmp { target: bind_b });
                        } else {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list exact pattern requires Array<T> or List<T> subject",
                            ));
                        }
                    }
                    crate::ast::PatternKind::ArrayHeadTail { head, .. } => {
                        let elem_tid = elem_tid_for_array(t_subj)
                            .or_else(|| elem_tid_for_list(t_subj))
                            .ok_or_else(|| {
                                CompileError::new(
                                    ErrorKind::Type,
                                    arms[i].pat.span,
                                    "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                                )
                            })?;
                        // len >= 1  (fail if len < 1)
                        if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                            let v_is_nil = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::ListIsNil { dst: v_is_nil, list: v_subj });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::NotBool { dst: v_ok, src: v_is_nil });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );
                        } else {
                            let v_len = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });
                            let v_one = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_one, imm: 1 });
                            let v_too_short = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::LtI32 { dst: v_too_short, a: v_len, b: v_one });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::NotBool { dst: v_ok, src: v_too_short });
                            let _ = chain_check(
                                b,
                                next_check,
                                v_ok,
                                format!("match_check{}_pass{}", i, b.func.blocks.len()),
                            );
                        }

                        match &head.node {
                            crate::ast::PatternKind::I32Lit(imm) => {
                                if elem_tid != T_I32 {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        head.span,
                                        "i32 element pattern requires Array<i32> or List<i32>",
                                    ));
                                }
                                let v_el = b.new_vreg(T_I32);
                                if t_subj == T_LIST_I32 {
                                    b.emit(head.span, IrOp::ListHead { dst: v_el, list: v_subj });
                                } else {
                                    let v0 = b.new_vreg(T_I32);
                                    b.emit(head.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                                    b.emit(head.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v0 });
                                }
                                let v_pat = b.new_vreg(T_I32);
                                b.emit(head.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                let v_eq = b.new_vreg(T_BOOL);
                                b.emit(head.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_eq,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );
                            }
                            crate::ast::PatternKind::Pin(name) => {
                                let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, head.span)?;
                                let v_el = b.new_vreg(elem_tid);
                                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                    b.emit(head.span, IrOp::ListHead { dst: v_el, list: v_subj });
                                } else {
                                    let v0 = b.new_vreg(T_I32);
                                    b.emit(head.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                                    b.emit(head.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v0 });
                                }
                                let v_eq = b.new_vreg(T_BOOL);
                                match t_pin {
                                    T_I32 => b.emit(head.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                    _ => b.emit(head.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
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
                                    head.span,
                                    "nested patterns in arrays not supported yet",
                                ))
                            }
                        }

                        b.term(IrTerminator::Jmp { target: bind_b });
                    }
                    crate::ast::PatternKind::ArrayPrefixRest { prefix, .. } => {
                        if let Some(elem_tid) = elem_tid_for_array(t_subj) {
                            // --- Array prefix/rest: len >= prefix.len()
                            let v_len = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });
                            let v_n = b.new_vreg(T_I32);
                            b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_n, imm: prefix.len() as i32 });
                            let v_too_short = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::LtI32 { dst: v_too_short, a: v_len, b: v_n });
                            let v_ok = b.new_vreg(T_BOOL);
                            b.emit(arms[i].pat.span, IrOp::NotBool { dst: v_ok, src: v_too_short });
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
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires Array<i32>"));
                                        }
                                        let v_i = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
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
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
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
                        } else if let Some(elem_tid) = elem_tid_for_list(t_subj) {
                            // --- List prefix/rest: ensure prefix elements exist and match; rest is tail after dropping prefix.len().
                            let mut v_cur = v_subj;
                            for p in prefix {
                                let v_is_nil = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::ListIsNil { dst: v_is_nil, list: v_cur });
                                let v_ok = b.new_vreg(T_BOOL);
                                b.emit(p.span, IrOp::NotBool { dst: v_ok, src: v_is_nil });
                                let _ = chain_check(
                                    b,
                                    next_check,
                                    v_ok,
                                    format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                );

                                match &p.node {
                                    crate::ast::PatternKind::I32Lit(imm) => {
                                        if elem_tid != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, p.span, "i32 element pattern requires List<i32>"));
                                        }
                                        let v_el = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_pat = b.new_vreg(T_I32);
                                        b.emit(p.span, IrOp::ConstI32 { dst: v_pat, imm: *imm });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pat });
                                        let _ = chain_check(
                                            b,
                                            next_check,
                                            v_eq,
                                            format!("match_check{}_pass{}", i, b.func.blocks.len()),
                                        );
                                    }
                                    crate::ast::PatternKind::Pin(name) => {
                                        let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, p.span)?;
                                        let v_el = b.new_vreg(elem_tid);
                                        b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                        let v_eq = b.new_vreg(T_BOOL);
                                        match t_pin {
                                            T_I32 => b.emit(p.span, IrOp::EqI32 { dst: v_eq, a: v_el, b: v_pin }),
                                            _ => b.emit(p.span, IrOp::Physeq { dst: v_eq, a: v_el, b: v_pin }),
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
                                            "nested patterns in lists not supported yet",
                                        ))
                                    }
                                }

                                let v_next = b.new_vreg(t_subj);
                                b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                v_cur = v_next;
                            }
                            b.term(IrTerminator::Jmp { target: bind_b });
                        } else {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "prefix/rest pattern requires Array<T> or List<T> subject",
                            ));
                        }
                    }
                }

                // --- bind block (introduce bindings, then jump to when/body)
                b.set_block(bind_b);
                ctx.env_stack.push(HashMap::new());

                // If present, compute `rest` as a fresh array slice starting at `start_idx`.
                let mut need_slice: Option<(usize, String)> = None;

                match &arms[i].pat.node {
                    crate::ast::PatternKind::Bind(name) => {
                        bind_local(ctx, name.as_str(), v_subj, t_subj);
                    }
                    crate::ast::PatternKind::Obj(fields) => {
                        for (k, vp) in fields {
                            if let crate::ast::PatternKind::Bind(name) = &vp.node {
                                let atom_id = intern_atom(k.as_str(), ctx);
                                let v = b.new_vreg(T_DYNAMIC);
                                b.emit(vp.span, IrOp::ObjGetAtom { dst: v, obj: v_subj, atom_id });
                                bind_local(ctx, name.as_str(), v, T_DYNAMIC);
                            }
                        }
                    }
                    crate::ast::PatternKind::TupleExact(elems) => {
                        let elem_tids = ctx.type_ctx.tuple_elems(t_subj).ok_or_else(|| {
                            CompileError::new(ErrorKind::Type, arms[i].pat.span, "tuple pattern requires tuple subject")
                        })?.to_vec();
                        if elem_tids.len() != elems.len() {
                            return Err(CompileError::new(ErrorKind::Type, arms[i].pat.span, "tuple pattern arity mismatch"));
                        }
                        for (idx, p) in elems.iter().enumerate() {
                            if let crate::ast::PatternKind::Bind(name) = &p.node {
                                let idx_s = idx.to_string();
                                let atom_id = intern_atom(idx_s.as_str(), ctx);
                                let tid = elem_tids[idx];
                                let v_el = b.new_vreg(tid);
                                b.emit(p.span, IrOp::ObjGetAtom { dst: v_el, obj: v_subj, atom_id });
                                bind_local(ctx, name.as_str(), v_el, tid);
                            }
                        }
                    }
                    crate::ast::PatternKind::ArrayExact(elems) => {
                        let elem_tid = elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                            )
                        })?;
                        for (idx, p) in elems.iter().enumerate() {
                            if let crate::ast::PatternKind::Bind(name) = &p.node {
                                let v_el = b.new_vreg(elem_tid);
                                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                    // Walk idx steps then take head.
                                    let mut v_cur = v_subj;
                                    for _ in 0..idx {
                                        let v_next = b.new_vreg(t_subj);
                                        b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                        v_cur = v_next;
                                    }
                                    b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                } else {
                                    let v_i = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                    b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                }
                                bind_local(ctx, name.as_str(), v_el, elem_tid);
                            }
                        }
                    }
                    crate::ast::PatternKind::ArrayHeadTail { head, rest } => {
                        let elem_tid = elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                            )
                        })?;
                        if let crate::ast::PatternKind::Bind(name) = &head.node {
                            let v_el = b.new_vreg(elem_tid);
                            if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                b.emit(head.span, IrOp::ListHead { dst: v_el, list: v_subj });
                            } else {
                                let v0 = b.new_vreg(T_I32);
                                b.emit(head.span, IrOp::ConstI32 { dst: v0, imm: 0 });
                                b.emit(head.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v0 });
                            }
                            bind_local(ctx, name.as_str(), v_el, elem_tid);
                        }
                        if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                            let v_tail = b.new_vreg(t_subj);
                            b.emit(arms[i].pat.span, IrOp::ListTail { dst: v_tail, list: v_subj });
                            bind_local(ctx, rest.as_str(), v_tail, t_subj);
                        } else {
                            // rest is indexes 1..len
                            need_slice = Some((1, rest.clone()));
                        }
                    }
                    crate::ast::PatternKind::ArrayPrefixRest { prefix, rest } => {
                        let elem_tid = elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                            CompileError::new(
                                ErrorKind::Type,
                                arms[i].pat.span,
                                "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                            )
                        })?;
                        for (idx, p) in prefix.iter().enumerate() {
                            if let crate::ast::PatternKind::Bind(name) = &p.node {
                                let v_el = b.new_vreg(elem_tid);
                                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                                    let mut v_cur = v_subj;
                                    for _ in 0..idx {
                                        let v_next = b.new_vreg(t_subj);
                                        b.emit(p.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                        v_cur = v_next;
                                    }
                                    b.emit(p.span, IrOp::ListHead { dst: v_el, list: v_cur });
                                } else {
                                    let v_i = b.new_vreg(T_I32);
                                    b.emit(p.span, IrOp::ConstI32 { dst: v_i, imm: idx as i32 });
                                    b.emit(p.span, IrOp::ArrayGet { dst: v_el, arr: v_subj, index: v_i });
                                }
                                bind_local(ctx, name.as_str(), v_el, elem_tid);
                            }
                        }
                        if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                            let mut v_cur = v_subj;
                            for _ in 0..prefix.len() {
                                let v_next = b.new_vreg(t_subj);
                                b.emit(arms[i].pat.span, IrOp::ListTail { dst: v_next, list: v_cur });
                                v_cur = v_next;
                            }
                            bind_local(ctx, rest.as_str(), v_cur, t_subj);
                        } else {
                            need_slice = Some((prefix.len(), rest.clone()));
                        }
                    }
                    _ => {}
                }

                // Compute rest slice if needed.
                let mut bind_cont_block = bind_b;
                if let Some((start_idx, rest_name)) = need_slice {
                    let elem_tid = elem_tid_for_array(t_subj).ok_or_else(|| {
                        CompileError::new(ErrorKind::Type, arms[i].pat.span, "array pattern requires Array<i32> or Array<bytes> subject")
                    })?;

                    let v_len = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ArrayLen { dst: v_len, arr: v_subj });

                    let v_start = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_start, imm: start_idx as i32 });

                    let v_rest_len = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::SubI32 { dst: v_rest_len, a: v_len, b: v_start });

                    let v_rest = b.new_vreg(t_subj);
                    b.emit(arms[i].pat.span, IrOp::ArrayNew { dst: v_rest, len: v_rest_len });
                    bind_local(ctx, rest_name.as_str(), v_rest, t_subj);

                    // Two counters:
                    // - `dst_i`: index into the new `rest` array (0..rest_len-1)
                    // - `src_i`: index into the source array (start..len-1)
                    let v_zero = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_zero, imm: 0 });
                    let v_one = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_one, imm: 1 });

                    let v_dst_i = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_dst_i, src: v_zero });

                    let v_src_i = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_src_i, src: v_start });

                    let loop_cond = b.new_block(Some(format!("match_slice{}_cond", i)));
                    let loop_body = b.new_block(Some(format!("match_slice{}_body", i)));
                    let loop_exit = b.new_block(Some(format!("match_slice{}_exit", i)));

                    b.term(IrTerminator::Jmp { target: loop_cond });

                    b.set_block(loop_cond);
                    let v_more = b.new_vreg(T_BOOL);
                    b.emit(arms[i].pat.span, IrOp::LtI32 { dst: v_more, a: v_dst_i, b: v_rest_len });
                    b.term(IrTerminator::JmpIf {
                        cond: v_more,
                        then_tgt: loop_body,
                        else_tgt: loop_exit,
                    });

                    b.set_block(loop_body);
                    let v_val = b.new_vreg(elem_tid);
                    b.emit(arms[i].pat.span, IrOp::ArrayGet { dst: v_val, arr: v_subj, index: v_src_i });
                    b.emit(arms[i].pat.span, IrOp::ArraySet { arr: v_rest, index: v_dst_i, value: v_val });

                    let v_src_i2 = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::AddI32 { dst: v_src_i2, a: v_src_i, b: v_one });
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_src_i, src: v_src_i2 });

                    let v_dst_i2 = b.new_vreg(T_I32);
                    b.emit(arms[i].pat.span, IrOp::AddI32 { dst: v_dst_i2, a: v_dst_i, b: v_one });
                    b.emit(arms[i].pat.span, IrOp::Mov { dst: v_dst_i, src: v_dst_i2 });
                    b.term(IrTerminator::Jmp { target: loop_cond });

                    b.set_block(loop_exit);
                    bind_cont_block = loop_exit;
                }

                // --- when guard (optional)
                if let Some(gb) = guard_b {
                    b.set_block(bind_cont_block);
                    b.term(IrTerminator::Jmp { target: gb });

                    b.set_block(gb);
                    let w = arms[i].when.as_ref().expect("guard block implies when");
                    let (v_w, t_w) = lower_expr_expect(w, ctx, b)?;
                    if t_w != T_BOOL {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            w.span,
                            "match when guard must be bool",
                        ));
                    }
                    b.term(IrTerminator::JmpIf {
                        cond: v_w,
                        then_tgt: body_b,
                        else_tgt: next_check,
                    });
                } else {
                    b.set_block(bind_cont_block);
                    b.term(IrTerminator::Jmp { target: body_b });
                }

                // --- body
                b.set_block(body_b);
                for st in &arms[i].body {
                    lower_stmt(st, ctx, b)?;
                }

                // If the arm already terminated control flow (eg `return`), don't try to
                // fall through / join.
                if !b.is_open() {
                    ctx.env_stack.pop();
                    continue;
                }

                if let Some(tail) = &arms[i].tail {
                    let (v_tail, t_tail) = lower_expr_expect(tail, ctx, b)?;
                    if t_tail != out_tid {
                        return Err(CompileError::new(
                            ErrorKind::Internal,
                            tail.span,
                            "semantic type mismatch for match arm value",
                        ));
                    }
                    let dst = v_out.expect("match output");
                    b.emit(e.span, IrOp::Mov { dst, src: v_tail });
                    ctx.env_stack.pop();
                    b.term(IrTerminator::Jmp { target: join_b });
                } else {
                    // Fall through to next arm.
                    ctx.env_stack.pop();
                    b.term(IrTerminator::Jmp { target: next_check });
                }
            }

            let dst = v_out.expect("match output");
            b.set_block(join_b);
            Ok((dst, out_tid))
}

fn lower_match_dynamic_scalar(
    e: &Expr,
    v_subj: VRegId,
    arms: &[crate::ast::MatchArm],
    out_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Re-check exhaustiveness / value production (same rules as the general match lowering).
    if arms.is_empty() {
        return Err(CompileError::new(ErrorKind::Type, e.span, "match must have at least one arm"));
    }
    if !matches!(arms.last().unwrap().pat.node, crate::ast::PatternKind::Wildcard) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "non-exhaustive match (last arm must be '_')",
        ));
    }
    if arms.last().unwrap().when.is_some() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "last '_' match arm cannot have a when-guard",
        ));
    }
    if arms.last().unwrap().tail.is_none() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "match must produce a value (last arm must have a value)",
        ));
    }

    // Ensure we are in an open block.
    if !b.is_open() {
        let nb = b.new_block(Some("cont".to_string()));
        b.set_block(nb);
    }

    let v_out: Option<VRegId> = Some(b.new_vreg(out_tid));

    // Shared blocks per arm (bind/guard/body), and per-kind check chains.
    let mut bind_bs = Vec::with_capacity(arms.len());
    let mut guard_bs: Vec<Option<crate::ir::BlockId>> = Vec::with_capacity(arms.len());
    let mut body_bs = Vec::with_capacity(arms.len());
    let mut resume_bs = Vec::with_capacity(arms.len());
    let mut check_bool_bs = Vec::with_capacity(arms.len());
    let mut check_i32_bs = Vec::with_capacity(arms.len());
    let mut check_other_bs = Vec::with_capacity(arms.len());

    for i in 0..arms.len() {
        bind_bs.push(b.new_block(Some(format!("dmatch_bind{}", i))));
        guard_bs.push(if arms[i].when.is_some() {
            Some(b.new_block(Some(format!("dmatch_when{}", i))))
        } else {
            None
        });
        body_bs.push(b.new_block(Some(format!("dmatch_body{}", i))));
        resume_bs.push(b.new_block(Some(format!("dmatch_resume{}", i))));
        check_bool_bs.push(b.new_block(Some(format!("dmatch_check_bool{}", i))));
        check_i32_bs.push(b.new_block(Some(format!("dmatch_check_i32{}", i))));
        check_other_bs.push(b.new_block(Some(format!("dmatch_check_other{}", i))));
    }

    let join_b = b.new_block(Some("dmatch_join".to_string()));

    // Find chain entry points (first arm that could match the kind).
    let mut first_bool = None::<crate::ir::BlockId>;
    let mut first_i32 = None::<crate::ir::BlockId>;
    let mut first_other = None::<crate::ir::BlockId>;
    for i in 0..arms.len() {
        match &arms[i].pat.node {
            crate::ast::PatternKind::BoolLit(_) | crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) | crate::ast::PatternKind::Pin(_) => {
                if first_bool.is_none() {
                    first_bool = Some(check_bool_bs[i]);
                }
            }
            _ => {}
        }
        match &arms[i].pat.node {
            crate::ast::PatternKind::I32Lit(_)
                | crate::ast::PatternKind::I8Lit(_)
                | crate::ast::PatternKind::I16Lit(_)
                | crate::ast::PatternKind::Wildcard
                | crate::ast::PatternKind::Bind(_)
                | crate::ast::PatternKind::Pin(_) =>
            {
                if first_i32.is_none() {
                    first_i32 = Some(check_i32_bs[i]);
                }
            }
            _ => {}
        }
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) | crate::ast::PatternKind::Pin(_) => {
                if first_other.is_none() {
                    first_other = Some(check_other_bs[i]);
                }
            }
            _ => {}
        }
    }
    let entry_bool = first_bool.unwrap_or(check_other_bs[0]);
    let entry_i32 = first_i32.unwrap_or(check_other_bs[0]);
    let entry_other = first_other.unwrap_or(check_other_bs[0]);

    // Dispatch by KINDOF using SWITCH_KIND.
    let v_kind = b.new_vreg(T_I32);
    b.emit(e.span, IrOp::Kindof { dst: v_kind, src: v_subj });
    b.term(IrTerminator::SwitchKind {
        kind: v_kind,
        cases: vec![(1, entry_bool), (2, entry_i32)],
        default: entry_other,
    });

    // Resume dispatch blocks: after an arm fails its `when` guard or falls through,
    // jump to the next arm index by re-dispatching on the (already computed) kind code.
    for i in 0..arms.len() {
        b.set_block(resume_bs[i]);
        let j = i + 1;
        if j >= arms.len() {
            // No next arm; should be unreachable under our exhaustiveness rules.
            b.term(IrTerminator::Jmp { target: join_b });
            continue;
        }
        b.term(IrTerminator::SwitchKind {
            kind: v_kind,
            cases: vec![(1, check_bool_bs[j]), (2, check_i32_bs[j])],
            default: check_other_bs[j],
        });
    }

    for i in 0..arms.len() {
        let bind_b = bind_bs[i];
        let body_b = body_bs[i];
        let guard_b = guard_bs[i];
        let resume_b = resume_bs[i];

        // --- check chain helpers
        let next_bool = if i + 1 < arms.len() { check_bool_bs[i + 1] } else { bind_b };
        let next_i32 = if i + 1 < arms.len() { check_i32_bs[i + 1] } else { bind_b };
        let next_other = if i + 1 < arms.len() { check_other_bs[i + 1] } else { bind_b };

        // --- check_bool
        b.set_block(check_bool_bs[i]);
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
                b.term(IrTerminator::Jmp { target: bind_b });
            }
            crate::ast::PatternKind::Pin(name) => {
                emit_pin_check_dynamic_subject(ctx, b, v_subj, arms[i].pat.span, name.as_str(), next_bool, bind_b)?;
            }
            crate::ast::PatternKind::BoolLit(p) => {
                let v_subj_b = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::FromDynBool { dst: v_subj_b, src: v_subj });
                let v_pat = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::ConstBool { dst: v_pat, imm: *p });
                let v_cond = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::Physeq { dst: v_cond, a: v_subj_b, b: v_pat });
                b.term(IrTerminator::JmpIf {
                    cond: v_cond,
                    then_tgt: bind_b,
                    else_tgt: next_bool,
                });
            }
            // Not applicable in bool chain.
            _ => b.term(IrTerminator::Jmp { target: next_bool }),
        }

        // --- check_i32
        b.set_block(check_i32_bs[i]);
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
                b.term(IrTerminator::Jmp { target: bind_b });
            }
            crate::ast::PatternKind::Pin(name) => {
                emit_pin_check_dynamic_subject(ctx, b, v_subj, arms[i].pat.span, name.as_str(), next_i32, bind_b)?;
            }
            crate::ast::PatternKind::I32Lit(p) => {
                let v_subj_i = b.new_vreg(T_I32);
                b.emit(arms[i].pat.span, IrOp::FromDynI32 { dst: v_subj_i, src: v_subj });
                let v_pat = b.new_vreg(T_I32);
                b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_pat, imm: *p });
                let v_cond = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj_i, b: v_pat });
                b.term(IrTerminator::JmpIf {
                    cond: v_cond,
                    then_tgt: bind_b,
                    else_tgt: next_i32,
                });
            }
            crate::ast::PatternKind::I8Lit(p) => {
                let v_subj_i = b.new_vreg(T_I8);
                b.emit(arms[i].pat.span, IrOp::FromDynI8 { dst: v_subj_i, src: v_subj });
                let v_pat = b.new_vreg(T_I8);
                let imm = (*p as i16).to_le_bytes()[0];
                b.emit(arms[i].pat.span, IrOp::ConstI8Imm { dst: v_pat, imm });
                let v_cond = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj_i, b: v_pat });
                b.term(IrTerminator::JmpIf {
                    cond: v_cond,
                    then_tgt: bind_b,
                    else_tgt: next_i32,
                });
            }
            crate::ast::PatternKind::I16Lit(p) => {
                let v_subj_i = b.new_vreg(T_I16);
                b.emit(arms[i].pat.span, IrOp::FromDynI16 { dst: v_subj_i, src: v_subj });
                let v_pat = b.new_vreg(T_I16);
                b.emit(arms[i].pat.span, IrOp::ConstI32 { dst: v_pat, imm: *p });
                let v_cond = b.new_vreg(T_BOOL);
                b.emit(arms[i].pat.span, IrOp::EqI32 { dst: v_cond, a: v_subj_i, b: v_pat });
                b.term(IrTerminator::JmpIf {
                    cond: v_cond,
                    then_tgt: bind_b,
                    else_tgt: next_i32,
                });
            }
            _ => b.term(IrTerminator::Jmp { target: next_i32 }),
        }

        // --- check_other (only wildcard/bind/pin apply)
        b.set_block(check_other_bs[i]);
        match &arms[i].pat.node {
            crate::ast::PatternKind::Wildcard | crate::ast::PatternKind::Bind(_) => {
                b.term(IrTerminator::Jmp { target: bind_b });
            }
            crate::ast::PatternKind::Pin(name) => {
                emit_pin_check_dynamic_subject(ctx, b, v_subj, arms[i].pat.span, name.as_str(), next_other, bind_b)?;
            }
            _ => b.term(IrTerminator::Jmp { target: next_other }),
        }

        // --- bind block
        b.set_block(bind_b);
        ctx.env_stack.push(HashMap::new());
        if let crate::ast::PatternKind::Bind(name) = &arms[i].pat.node {
            bind_local(ctx, name.as_str(), v_subj, T_DYNAMIC);
        }

        // Route through optional when-guard.
        if let Some(gb) = guard_b {
            b.term(IrTerminator::Jmp { target: gb });
            b.set_block(gb);
            let w = arms[i].when.as_ref().expect("guard block implies when");
            let (v_w, t_w) = lower_expr_expect(w, ctx, b)?;
            if t_w != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, w.span, "match when guard must be bool"));
            }
            b.term(IrTerminator::JmpIf {
                cond: v_w,
                then_tgt: body_b,
                else_tgt: resume_b,
            });
        } else {
            b.term(IrTerminator::Jmp { target: body_b });
        }

        // --- body
        b.set_block(body_b);
        for st in &arms[i].body {
            lower_stmt(st, ctx, b)?;
        }
        if !b.is_open() {
            ctx.env_stack.pop();
            continue;
        }

        if let Some(tail) = &arms[i].tail {
            let (v_tail, t_tail) = lower_expr_expect(tail, ctx, b)?;
            if t_tail != out_tid {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    tail.span,
                    "semantic type mismatch for match arm value",
                ));
            }
            let dst = v_out.expect("match output");
            b.emit(e.span, IrOp::Mov { dst, src: v_tail });
            ctx.env_stack.pop();
            b.term(IrTerminator::Jmp { target: join_b });
        } else {
            ctx.env_stack.pop();
            b.term(IrTerminator::Jmp { target: resume_b });
        }
    }

    let dst = v_out.expect("allocated when type known");
    b.set_block(join_b);
    Ok((dst, out_tid))
}
