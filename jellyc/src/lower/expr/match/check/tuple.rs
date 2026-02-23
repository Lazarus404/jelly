use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{intern_atom, LowerCtx};

use super::super::super::{T_BOOL, T_I32};
use super::super::util::{chain_check, lower_pin_as};

pub(super) fn lower_check_tuple_exact(
    i: usize,
    pat_span: Span,
    elems: &[Pattern],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    if !ctx.type_ctx.is_tuple_type(t_subj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            pat_span,
            "tuple pattern requires tuple subject",
        ));
    }
    let elem_tids = ctx
        .type_ctx
        .tuple_elems(t_subj)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, pat_span, "bad tuple type"))?
        .to_vec();
    if elem_tids.len() != elems.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            pat_span,
            "tuple pattern arity mismatch",
        ));
    }

    for (idx, p) in elems.iter().enumerate() {
        let atom_id = intern_atom(idx.to_string().as_str(), ctx);
        let elem_tid = elem_tids[idx];
        match &p.node {
            crate::ast::PatternKind::I32Lit(imm) => {
                if elem_tid != T_I32 {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        p.span,
                        "i32 element pattern requires I32 element",
                    ));
                }
                let v_el = b.new_vreg(T_I32);
                b.emit(
                    p.span,
                    IrOp::ObjGetAtom {
                        dst: v_el,
                        obj: v_subj,
                        atom_id,
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
            crate::ast::PatternKind::BoolLit(imm) => {
                if elem_tid != T_BOOL {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        p.span,
                        "bool element pattern requires Bool element",
                    ));
                }
                let v_el = b.new_vreg(T_BOOL);
                b.emit(
                    p.span,
                    IrOp::ObjGetAtom {
                        dst: v_el,
                        obj: v_subj,
                        atom_id,
                    },
                );
                let v_pat = b.new_vreg(T_BOOL);
                b.emit(
                    p.span,
                    IrOp::ConstBool {
                        dst: v_pat,
                        imm: *imm,
                    },
                );
                let v_eq = b.new_vreg(T_BOOL);
                b.emit(
                    p.span,
                    IrOp::Physeq {
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
                let v_el = b.new_vreg(elem_tid);
                b.emit(
                    p.span,
                    IrOp::ObjGetAtom {
                        dst: v_el,
                        obj: v_subj,
                        atom_id,
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
                    "nested patterns in tuples not supported yet",
                ))
            }
        }
    }
    b.term(IrTerminator::Jmp { target: bind_b });
    Ok(())
}
