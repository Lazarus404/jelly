use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::super::super::{T_BOOL, T_I32};
use super::super::super::util::{chain_check, lower_pin_as};

pub(super) fn lower_check_array_exact_array(
    i: usize,
    pat_span: Span,
    elems: &[Pattern],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    _t_subj: TypeId,
    elem_tid: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    // --- Array exact match: len == elems.len()
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
            imm: elems.len() as i32,
        },
    );
    let v_ok = b.new_vreg(T_BOOL);
    b.emit(
        pat_span,
        IrOp::EqI32 {
            dst: v_ok,
            a: v_len,
            b: v_n,
        },
    );
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
