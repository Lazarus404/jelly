use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::super::super::{T_BOOL, T_I32};
use super::super::super::util::{chain_check, lower_pin_as};

pub(super) fn lower_check_array_head_tail_list(
    i: usize,
    pat_span: Span,
    head: &Pattern,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    _t_subj: TypeId,
    elem_tid: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    // len >= 1  (fail if len < 1)
    let v_is_nil = b.new_vreg(T_BOOL);
    b.emit(
        pat_span,
        IrOp::ListIsNil {
            dst: v_is_nil,
            list: v_subj,
        },
    );
    let v_ok = b.new_vreg(T_BOOL);
    b.emit(
        pat_span,
        IrOp::NotBool {
            dst: v_ok,
            src: v_is_nil,
        },
    );
    let _ = chain_check(
        b,
        next_check,
        v_ok,
        format!("match_check{}_pass{}", i, b.func.blocks.len()),
    );

    match &head.node {
        crate::ast::PatternKind::I32Lit(imm) => {
            if elem_tid != T_I32 {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    head.span,
                    "i32 element pattern requires List<i32>",
                ));
            }
            let v_el = b.new_vreg(T_I32);
            b.emit(
                head.span,
                IrOp::ListHead {
                    dst: v_el,
                    list: v_subj,
                },
            );
            let v_pat = b.new_vreg(T_I32);
            b.emit(
                head.span,
                IrOp::ConstI32 {
                    dst: v_pat,
                    imm: *imm,
                },
            );
            let v_eq = b.new_vreg(T_BOOL);
            b.emit(
                head.span,
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
            let (v_pin, t_pin) = lower_pin_as(ctx, b, name.as_str(), elem_tid, head.span)?;
            let v_el = b.new_vreg(elem_tid);
            b.emit(
                head.span,
                IrOp::ListHead {
                    dst: v_el,
                    list: v_subj,
                },
            );
            let v_eq = b.new_vreg(T_BOOL);
            match t_pin {
                T_I32 => b.emit(
                    head.span,
                    IrOp::EqI32 {
                        dst: v_eq,
                        a: v_el,
                        b: v_pin,
                    },
                ),
                _ => b.emit(
                    head.span,
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
                head.span,
                "nested patterns in lists not supported yet",
            ))
        }
    }

    b.term(IrTerminator::Jmp { target: bind_b });
    Ok(())
}
