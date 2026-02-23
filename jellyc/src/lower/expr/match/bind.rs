use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};
use crate::lower::{bind_local, intern_atom, LowerCtx};
use crate::typectx::T_DYNAMIC;

use super::super::{T_BOOL, T_I32};
use super::util::elem_tid_for_array;

pub(super) fn bind_obj_fields(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    fields: &[(String, Pattern)],
) {
    for (k, vp) in fields {
        if let crate::ast::PatternKind::Bind(name) = &vp.node {
            let atom_id = intern_atom(k.as_str(), ctx);
            let v = b.new_vreg(T_DYNAMIC);
            b.emit(
                vp.span,
                IrOp::ObjGetAtom {
                    dst: v,
                    obj: v_subj,
                    atom_id,
                },
            );
            bind_local(ctx, name.as_str(), v, T_DYNAMIC);
        }
    }
}

pub(super) fn bind_tuple_elems(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    pat_span: Span,
    elems: &[Pattern],
) -> Result<(), CompileError> {
    let elem_tids = ctx
        .type_ctx
        .tuple_elems(t_subj)
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Type,
                pat_span,
                "tuple pattern requires tuple subject",
            )
        })?
        .to_vec();
    if elem_tids.len() != elems.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            pat_span,
            "tuple pattern arity mismatch",
        ));
    }
    for (idx, p) in elems.iter().enumerate() {
        if let crate::ast::PatternKind::Bind(name) = &p.node {
            let idx_s = idx.to_string();
            let atom_id = intern_atom(idx_s.as_str(), ctx);
            let tid = elem_tids[idx];
            let v_el = b.new_vreg(tid);
            b.emit(
                p.span,
                IrOp::ObjGetAtom {
                    dst: v_el,
                    obj: v_subj,
                    atom_id,
                },
            );
            bind_local(ctx, name.as_str(), v_el, tid);
        }
    }
    Ok(())
}

pub(super) fn bind_array_rest_slice(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    i: usize,
    span: Span,
    v_subj: VRegId,
    t_subj: TypeId,
    start_idx: usize,
    rest_name: &str,
) -> Result<BlockId, CompileError> {
    let elem_tid = elem_tid_for_array(t_subj).ok_or_else(|| {
        CompileError::new(
            ErrorKind::Type,
            span,
            "array pattern requires Array<i32> or Array<bytes> subject",
        )
    })?;

    let v_len = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::ArrayLen {
            dst: v_len,
            arr: v_subj,
        },
    );

    let v_start = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::ConstI32 {
            dst: v_start,
            imm: start_idx as i32,
        },
    );

    let v_rest_len = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::SubI32 {
            dst: v_rest_len,
            a: v_len,
            b: v_start,
        },
    );

    let v_rest = b.new_vreg(t_subj);
    b.emit(
        span,
        IrOp::ArrayNew {
            dst: v_rest,
            len: v_rest_len,
        },
    );
    bind_local(ctx, rest_name, v_rest, t_subj);

    // Two counters:
    // - `dst_i`: index into the new `rest` array (0..rest_len-1)
    // - `src_i`: index into the source array (start..len-1)
    let v_zero = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::ConstI32 {
            dst: v_zero,
            imm: 0,
        },
    );
    let v_one = b.new_vreg(T_I32);
    b.emit(span, IrOp::ConstI32 { dst: v_one, imm: 1 });

    let v_dst_i = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::Mov {
            dst: v_dst_i,
            src: v_zero,
        },
    );

    let v_src_i = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::Mov {
            dst: v_src_i,
            src: v_start,
        },
    );

    let loop_cond = b.new_block(Some(format!("match_slice{}_cond", i)));
    let loop_body = b.new_block(Some(format!("match_slice{}_body", i)));
    let loop_exit = b.new_block(Some(format!("match_slice{}_exit", i)));

    b.term(IrTerminator::Jmp { target: loop_cond });

    b.set_block(loop_cond);
    let v_more = b.new_vreg(T_BOOL);
    b.emit(
        span,
        IrOp::LtI32 {
            dst: v_more,
            a: v_dst_i,
            b: v_rest_len,
        },
    );
    b.term(IrTerminator::JmpIf {
        cond: v_more,
        then_tgt: loop_body,
        else_tgt: loop_exit,
    });

    b.set_block(loop_body);
    let v_val = b.new_vreg(elem_tid);
    b.emit(
        span,
        IrOp::ArrayGet {
            dst: v_val,
            arr: v_subj,
            index: v_src_i,
        },
    );
    b.emit(
        span,
        IrOp::ArraySet {
            arr: v_rest,
            index: v_dst_i,
            value: v_val,
        },
    );

    let v_src_i2 = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::AddI32 {
            dst: v_src_i2,
            a: v_src_i,
            b: v_one,
        },
    );
    b.emit(
        span,
        IrOp::Mov {
            dst: v_src_i,
            src: v_src_i2,
        },
    );

    let v_dst_i2 = b.new_vreg(T_I32);
    b.emit(
        span,
        IrOp::AddI32 {
            dst: v_dst_i2,
            a: v_dst_i,
            b: v_one,
        },
    );
    b.emit(
        span,
        IrOp::Mov {
            dst: v_dst_i,
            src: v_dst_i2,
        },
    );
    b.term(IrTerminator::Jmp { target: loop_cond });

    b.set_block(loop_exit);
    Ok(loop_exit)
}
