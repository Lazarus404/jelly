use crate::ast::{Pattern, Span};
use crate::error::CompileError;
use crate::ir::{BlockId, IrBuilder, TypeId, VRegId};
use crate::lower::LowerCtx;

mod object;
mod tuple;

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
    object::lower_check_obj(
        i, pat_span, fields, ctx, b, v_subj, t_subj, bind_b, next_check,
    )
}

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
    tuple::lower_check_tuple_exact(
        i, pat_span, elems, ctx, b, v_subj, t_subj, bind_b, next_check,
    )
}
