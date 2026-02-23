mod exact;
mod head_tail;
mod prefix_rest;

use crate::ast::{Pattern, Span};
use crate::error::CompileError;
use crate::ir::{BlockId, IrBuilder, TypeId, VRegId};
use crate::lower::LowerCtx;

pub(super) fn lower_check_array_exact(
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
    exact::lower_check_array_exact(
        i, pat_span, elems, ctx, b, v_subj, t_subj, bind_b, next_check,
    )
}

pub(super) fn lower_check_array_head_tail(
    i: usize,
    pat_span: Span,
    head: &Pattern,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    head_tail::lower_check_array_head_tail(
        i, pat_span, head, ctx, b, v_subj, t_subj, bind_b, next_check,
    )
}

pub(super) fn lower_check_array_prefix_rest(
    i: usize,
    pat_span: Span,
    prefix: &[Pattern],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    prefix_rest::lower_check_array_prefix_rest(
        i, pat_span, prefix, ctx, b, v_subj, t_subj, bind_b, next_check,
    )
}
