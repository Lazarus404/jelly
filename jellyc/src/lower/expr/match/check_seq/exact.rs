use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::util::{elem_tid_for_array, elem_tid_for_list};

mod exact_array;
mod exact_list;

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
    if let Some(elem_tid) = elem_tid_for_array(t_subj) {
        exact_array::lower_check_array_exact_array(
            i, pat_span, elems, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    } else if let Some(elem_tid) = elem_tid_for_list(t_subj) {
        exact_list::lower_check_array_exact_list(
            i, pat_span, elems, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            pat_span,
            "array/list exact pattern requires Array<T> or List<T> subject",
        ))
    }
}
