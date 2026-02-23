use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::util::{elem_tid_for_array, elem_tid_for_list};

mod array;
mod list;

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
    if let Some(elem_tid) = elem_tid_for_array(t_subj) {
        array::lower_check_array_prefix_rest_array(
            i, pat_span, prefix, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    } else if let Some(elem_tid) = elem_tid_for_list(t_subj) {
        list::lower_check_array_prefix_rest_list(
            i, pat_span, prefix, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            pat_span,
            "prefix/rest pattern requires Array<T> or List<T> subject",
        ))
    }
}
