use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::util::{elem_tid_for_array, elem_tid_for_list};

mod array;
mod list;

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
    let elem_tid = elem_tid_for_array(t_subj)
        .or_else(|| elem_tid_for_list(t_subj))
        .ok_or_else(|| {
            CompileError::new(
            ErrorKind::Type,
            pat_span,
            "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
        )
        })?;

    if elem_tid_for_list(t_subj).is_some() {
        list::lower_check_array_head_tail_list(
            i, pat_span, head, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    } else {
        array::lower_check_array_head_tail_array(
            i, pat_span, head, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    }
}
