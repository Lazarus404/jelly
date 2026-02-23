use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, TypeId, VRegId};
use crate::lower::{bind_local, LowerCtx};

use super::util::{
    elem_tid_for_array, elem_tid_for_list, emit_array_get_at, emit_list_drop, emit_list_nth_head,
};
use super::{T_LIST_BYTES, T_LIST_I32};

pub(super) fn bind_seq_pattern(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    pat_span: crate::ast::Span,
    pat: &crate::ast::PatternKind,
    v_subj: VRegId,
    t_subj: TypeId,
) -> Result<Option<(usize, String)>, CompileError> {
    match pat {
        crate::ast::PatternKind::ArrayExact(elems) => {
            let elem_tid =
                elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Type,
                        pat_span,
                        "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                    )
                })?;
            for (idx, p) in elems.iter().enumerate() {
                if let crate::ast::PatternKind::Bind(name) = &p.node {
                    if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                        let v_el = emit_list_nth_head(b, p.span, t_subj, v_subj, idx, elem_tid);
                        bind_local(ctx, name.as_str(), v_el, elem_tid);
                    } else {
                        let v_el = emit_array_get_at(b, p.span, v_subj, idx, elem_tid);
                        bind_local(ctx, name.as_str(), v_el, elem_tid);
                    }
                }
            }
            Ok(None)
        }
        crate::ast::PatternKind::ArrayHeadTail { head, rest } => {
            let elem_tid =
                elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Type,
                        pat_span,
                        "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                    )
                })?;
            if let crate::ast::PatternKind::Bind(name) = &head.node {
                if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                    let v_el = emit_list_nth_head(b, head.span, t_subj, v_subj, 0, elem_tid);
                    bind_local(ctx, name.as_str(), v_el, elem_tid);
                } else {
                    let v_el = emit_array_get_at(b, head.span, v_subj, 0, elem_tid);
                    bind_local(ctx, name.as_str(), v_el, elem_tid);
                }
            }
            if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                let v_tail = emit_list_drop(b, pat_span, t_subj, v_subj, 1);
                bind_local(ctx, rest.as_str(), v_tail, t_subj);
                Ok(None)
            } else {
                Ok(Some((1, rest.clone())))
            }
        }
        crate::ast::PatternKind::ArrayPrefixRest { prefix, rest } => {
            let elem_tid =
                elem_tid_for_array(t_subj).or_else(|| elem_tid_for_list(t_subj)).ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Type,
                        pat_span,
                        "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
                    )
                })?;
            for (idx, p) in prefix.iter().enumerate() {
                if let crate::ast::PatternKind::Bind(name) = &p.node {
                    if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                        let v_el = emit_list_nth_head(b, p.span, t_subj, v_subj, idx, elem_tid);
                        bind_local(ctx, name.as_str(), v_el, elem_tid);
                    } else {
                        let v_el = emit_array_get_at(b, p.span, v_subj, idx, elem_tid);
                        bind_local(ctx, name.as_str(), v_el, elem_tid);
                    }
                }
            }
            if t_subj == T_LIST_I32 || t_subj == T_LIST_BYTES {
                let v_cur = emit_list_drop(b, pat_span, t_subj, v_subj, prefix.len());
                bind_local(ctx, rest.as_str(), v_cur, t_subj);
                Ok(None)
            } else {
                Ok(Some((prefix.len(), rest.clone())))
            }
        }
        _ => Ok(None),
    }
}
