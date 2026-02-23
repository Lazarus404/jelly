use crate::ast::{Expr, MatchArm, Pattern, PatternKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_BOOL, T_BYTES, T_DYNAMIC, T_I32, T_LIST_BYTES, T_LIST_I32,
};

use super::TypeChecker;

pub(super) fn type_match(
    tc: &mut TypeChecker,
    e: &Expr,
    subject: &Expr,
    arms: &[MatchArm],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let subj_tid = tc.check_expr(subject, None)?;
    let mut out_t: Option<TypeId> = None;
    for a in arms {
        let t = check_arm(tc, a, subj_tid, expect)?;
        // Arms without a tail expression are "fallthrough" arms and do not
        // contribute a value to the match result.
        let Some(t) = t else { continue };
        if let Some(prev) = out_t {
            if t != prev {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "match arms must have same type",
                ));
            }
        } else {
            out_t = Some(t);
        }
    }
    Ok(out_t.unwrap_or(expect.unwrap_or(T_DYNAMIC)))
}

fn check_arm(
    tc: &mut TypeChecker,
    a: &MatchArm,
    subj_tid: TypeId,
    expect: Option<TypeId>,
) -> Result<Option<TypeId>, CompileError> {
    tc.with_scope(|tc| {
        bind_pattern_typed(tc, &a.pat, subj_tid);
        if let Some(w) = &a.when {
            let t = tc.check_expr(w, Some(T_BOOL))?;
            if t != T_BOOL {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    w.span,
                    "match when must be bool",
                ));
            }
        }
        for st in &a.body {
            tc.check_stmt(st)?;
        }
        let out_t = if let Some(t) = &a.tail {
            Some(tc.check_expr(t, expect)?)
        } else {
            None
        };
        Ok(out_t)
    })
}

fn bind_pattern_typed(tc: &mut TypeChecker, p: &Pattern, subj_tid: TypeId) {
    match &p.node {
        PatternKind::Bind(n) => tc.bind_local(n, subj_tid),
        PatternKind::Obj(fields) => {
            for (_k, v) in fields {
                bind_pattern_typed(tc, v, T_DYNAMIC);
            }
        }
        PatternKind::TupleExact(elems) => {
            let elem_tids: Vec<TypeId> = tc.tuple_elems_or_empty(subj_tid);
            for (i, el) in elems.iter().enumerate() {
                let tid = elem_tids.get(i).copied().unwrap_or(T_DYNAMIC);
                bind_pattern_typed(tc, el, tid);
            }
        }
        PatternKind::ArrayExact(elems) => {
            let elem_tid = match subj_tid {
                T_ARRAY_I32 | T_LIST_I32 => Some(T_I32),
                T_ARRAY_BYTES | T_LIST_BYTES => Some(T_BYTES),
                _ => None,
            };
            for el in elems {
                bind_pattern_typed(tc, el, elem_tid.unwrap_or(T_DYNAMIC));
            }
        }
        PatternKind::ArrayHeadTail { head, rest } => {
            let elem_tid = match subj_tid {
                T_ARRAY_I32 | T_LIST_I32 => Some(T_I32),
                T_ARRAY_BYTES | T_LIST_BYTES => Some(T_BYTES),
                _ => None,
            };
            bind_pattern_typed(tc, head, elem_tid.unwrap_or(T_DYNAMIC));
            tc.bind_local(rest, subj_tid);
        }
        PatternKind::ArrayPrefixRest { prefix, rest } => {
            let elem_tid = match subj_tid {
                T_ARRAY_I32 | T_LIST_I32 => Some(T_I32),
                T_ARRAY_BYTES | T_LIST_BYTES => Some(T_BYTES),
                _ => None,
            };
            for el in prefix {
                bind_pattern_typed(tc, el, elem_tid.unwrap_or(T_DYNAMIC));
            }
            tc.bind_local(rest, subj_tid);
        }
        PatternKind::Wildcard
        | PatternKind::BoolLit(_)
        | PatternKind::I8Lit(_)
        | PatternKind::I16Lit(_)
        | PatternKind::I32Lit(_)
        | PatternKind::Pin(_) => {}
    }
}
