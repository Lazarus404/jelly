/**
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

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

/// Typecheck `with` expression: clauses, body, and optional else arms.
pub(super) fn type_with(
    tc: &mut TypeChecker,
    e: &Expr,
    clauses: &[(Pattern, Expr)],
    body: &Expr,
    else_arms: &Option<Vec<MatchArm>>,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let body_t = tc.with_scope(|tc| {
        for (pat, expr) in clauses {
            let expr_t = tc.check_expr(expr, None)?;
            bind_pattern_typed(tc, pat, expr_t);
        }
        tc.check_expr(body, expect)
    })?;
    if let Some(arms) = else_arms {
        if arms.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "with else must have at least one arm",
            ));
        }
        if !matches!(arms.last().unwrap().pat.node, PatternKind::Wildcard) {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "with else must be exhaustive (last arm must be '_')",
            ));
        }
        if arms.last().unwrap().when.is_some() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "last '_' arm in with else cannot have a when-guard",
            ));
        }
        let mut else_t: Option<TypeId> = None;
        for a in arms {
            let t = check_arm(tc, a, T_DYNAMIC, Some(body_t))?;
            let Some(t) = t else { continue };
            if let Some(prev) = else_t {
                if t != prev {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "with else arms must have same type",
                    ));
                }
            } else {
                else_t = Some(t);
            }
        }
        let else_t = else_t.unwrap_or(body_t);
        if body_t != else_t {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "with body and else must have same type",
            ));
        }
    }
    Ok(body_t)
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

pub(super) fn bind_pattern_typed(tc: &mut TypeChecker, p: &Pattern, subj_tid: TypeId) {
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
