/*
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

use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

#[derive(Clone, Copy, Debug)]
pub(super) enum ITy {
    Known(TypeId),
    Var(usize),
}

#[derive(Debug)]
pub(super) struct Dsu {
    parent: Vec<usize>,
    value: Vec<Option<TypeId>>,
    numeric_hint: Vec<bool>,
}

impl Dsu {
    pub(super) fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            value: vec![None; n],
            numeric_hint: vec![false; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        let p = self.parent[x];
        if p == x {
            return x;
        }
        let r = self.find(p);
        self.parent[x] = r;
        r
    }

    pub(super) fn union(&mut self, a: usize, b: usize) -> Result<usize, CompileError> {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return Ok(ra);
        }
        self.parent[rb] = ra;
        self.numeric_hint[ra] |= self.numeric_hint[rb];
        match (self.value[ra], self.value[rb]) {
            (Some(x), Some(y)) if x != y => {
                if is_numeric(x) && is_numeric(y) {
                    self.value[ra] = Some(join_numeric(x, y));
                    Ok(ra)
                } else if x == T_DYNAMIC || y == T_DYNAMIC {
                    // Dynamic unifies with anything; prefer the more precise type.
                    self.value[ra] = Some(if x == T_DYNAMIC { y } else { x });
                    Ok(ra)
                } else {
                    Err(CompileError::new(
                        ErrorKind::Type,
                        crate::ast::Span::point(0),
                        "inferred type conflict",
                    ))
                }
            }
            (None, Some(y)) => {
                self.value[ra] = Some(y);
                Ok(ra)
            }
            _ => Ok(ra),
        }
    }

    pub(super) fn constrain(&mut self, v: usize, tid: TypeId) -> Result<(), CompileError> {
        let r = self.find(v);
        if let Some(cur) = self.value[r] {
            if cur != tid {
                if is_numeric(cur) && is_numeric(tid) {
                    self.value[r] = Some(join_numeric(cur, tid));
                    return Ok(());
                }
                if cur == T_DYNAMIC || tid == T_DYNAMIC {
                    self.value[r] = Some(if cur == T_DYNAMIC { tid } else { cur });
                    return Ok(());
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "inferred type conflict",
                ));
            }
        } else {
            self.value[r] = Some(tid);
        }
        Ok(())
    }

    pub(super) fn mark_numeric(&mut self, v: usize) {
        let r = self.find(v);
        self.numeric_hint[r] = true;
    }

    pub(super) fn resolve_or_default(&mut self, v: usize) -> TypeId {
        let r = self.find(v);
        if let Some(t) = self.value[r] {
            return t;
        }
        if self.numeric_hint[r] {
            return T_I32;
        }
        T_DYNAMIC
    }

    pub(super) fn resolve_known(&mut self, v: usize) -> Option<TypeId> {
        let r = self.find(v);
        self.value[r]
    }
}

pub(super) fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

fn numeric_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        T_F16 => 4,
        T_F32 => 5,
        T_F64 => 6,
        _ => 255,
    }
}

pub(super) fn join_numeric(a: TypeId, b: TypeId) -> TypeId {
    if numeric_rank(a) >= numeric_rank(b) {
        a
    } else {
        b
    }
}

pub(super) fn unify(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    match (a, b) {
        (ITy::Known(x), ITy::Known(y)) => {
            if x != y {
                if is_numeric(x) && is_numeric(y) {
                    return Ok(ITy::Known(join_numeric(x, y)));
                }
                if x == T_DYNAMIC || y == T_DYNAMIC {
                    return Ok(ITy::Known(if x == T_DYNAMIC { y } else { x }));
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "inferred type conflict",
                ));
            }
            Ok(ITy::Known(x))
        }
        (ITy::Var(v), ITy::Known(t)) | (ITy::Known(t), ITy::Var(v)) => {
            dsu.constrain(v, t)?;
            Ok(ITy::Known(t))
        }
        (ITy::Var(a), ITy::Var(b)) => {
            let r = dsu.union(a, b)?;
            Ok(ITy::Var(r))
        }
    }
}

pub(super) fn infer_numeric_bin(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    match (a, b) {
        (ITy::Known(ta), ITy::Known(tb)) => {
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "numeric operator expects numeric operands",
                ));
            }
            Ok(ITy::Known(join_numeric(ta, tb)))
        }
        (ITy::Known(ta), ITy::Var(vb)) if is_numeric(ta) => {
            dsu.mark_numeric(vb);
            dsu.constrain(vb, ta)?;
            Ok(ITy::Known(ta))
        }
        (ITy::Var(va), ITy::Known(tb)) if is_numeric(tb) => {
            dsu.mark_numeric(va);
            dsu.constrain(va, tb)?;
            Ok(ITy::Known(tb))
        }
        (ITy::Var(va), ITy::Var(vb)) => {
            dsu.mark_numeric(va);
            dsu.mark_numeric(vb);
            let r = dsu.union(va, vb)?;
            Ok(ITy::Var(r))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            crate::ast::Span::point(0),
            "numeric operator expects numeric operands",
        )),
    }
}

/// Infer Add: Bytes concatenation or numeric addition.
pub(super) fn infer_add_bin(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    // Bytes concatenation: when at least one operand is known Bytes.
    match (a, b) {
        (ITy::Known(T_BYTES), ITy::Known(T_BYTES)) => return Ok(ITy::Known(T_BYTES)),
        (ITy::Known(T_BYTES), ITy::Var(v)) => {
            dsu.constrain(v, T_BYTES)?;
            return Ok(ITy::Known(T_BYTES));
        }
        (ITy::Var(v), ITy::Known(T_BYTES)) => {
            dsu.constrain(v, T_BYTES)?;
            return Ok(ITy::Known(T_BYTES));
        }
        _ => {}
    }
    infer_numeric_bin(dsu, a, b)
}
