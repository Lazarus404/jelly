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

use std::collections::HashMap;

use crate::ast::{ExprKind, Stmt, StmtKind};
use crate::error::CompileError;
use crate::typectx::{T_BYTES, T_I32};

use super::super::dsu::{unify, ITy};
use super::Infer;

impl<'a> Infer<'a> {
    pub(super) fn infer_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
        match &s.node {
            StmtKind::Let { name, ty, expr, .. } => {
                let et = if let Some(t) = ty {
                    Some(self.resolve_ty_ann(t)?)
                } else {
                    None
                };
                let mut t = self.infer_expr(expr)?;
                // `fn_infer` is a lightweight pass used only to infer the outer function's
                // signature. Nested function literals are treated as `Dynamic` by `infer_expr`
                // (we don't infer their signatures here). When there *is* an explicit
                // annotation on the binding, trust it to avoid spurious conflicts.
                if let (Some(et), ExprKind::Fn { .. }) = (et, &expr.node) {
                    let is_fun = self
                        .type_ctx
                        .types
                        .get(et as usize)
                        .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Function);
                    if is_fun {
                        t = ITy::Known(et);
                    }
                }
                let t = if let Some(et) = et {
                    unify(&mut self.dsu, t, ITy::Known(et))?
                } else {
                    t
                };
                self.bind(name, t);
                Ok(())
            }
            StmtKind::Assign { name, expr } => {
                if let Some(dst) = self.lookup(name) {
                    let mut t = self.infer_expr(expr)?;
                    // If assigning a `fn {..}` to a variable that is already known to be a
                    // function type, trust the destination type (this pass doesn't infer
                    // nested function signatures).
                    if let ExprKind::Fn { .. } = &expr.node {
                        if let Some(dst_tid) = self.resolve_known_tid(dst) {
                            let is_fun = self
                                .type_ctx
                                .types
                                .get(dst_tid as usize)
                                .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Function);
                            if is_fun {
                                t = ITy::Known(dst_tid);
                            }
                        }
                    }
                    let _ = unify(&mut self.dsu, dst, t)?;
                }
                Ok(())
            }
            StmtKind::Return { expr } => {
                if let Some(e) = expr {
                    let mut t = self.infer_expr(e)?;
                    // If returning a `fn {..}` and the return type is already constrained to
                    // a function type, trust the constrained return type.
                    if let ExprKind::Fn { .. } = &e.node {
                        let cur = self.resolve_known_tid(ITy::Var(self.ret_var));
                        if let Some(ret_tid) = cur {
                            let is_fun = self
                                .type_ctx
                                .types
                                .get(ret_tid as usize)
                                .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Function);
                            if is_fun {
                                t = ITy::Known(ret_tid);
                            }
                        }
                    }
                    let _ = unify(&mut self.dsu, ITy::Var(self.ret_var), t)?;
                }
                Ok(())
            }
            StmtKind::Expr { expr } => {
                let _ = self.infer_expr(expr)?;
                Ok(())
            }
            StmtKind::While { cond, body } => {
                let tc = self.infer_expr(cond)?;
                // Jelly truthiness: loop conditions need not be `Bool`.
                let _ = tc;
                self.scopes.push(HashMap::new());
                for st in body {
                    self.infer_stmt(st)?;
                }
                self.scopes.pop();
                Ok(())
            }
            StmtKind::Break | StmtKind::Continue => Ok(()),
            StmtKind::Throw { expr } => {
                let _ = self.infer_expr(expr)?;
                Ok(())
            }
            StmtKind::MemberAssign { base, expr, .. } => {
                let _ = self.infer_expr(base)?;
                let _ = self.infer_expr(expr)?;
                Ok(())
            }
            StmtKind::IndexAssign { base, index, expr } => {
                let tb = self.infer_expr(base)?;
                let ti = self.infer_expr(index)?;
                self.require_int_index(ti, index.span)?;
                let te = self.infer_expr(expr)?;
                if let Some(base_tid) = self.resolve_known_tid(tb) {
                    match base_tid {
                        crate::typectx::T_ARRAY_I32 => {
                            let _ = unify(&mut self.dsu, te, ITy::Known(T_I32))?;
                        }
                        crate::typectx::T_ARRAY_BYTES => {
                            let _ = unify(&mut self.dsu, te, ITy::Known(T_BYTES))?;
                        }
                        T_BYTES => {
                            let _ = unify(&mut self.dsu, te, ITy::Known(T_I32))?;
                        }
                        _ => {}
                    }
                }
                Ok(())
            }
            StmtKind::ImportModule { .. }
            | StmtKind::ImportFrom { .. }
            | StmtKind::Prototype { .. } => Ok(()),
        }
    }
}
