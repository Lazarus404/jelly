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

use std::collections::HashMap;

use crate::ast::{Expr, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{TypeCtx, T_DYNAMIC, T_I16, T_I32, T_I64, T_I8};

use super::dsu::{unify, Dsu, ITy};

mod expr;
mod stmt;

struct Infer<'a> {
    type_ctx: &'a mut TypeCtx,
    self_name: &'a str,
    lookup_outer: &'a dyn Fn(&str) -> Option<TypeId>,
    lookup_module_export: &'a dyn Fn(&str, &str) -> Option<TypeId>,
    param_vars: Vec<usize>,
    ret_var: usize,
    dsu: Dsu,
    index_int_constraints: Vec<(usize, crate::ast::Span)>,
    scopes: Vec<HashMap<String, ITy>>,
}

impl<'a> Infer<'a> {
    fn new(
        type_ctx: &'a mut TypeCtx,
        self_name: &'a str,
        nparams: usize,
        lookup_outer: &'a dyn Fn(&str) -> Option<TypeId>,
        lookup_module_export: &'a dyn Fn(&str, &str) -> Option<TypeId>,
    ) -> Self {
        Self {
            type_ctx,
            self_name,
            lookup_outer,
            lookup_module_export,
            param_vars: (0..nparams).collect(),
            ret_var: nparams,
            dsu: Dsu::new(nparams + 1),
            index_int_constraints: Vec::new(),
            scopes: vec![HashMap::new()],
        }
    }

    fn bind(&mut self, name: &str, ty: ITy) {
        self.scopes
            .last_mut()
            .expect("scope")
            .insert(name.to_string(), ty);
    }

    fn lookup(&self, name: &str) -> Option<ITy> {
        for s in self.scopes.iter().rev() {
            if let Some(t) = s.get(name) {
                return Some(*t);
            }
        }
        (self.lookup_outer)(name).map(ITy::Known)
    }

    fn resolve_ty_ann(&mut self, t: &Ty) -> Result<TypeId, CompileError> {
        self.type_ctx.resolve_ty(t)
    }

    fn resolve_known_tid(&mut self, t: ITy) -> Option<TypeId> {
        match t {
            ITy::Known(tid) => Some(tid),
            ITy::Var(v) => self.dsu.resolve_known(v),
        }
    }

    fn require_int_index(&mut self, t: ITy, span: crate::ast::Span) -> Result<(), CompileError> {
        fn ok_index_tid(tid: TypeId) -> bool {
            matches!(tid, T_DYNAMIC | T_I8 | T_I16 | T_I32 | T_I64)
        }
        if let Some(tid) = self.resolve_known_tid(t) {
            if !ok_index_tid(tid) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "index must be an integer",
                ));
            }
            return Ok(());
        }
        if let ITy::Var(v) = t {
            self.index_int_constraints.push((v, span));
        }
        Ok(())
    }

    fn check_int_index_constraints(&mut self) -> Result<(), CompileError> {
        for (v, span) in std::mem::take(&mut self.index_int_constraints) {
            let tid = self.dsu.resolve_or_default(v);
            if !matches!(tid, T_DYNAMIC | T_I8 | T_I16 | T_I32 | T_I64) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "index must be an integer",
                ));
            }
        }
        Ok(())
    }
}

pub fn infer_fn_type_for_let(
    self_name: &str,
    params: &[(String, Option<Ty>)],
    body: &[Stmt],
    tail: &Option<Box<Expr>>,
    type_ctx: &mut TypeCtx,
    lookup_outer: &dyn Fn(&str) -> Option<TypeId>,
    lookup_module_export: &dyn Fn(&str, &str) -> Option<TypeId>,
) -> Result<(TypeId, Vec<TypeId>, TypeId), CompileError> {
    let mut inf = Infer::new(
        type_ctx,
        self_name,
        params.len(),
        lookup_outer,
        lookup_module_export,
    );
    for (i, (name, ann)) in params.iter().enumerate() {
        inf.bind(name, ITy::Var(inf.param_vars[i]));
        if let Some(t) = ann {
            let tid = inf.resolve_ty_ann(t)?;
            inf.dsu.constrain(inf.param_vars[i], tid)?;
        }
    }
    for s in body {
        inf.infer_stmt(s)?;
    }
    if let Some(t) = tail {
        let tt = inf.infer_expr(t)?;
        let _ = unify(&mut inf.dsu, ITy::Var(inf.ret_var), tt)?;
    }
    inf.check_int_index_constraints()?;

    let arg_tids: Vec<TypeId> = inf
        .param_vars
        .iter()
        .map(|&v| inf.dsu.resolve_or_default(v))
        .collect();
    let ret_tid = inf.dsu.resolve_or_default(inf.ret_var);
    let fun_tid = inf.type_ctx.intern_fun_type(ret_tid, &arg_tids);
    Ok((fun_tid, arg_tids, ret_tid))
}
