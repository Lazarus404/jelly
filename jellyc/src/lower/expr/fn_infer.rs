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

// Minimal local type inference for `let f = fn(...) { ... }`.
//
// This is intentionally conservative: we infer only from the function body and any
// parameter annotations. Unconstrained params/return default to `Dynamic`.
//
// This is enough to support common recursive patterns like:
// `let ack = fn(x, y) { ... ack(x - 1, ack(x, y - 1)) ... };`

use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Stmt, StmtKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::lower::LowerCtx;
use crate::typectx::{
    T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8,
};

#[derive(Clone, Copy, Debug)]
enum ITy {
    Known(TypeId),
    Var(usize),
}

#[derive(Debug)]
struct Dsu {
    parent: Vec<usize>,
    value: Vec<Option<TypeId>>,
    numeric_hint: Vec<bool>,
}

impl Dsu {
    fn new(n: usize) -> Self {
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

    fn union(&mut self, a: usize, b: usize) -> Result<usize, CompileError> {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return Ok(ra);
        }
        // Attach rb -> ra
        self.parent[rb] = ra;
        self.numeric_hint[ra] |= self.numeric_hint[rb];
        match (self.value[ra], self.value[rb]) {
            (Some(x), Some(y)) if x != y => Err(CompileError::new(
                ErrorKind::Type,
                crate::ast::Span::point(0),
                "inferred type conflict",
            )),
            (None, Some(y)) => {
                self.value[ra] = Some(y);
                Ok(ra)
            }
            _ => Ok(ra),
        }
    }

    fn constrain(&mut self, v: usize, tid: TypeId) -> Result<(), CompileError> {
        let r = self.find(v);
        if let Some(cur) = self.value[r] {
            if cur != tid {
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

    fn mark_numeric(&mut self, v: usize) {
        let r = self.find(v);
        self.numeric_hint[r] = true;
    }

    fn resolve_or_default(&mut self, v: usize) -> TypeId {
        let r = self.find(v);
        if let Some(t) = self.value[r] {
            return t;
        }
        // If it was used in a numeric context but never concretely constrained, default to I32.
        if self.numeric_hint[r] {
            return T_I32;
        }
        T_DYNAMIC
    }
}

fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

fn unify(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    match (a, b) {
        (ITy::Known(x), ITy::Known(y)) => {
            if x != y {
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

fn infer_numeric_bin(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    // Try to push a concrete numeric type across the operator.
    match (a, b) {
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
            // Leave unconstrained for now; may be fixed by later constraints.
            let _ = dsu.union(va, vb)?;
            Ok(ITy::Var(dsu.find(va)))
        }
        (ITy::Known(ta), ITy::Known(tb)) => {
            if !is_numeric(ta) || !is_numeric(tb) || ta != tb {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "numeric operator expects same numeric types",
                ));
            }
            Ok(ITy::Known(ta))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            crate::ast::Span::point(0),
            "numeric operator expects numeric operands",
        )),
    }
}

struct Infer<'a> {
    ctx: &'a mut LowerCtx,
    self_name: &'a str,
    param_names: Vec<String>,
    param_vars: Vec<usize>,
    ret_var: usize,
    dsu: Dsu,
    scopes: Vec<HashMap<String, ITy>>,
}

impl<'a> Infer<'a> {
    fn new(ctx: &'a mut LowerCtx, self_name: &'a str, nparams: usize) -> Self {
        // Vars: 0..nparams for params, plus one for return.
        let ret_var = nparams;
        let mut scopes = Vec::new();
        scopes.push(HashMap::new());
        Self {
            ctx,
            self_name,
            param_names: Vec::with_capacity(nparams),
            param_vars: (0..nparams).collect(),
            ret_var,
            dsu: Dsu::new(nparams + 1),
            scopes,
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
        None
    }

    fn resolve_ty_ann(&mut self, t: &Ty) -> Result<TypeId, CompileError> {
        self.ctx.type_ctx.resolve_ty(t)
    }

    fn infer_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
        match &s.node {
            StmtKind::Let { name, ty, expr, .. } => {
                let et = if let Some(t) = ty {
                    Some(self.resolve_ty_ann(t)?)
                } else {
                    None
                };
                let t = self.infer_expr(expr)?;
                let t = if let Some(et) = et { unify(&mut self.dsu, t, ITy::Known(et))? } else { t };
                self.bind(name, t);
                Ok(())
            }
            StmtKind::Assign { name, expr } => {
                if let Some(dst) = self.lookup(name) {
                    let t = self.infer_expr(expr)?;
                    let _ = unify(&mut self.dsu, dst, t)?;
                }
                Ok(())
            }
            StmtKind::Return { expr } => {
                if let Some(e) = expr {
                    let t = self.infer_expr(e)?;
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
                let _ = unify(&mut self.dsu, tc, ITy::Known(T_BOOL))?;
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
                let _ = self.infer_expr(base)?;
                let _ = self.infer_expr(index)?;
                let _ = self.infer_expr(expr)?;
                Ok(())
            }
            StmtKind::ImportModule { .. }
            | StmtKind::ImportFrom { .. }
            | StmtKind::Prototype { .. } => Ok(()),
        }
    }

    fn infer_expr(&mut self, e: &Expr) -> Result<ITy, CompileError> {
        match &e.node {
            ExprKind::I32Lit(_) => Ok(ITy::Known(T_I32)),
            ExprKind::I8Lit(_) => Ok(ITy::Known(T_I8)),
            ExprKind::I16Lit(_) => Ok(ITy::Known(T_I16)),
            ExprKind::I64Lit(_) => Ok(ITy::Known(T_I64)),
            ExprKind::F16Lit(_) => Ok(ITy::Known(T_F16)),
            ExprKind::F64Lit(_) => Ok(ITy::Known(T_F64)),
            ExprKind::BoolLit(_) => Ok(ITy::Known(T_BOOL)),
            ExprKind::BytesLit(_) => Ok(ITy::Known(T_BYTES)),
            ExprKind::AtomLit(_) => Ok(ITy::Known(T_ATOM)),
            ExprKind::Null => Ok(ITy::Known(T_DYNAMIC)),
            ExprKind::Var(n) => Ok(self.lookup(n).unwrap_or(ITy::Known(T_DYNAMIC))),
            ExprKind::Neg(x) => {
                let t = self.infer_expr(x)?;
                // Mark var as numeric if needed.
                if let ITy::Var(v) = t {
                    self.dsu.mark_numeric(v);
                }
                Ok(t)
            }
            ExprKind::Not(x) => {
                let t = self.infer_expr(x)?;
                let _ = unify(&mut self.dsu, t, ITy::Known(T_BOOL))?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                infer_numeric_bin(&mut self.dsu, ta, tb)
            }
            ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                // Equality/comparison: unify operand types if possible, allow numeric bin fallback.
                let _ = match (ta, tb) {
                    (ITy::Known(x), ITy::Known(y)) if x == y => Ok(ITy::Known(x)),
                    _ => infer_numeric_bin(&mut self.dsu, ta, tb),
                }?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::And(a, b) | ExprKind::Or(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                let _ = unify(&mut self.dsu, ta, ITy::Known(T_BOOL))?;
                let _ = unify(&mut self.dsu, tb, ITy::Known(T_BOOL))?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::If { cond, then_br, else_br } => {
                let tc = self.infer_expr(cond)?;
                let _ = unify(&mut self.dsu, tc, ITy::Known(T_BOOL))?;
                let tt = self.infer_expr(then_br)?;
                let te = self.infer_expr(else_br)?;
                unify(&mut self.dsu, tt, te)
            }
            ExprKind::Block { stmts, expr } => {
                self.scopes.push(HashMap::new());
                for s in stmts {
                    self.infer_stmt(s)?;
                }
                let t = self.infer_expr(expr)?;
                self.scopes.pop();
                Ok(t)
            }
            ExprKind::Call { callee, args, .. } => {
                // Special-case self recursion: `ack(...)`
                if let ExprKind::Var(n) = &callee.node {
                    if n == self.self_name {
                        if args.len() != self.param_vars.len() {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "call arity mismatch in inferred recursive call",
                            ));
                        }
                        for (i, a) in args.iter().enumerate() {
                            let ta = self.infer_expr(a)?;
                            let _ = unify(&mut self.dsu, ITy::Var(self.param_vars[i]), ta)?;
                        }
                        return Ok(ITy::Var(self.ret_var));
                    }
                }
                // Otherwise, we don't attempt to infer the signature of arbitrary callees.
                Ok(ITy::Known(T_DYNAMIC))
            }
            // Keep these conservative for now.
            ExprKind::Member { .. }
            | ExprKind::TypeApp { .. }
            | ExprKind::ArrayLit(_)
            | ExprKind::TupleLit(_)
            | ExprKind::ObjLit(_)
            | ExprKind::Index { .. }
            | ExprKind::Fn { .. }
            | ExprKind::Try { .. }
            | ExprKind::Match { .. }
            | ExprKind::New { .. } => Ok(ITy::Known(T_DYNAMIC)),
        }
    }
}

pub fn infer_fn_type_for_let(
    self_name: &str,
    params: &[(String, Option<Ty>)],
    body: &[Stmt],
    tail: &Option<Box<Expr>>,
    ctx: &mut LowerCtx,
) -> Result<(TypeId, Vec<TypeId>, TypeId), CompileError> {
    let mut inf = Infer::new(ctx, self_name, params.len());

    // Bind params
    for (i, (name, ann)) in params.iter().enumerate() {
        inf.param_names.push(name.clone());
        inf.bind(name, ITy::Var(inf.param_vars[i]));
        if let Some(t) = ann {
            let tid = inf.resolve_ty_ann(t)?;
            inf.dsu.constrain(inf.param_vars[i], tid)?;
        }
    }

    // Infer from body + tail
    for s in body {
        inf.infer_stmt(s)?;
    }
    if let Some(t) = tail {
        let tt = inf.infer_expr(t)?;
        let _ = unify(&mut inf.dsu, ITy::Var(inf.ret_var), tt)?;
    }

    let arg_tids: Vec<TypeId> = inf
        .param_vars
        .iter()
        .map(|&v| inf.dsu.resolve_or_default(v))
        .collect();
    let ret_tid = inf.dsu.resolve_or_default(inf.ret_var);

    let sig_id = inf.ctx.type_ctx.intern_sig(ret_tid, &arg_tids);
    let fun_tid = inf.ctx.type_ctx.intern_fun_type(ret_tid, &arg_tids);
    // Sanity: sig_id should match fun_tid's sig, but we don't need to re-check here.
    let _ = sig_id;
    Ok((fun_tid, arg_tids, ret_tid))
}

