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
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16,
    T_I32, T_I64, T_I8, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
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
            (Some(x), Some(y)) if x != y => {
                if is_numeric(x) && is_numeric(y) {
                    self.value[ra] = Some(join_numeric(x, y).expect("numeric join"));
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

    fn constrain(&mut self, v: usize, tid: TypeId) -> Result<(), CompileError> {
        let r = self.find(v);
        if let Some(cur) = self.value[r] {
            if cur != tid {
                if is_numeric(cur) && is_numeric(tid) {
                    self.value[r] = Some(join_numeric(cur, tid).expect("numeric join"));
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

fn join_numeric(a: TypeId, b: TypeId) -> Option<TypeId> {
    if !is_numeric(a) || !is_numeric(b) {
        return None;
    }
    if numeric_rank(a) >= numeric_rank(b) {
        Some(a)
    } else {
        Some(b)
    }
}

// Unify two types.
// Both types known, they are unified if they are the same.
// Both types unknown, they are unified if they are the same.
// Known and unknown respectively, the unknown type is constrained to the known type.
// Unknown and known respectively, the known type is constrained to the unknown type.
// Needs further consideration in the future.
fn unify(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    match (a, b) {
        (ITy::Known(x), ITy::Known(y)) => {
            if x != y {
                if is_numeric(x) && is_numeric(y) {
                    return Ok(ITy::Known(join_numeric(x, y).expect("numeric join")));
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

// Infer the type of a numeric binary operator.
fn infer_numeric_bin(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    // Numeric operators promote/join numeric types (widening only).
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
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "numeric operator expects numeric operands",
                ));
            }
            Ok(ITy::Known(join_numeric(ta, tb).expect("numeric join")))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            crate::ast::Span::point(0),
            "numeric operator expects numeric operands",
        )),
    }
}

// Get the namespace and name of a builtin function.
fn builtin_name(callee: &Expr) -> Option<(&str, &str)> {
    match &callee.node {
        ExprKind::Var(n) => Some(("", n.as_str())),
        ExprKind::Member { base, name } => match &base.node {
            ExprKind::Var(ns) => Some((ns.as_str(), name.as_str())),
            _ => None,
        },
        _ => None,
    }
}

// Infer the type of a function defined by a `let f = fn(...) { ... }` binding.
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
        // Fall back to any already-typed outer binding available during lowering.
        self.ctx
            .env_stack
            .iter()
            .rev()
            .find_map(|m| m.get(name))
            .map(|bd| ITy::Known(bd.tid))
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
            ExprKind::Add(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                // `bytes + bytes -> bytes` concatenation.
                if matches!(ta, ITy::Known(T_BYTES)) || matches!(tb, ITy::Known(T_BYTES)) {
                    let _ = unify(&mut self.dsu, ta, ITy::Known(T_BYTES))?;
                    let _ = unify(&mut self.dsu, tb, ITy::Known(T_BYTES))?;
                    Ok(ITy::Known(T_BYTES))
                } else {
                    infer_numeric_bin(&mut self.dsu, ta, tb)
                }
            }
            ExprKind::Sub(a, b) | ExprKind::Mul(a, b) | ExprKind::Div(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                infer_numeric_bin(&mut self.dsu, ta, tb)
            }
            ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                // Equality:
                // - if both are numeric, allow mixed types (value-based) and join/promote
                // - otherwise require/unify to the same type
                let _ = match (ta, tb) {
                    (ITy::Known(x), ITy::Known(y)) if is_numeric(x) && is_numeric(y) => {
                        Ok(ITy::Known(join_numeric(x, y).expect("numeric join")))
                    }
                    (x, y) => unify(&mut self.dsu, x, y),
                }?;
                Ok(ITy::Known(T_BOOL))
            }
            ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b) => {
                let ta = self.infer_expr(a)?;
                let tb = self.infer_expr(b)?;
                let _ = infer_numeric_bin(&mut self.dsu, ta, tb)?;
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
            ExprKind::Call {
                callee,
                type_args,
                args,
            } => {
                // Builtins (best-effort inference mirroring call lowering).
                if let Some((ns, name)) = builtin_name(callee) {
                    match (ns, name) {
                        ("System", "assert") => {
                            if args.len() == 1 {
                                let t0 = self.infer_expr(&args[0])?;
                                let _ = unify(&mut self.dsu, t0, ITy::Known(T_BOOL))?;
                                return Ok(ITy::Known(T_BOOL));
                            }
                        }
                        ("Math", "sqrt") => {
                            if args.len() == 1 {
                                let t0 = self.infer_expr(&args[0])?;
                                let _ = infer_numeric_bin(&mut self.dsu, t0, ITy::Known(T_F64))?;
                                return Ok(ITy::Known(T_F64));
                            }
                        }
                        ("Integer", "to_i8") => return self.infer_unary_numeric_cast(args, T_I8),
                        ("Integer", "to_i16") => return self.infer_unary_numeric_cast(args, T_I16),
                        ("Integer", "to_i32") => return self.infer_unary_numeric_cast(args, T_I32),
                        ("Integer", "to_i64") => return self.infer_unary_numeric_cast(args, T_I64),
                        ("Float", "to_f16") => return self.infer_unary_numeric_cast(args, T_F16),
                        ("Float", "to_f32") => return self.infer_unary_numeric_cast(args, T_F32),
                        ("Float", "to_f64") => return self.infer_unary_numeric_cast(args, T_F64),
                        ("Bytes", "new") => {
                            if args.len() == 1 {
                                let tlen = self.infer_expr(&args[0])?;
                                let _ = unify(&mut self.dsu, tlen, ITy::Known(T_I32))?;
                                return Ok(ITy::Known(T_BYTES));
                            }
                        }
                        ("Bytes", "len") => {
                            if args.len() == 1 {
                                let tb = self.infer_expr(&args[0])?;
                                let _ = unify(&mut self.dsu, tb, ITy::Known(T_BYTES))?;
                                return Ok(ITy::Known(T_I32));
                            }
                        }
                        ("Bytes", "get_u8") => {
                            if args.len() == 2 {
                                let tb = self.infer_expr(&args[0])?;
                                let ti = self.infer_expr(&args[1])?;
                                let _ = unify(&mut self.dsu, tb, ITy::Known(T_BYTES))?;
                                let _ = unify(&mut self.dsu, ti, ITy::Known(T_I32))?;
                                return Ok(ITy::Known(T_I32));
                            }
                        }
                        ("Bytes", "set_u8") => {
                            if args.len() == 3 {
                                let tb = self.infer_expr(&args[0])?;
                                let ti = self.infer_expr(&args[1])?;
                                let tv = self.infer_expr(&args[2])?;
                                let _ = unify(&mut self.dsu, tb, ITy::Known(T_BYTES))?;
                                let _ = unify(&mut self.dsu, ti, ITy::Known(T_I32))?;
                                let _ = unify(&mut self.dsu, tv, ITy::Known(T_I32))?;
                                return Ok(ITy::Known(T_BYTES));
                            }
                        }
                        ("Atom", "intern") => {
                            if args.len() == 1 {
                                let ts = self.infer_expr(&args[0])?;
                                let _ = unify(&mut self.dsu, ts, ITy::Known(T_BYTES))?;
                                return Ok(ITy::Known(T_ATOM));
                            }
                        }
                        ("Object", "get") => {
                            if args.len() == 2 {
                                let to = self.infer_expr(&args[0])?;
                                let tk = self.infer_expr(&args[1])?;
                                let _ = unify(&mut self.dsu, to, ITy::Known(T_OBJECT))?;
                                let _ = unify(&mut self.dsu, tk, ITy::Known(T_ATOM))?;
                                if type_args.len() == 1 {
                                    let out = self.ctx.type_ctx.resolve_ty(&type_args[0])?;
                                    return Ok(ITy::Known(out));
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("Object", "set") => {
                            if args.len() == 3 {
                                let to = self.infer_expr(&args[0])?;
                                let tk = self.infer_expr(&args[1])?;
                                let _ = self.infer_expr(&args[2])?;
                                let _ = unify(&mut self.dsu, to, ITy::Known(T_OBJECT))?;
                                let _ = unify(&mut self.dsu, tk, ITy::Known(T_ATOM))?;
                                return Ok(ITy::Known(T_OBJECT));
                            }
                        }
                        ("Array", "new") => {
                            if args.len() == 1 {
                                let tlen = self.infer_expr(&args[0])?;
                                let _ = unify(&mut self.dsu, tlen, ITy::Known(T_I32))?;
                                // If explicit type arg is present, pick concrete array type.
                                if type_args.len() == 1 {
                                    let elem = self.ctx.type_ctx.resolve_ty(&type_args[0])?;
                                    let out = match elem {
                                        T_I32 => T_ARRAY_I32,
                                        T_BYTES => T_ARRAY_BYTES,
                                        _ => T_DYNAMIC,
                                    };
                                    return Ok(ITy::Known(out));
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("Array", "get") => {
                            if args.len() == 2 {
                                let ta = self.infer_expr(&args[0])?;
                                let ti = self.infer_expr(&args[1])?;
                                let _ = unify(&mut self.dsu, ti, ITy::Known(T_I32))?;
                                if let ITy::Known(arr_tid) = ta {
                                    return Ok(ITy::Known(match arr_tid {
                                        T_ARRAY_I32 => T_I32,
                                        T_ARRAY_BYTES => T_BYTES,
                                        _ => T_DYNAMIC,
                                    }));
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("Array", "set") => {
                            if args.len() == 3 {
                                let ta = self.infer_expr(&args[0])?;
                                let ti = self.infer_expr(&args[1])?;
                                let tv = self.infer_expr(&args[2])?;
                                let _ = unify(&mut self.dsu, ti, ITy::Known(T_I32))?;
                                if let ITy::Known(arr_tid) = ta {
                                    let want = match arr_tid {
                                        T_ARRAY_I32 => Some(T_I32),
                                        T_ARRAY_BYTES => Some(T_BYTES),
                                        _ => None,
                                    };
                                    if let Some(w) = want {
                                        let _ = unify(&mut self.dsu, tv, ITy::Known(w))?;
                                    }
                                    return Ok(ITy::Known(arr_tid));
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("Array", "len") => {
                            if args.len() == 1 {
                                let _ = self.infer_expr(&args[0])?;
                                return Ok(ITy::Known(T_I32));
                            }
                        }
                        ("List", "nil") => {
                            if args.is_empty() {
                                if type_args.len() == 1 {
                                    let elem = self.ctx.type_ctx.resolve_ty(&type_args[0])?;
                                    return Ok(ITy::Known(match elem {
                                        T_I32 => T_LIST_I32,
                                        T_BYTES => T_LIST_BYTES,
                                        _ => T_DYNAMIC,
                                    }));
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("List", "cons") => {
                            if args.len() == 2 {
                                let th = self.infer_expr(&args[0])?;
                                let tt = self.infer_expr(&args[1])?;
                                // If explicit type arg is present, enforce element + tail list type.
                                if type_args.len() == 1 {
                                    let elem = self.ctx.type_ctx.resolve_ty(&type_args[0])?;
                                    let list_tid = match elem {
                                        T_I32 => T_LIST_I32,
                                        T_BYTES => T_LIST_BYTES,
                                        _ => T_DYNAMIC,
                                    };
                                    if list_tid != T_DYNAMIC {
                                        let _ = unify(&mut self.dsu, tt, ITy::Known(list_tid))?;
                                        let _ = unify(&mut self.dsu, th, ITy::Known(elem))?;
                                        return Ok(ITy::Known(list_tid));
                                    }
                                }
                                let _ = th;
                                let _ = tt;
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("List", "head") => {
                            if args.len() == 1 {
                                let tl = self.infer_expr(&args[0])?;
                                if let ITy::Known(list_tid) = tl {
                                    return Ok(ITy::Known(match list_tid {
                                        T_LIST_I32 => T_I32,
                                        T_LIST_BYTES => T_BYTES,
                                        _ => T_DYNAMIC,
                                    }));
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                        ("List", "tail") => {
                            if args.len() == 1 {
                                let tl = self.infer_expr(&args[0])?;
                                return Ok(tl);
                            }
                        }
                        ("List", "is_nil") => {
                            if args.len() == 1 {
                                let _ = self.infer_expr(&args[0])?;
                                return Ok(ITy::Known(T_BOOL));
                            }
                        }
                        _ => {}
                    }
                }

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

                // Module export call: `Mod.f(...)` (when module interface provides a function type).
                if let ExprKind::Member { base, name } = &callee.node {
                    if let ExprKind::Var(alias) = &base.node {
                        if let Some(exports) = self.ctx.module_alias_exports.get(alias) {
                            if let Some(&fun_tid) = exports.get(name) {
                                return self.infer_call_known_fun(fun_tid, args, e.span);
                            }
                        }
                    }
                }

                // Local/global binding call: `g(...)` when `g` has a known function type.
                if let ExprKind::Var(n) = &callee.node {
                    if let Some(ITy::Known(fun_tid)) = self.lookup(n) {
                        return self.infer_call_known_fun(fun_tid, args, e.span);
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

    fn infer_call_known_fun(
        &mut self,
        fun_tid: TypeId,
        args: &[Expr],
        call_span: crate::ast::Span,
    ) -> Result<ITy, CompileError> {
        let te = self
            .ctx
            .type_ctx
            .types
            .get(fun_tid as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, call_span, "bad function type id"))?;
        if te.kind != crate::jlyb::TypeKind::Function {
            return Ok(ITy::Known(T_DYNAMIC));
        }
        let sig = self
            .ctx
            .type_ctx
            .sigs
            .get(te.p0 as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, call_span, "bad fun sig id"))?
            .clone();
        if sig.args.len() != args.len() {
            return Err(CompileError::new(ErrorKind::Type, call_span, "call arity mismatch"));
        }
        for (i, a) in args.iter().enumerate() {
            let ta = self.infer_expr(a)?;
            let _ = unify(&mut self.dsu, ta, ITy::Known(sig.args[i]))?;
        }
        Ok(ITy::Known(sig.ret_type))
    }

    fn infer_unary_numeric_cast(&mut self, args: &[Expr], out: TypeId) -> Result<ITy, CompileError> {
        if args.len() != 1 {
            return Ok(ITy::Known(T_DYNAMIC));
        }
        let t0 = self.infer_expr(&args[0])?;
        let _ = infer_numeric_bin(&mut self.dsu, t0, ITy::Known(out))?;
        Ok(ITy::Known(out))
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ast::{ExprKind, Span, Spanned, StmtKind};
    use crate::ir::VRegId;
    use crate::lower::{Binding, LowerCtx};
    use crate::typectx::{T_BYTES, T_I32, T_I64, T_OBJECT};

    use super::infer_fn_type_for_let;

    fn mk_ctx() -> LowerCtx {
        LowerCtx {
            type_ctx: crate::typectx::TypeCtx::new_program_base(),
            const_bytes: Vec::new(),
            const_i64: Vec::new(),
            const_f64: Vec::new(),
            atoms: vec![b"__proto__".to_vec(), b"init".to_vec()],
            atom_ids: HashMap::from([
                ("__proto__".to_string(), crate::jlyb::ATOM___PROTO__),
                ("init".to_string(), crate::jlyb::ATOM_INIT),
            ]),
            env_stack: vec![HashMap::new()],
            loop_stack: Vec::new(),
            fn_stack: Vec::new(),
            nested_funcs: Vec::new(),
            pending_fn_self: None,
            user_top_level_fun_count: 1,
            exports_obj: None,
            module_alias_exports: HashMap::new(),
            module_key_to_alias: HashMap::new(),
            warnings: Vec::new(),
        }
    }

    #[test]
    fn infer_fn_uses_outer_typed_fun_binding() {
        let sp = Span::point(0);
        let mut ctx = mk_ctx();

        // Bind `g: (I64) -> I64` in the outer environment.
        let g_fun = ctx.type_ctx.intern_fun_type(T_I64, &[T_I64]);
        ctx.env_stack[0].insert(
            "g".to_string(),
            Binding {
                v: VRegId(0),
                tid: g_fun,
            },
        );
        // Also ensure `__global` exists in the env (mirrors normal lowering).
        ctx.env_stack[0].insert(
            "__global".to_string(),
            Binding {
                v: VRegId(1),
                tid: T_OBJECT,
            },
        );

        // fn(y) { return g(y); }
        let params = vec![("y".to_string(), None)];
        let call_g = Spanned::new(
            ExprKind::Call {
                callee: Box::new(Spanned::new(ExprKind::Var("g".to_string()), sp)),
                type_args: vec![],
                args: vec![Spanned::new(ExprKind::Var("y".to_string()), sp)],
            },
            sp,
        );
        let body = vec![Spanned::new(StmtKind::Return { expr: Some(call_g) }, sp)];

        let (_fun_tid, arg_tids, ret_tid) =
            infer_fn_type_for_let("f", &params, &body, &None, &mut ctx).unwrap();
        assert_eq!(arg_tids, vec![T_I64]);
        assert_eq!(ret_tid, T_I64);
    }

    #[test]
    fn infer_fn_from_bytes_len_builtin() {
        let sp = Span::point(0);
        let mut ctx = mk_ctx();
        ctx.env_stack[0].insert(
            "__global".to_string(),
            Binding {
                v: VRegId(0),
                tid: T_OBJECT,
            },
        );

        // fn(x) { return Bytes.len(x); }
        let params = vec![("x".to_string(), None)];
        let callee = Spanned::new(
            ExprKind::Member {
                base: Box::new(Spanned::new(ExprKind::Var("Bytes".to_string()), sp)),
                name: "len".to_string(),
            },
            sp,
        );
        let call = Spanned::new(
            ExprKind::Call {
                callee: Box::new(callee),
                type_args: vec![],
                args: vec![Spanned::new(ExprKind::Var("x".to_string()), sp)],
            },
            sp,
        );
        let body = vec![Spanned::new(StmtKind::Return { expr: Some(call) }, sp)];

        let (_fun_tid, arg_tids, ret_tid) =
            infer_fn_type_for_let("f", &params, &body, &None, &mut ctx).unwrap();
        assert_eq!(arg_tids, vec![T_BYTES]);
        assert_eq!(ret_tid, T_I32);
    }

    #[test]
    fn infer_fn_bytes_concat_via_plus() {
        let sp = Span::point(0);
        let mut ctx = mk_ctx();
        ctx.env_stack[0].insert(
            "__global".to_string(),
            Binding {
                v: VRegId(0),
                tid: T_OBJECT,
            },
        );

        // fn(x) { return x + "a"; }
        let params = vec![("x".to_string(), None)];
        let add = Spanned::new(
            ExprKind::Add(
                Box::new(Spanned::new(ExprKind::Var("x".to_string()), sp)),
                Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), sp)),
            ),
            sp,
        );
        let body = vec![Spanned::new(StmtKind::Return { expr: Some(add) }, sp)];

        let (_fun_tid, arg_tids, ret_tid) =
            infer_fn_type_for_let("f", &params, &body, &None, &mut ctx).unwrap();
        assert_eq!(arg_tids, vec![T_BYTES]);
        assert_eq!(ret_tid, T_BYTES);
    }
}
