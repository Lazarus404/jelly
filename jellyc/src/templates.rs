#![allow(dead_code)]

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

// Templates are a compile-time feature that allows you to define a template for a type
// and then instantiate it with different type arguments. This is used to define generic
// types and functions. This is a compile-time feature that allows you to define a 
// template for a type and then instantiate it with different type arguments.
// This is used to define generic types and functions.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, ExprKind, MatchArm, Program, Span, Stmt, StmtKind, Ty, TyKind};
use crate::error::{CompileError, ErrorKind};

#[derive(Clone)]
struct TemplateDef {
    span: Span,
    name: String,
    type_params: Vec<String>,
    ty: Ty,
    expr: Expr,
}

/// Get the key for a type.
fn type_key(t: &Ty) -> Result<String, CompileError> {
    fn enc(s: &str) -> String {
        // Identifier-safe fragment: [A-Za-z0-9_]+
        s.chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect()
    }

    match &t.node {
        TyKind::Named(n) => Ok(enc(n)),
        TyKind::Generic { base, args } => {
            let mut out = enc(base);
            for a in args {
                out.push('_');
                out.push_str(&type_key(a)?);
            }
            Ok(out)
        }
        TyKind::Tuple(elems) => {
            let mut out = "Tuple".to_string();
            out.push('_');
            out.push_str(&elems.len().to_string());
            for el in elems {
                out.push('_');
                out.push_str(&type_key(el)?);
            }
            Ok(out)
        }
        TyKind::Fun { .. } => Err(CompileError::new(
            ErrorKind::Type,
            t.span,
            "function types are not supported as template arguments yet",
        )),
    }
}

/// Get the name for a specialization.
fn spec_name(base: &str, type_args: &[Ty]) -> Result<String, CompileError> {
    let mut out = base.to_string();
    for a in type_args {
        out.push_str("__");
        out.push_str(&type_key(a)?);
    }
    Ok(out)
}

/// Substitute a type with a substitution.
fn subst_ty(t: &Ty, subst: &HashMap<String, Ty>) -> Ty {
    match &t.node {
        TyKind::Named(n) => subst
            .get(n)
            .cloned()
            .unwrap_or_else(|| Ty::new(TyKind::Named(n.clone()), t.span)),
        TyKind::Generic { base, args } => Ty::new(
            TyKind::Generic {
                base: base.clone(),
                args: args.iter().map(|a| subst_ty(a, subst)).collect(),
            },
            t.span,
        ),
        TyKind::Fun { args, ret } => Ty::new(
            TyKind::Fun {
                args: args.iter().map(|a| subst_ty(a, subst)).collect(),
                ret: Box::new(subst_ty(ret, subst)),
            },
            t.span,
        ),
        TyKind::Tuple(elems) => Ty::new(
            TyKind::Tuple(elems.iter().map(|a| subst_ty(a, subst)).collect()),
            t.span,
        ),
    }
}

/// Substitute an expression with a substitution.
fn subst_expr(e: &Expr, subst: &HashMap<String, Ty>) -> Expr {
    let node = match &e.node {
        ExprKind::BytesLit(b) => ExprKind::BytesLit(b.clone()),
        ExprKind::BoolLit(x) => ExprKind::BoolLit(*x),
        ExprKind::I32Lit(x) => ExprKind::I32Lit(*x),
        ExprKind::I8Lit(x) => ExprKind::I8Lit(*x),
        ExprKind::I16Lit(x) => ExprKind::I16Lit(*x),
        ExprKind::I64Lit(x) => ExprKind::I64Lit(*x),
        ExprKind::F16Lit(x) => ExprKind::F16Lit(*x),
        ExprKind::F64Lit(x) => ExprKind::F64Lit(*x),
        ExprKind::AtomLit(n) => ExprKind::AtomLit(n.clone()),
        ExprKind::Null => ExprKind::Null,
        ExprKind::Var(n) => ExprKind::Var(n.clone()),
        ExprKind::Member { base, name } => ExprKind::Member {
            base: Box::new(subst_expr(base, subst)),
            name: name.clone(),
        },
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => ExprKind::Call {
            callee: Box::new(subst_expr(callee, subst)),
            type_args: type_args.iter().map(|t| subst_ty(t, subst)).collect(),
            args: args.iter().map(|a| subst_expr(a, subst)).collect(),
        },
        ExprKind::TypeApp { base, type_args } => ExprKind::TypeApp {
            base: Box::new(subst_expr(base, subst)),
            type_args: type_args.iter().map(|t| subst_ty(t, subst)).collect(),
        },
        ExprKind::ArrayLit(elems) => ExprKind::ArrayLit(elems.iter().map(|a| subst_expr(a, subst)).collect()),
        ExprKind::TupleLit(elems) => ExprKind::TupleLit(elems.iter().map(|a| subst_expr(a, subst)).collect()),
        ExprKind::ObjLit(fields) => ExprKind::ObjLit(
            fields
                .iter()
                .map(|(k, v)| (k.clone(), subst_expr(v, subst)))
                .collect(),
        ),
        ExprKind::Index { base, index } => ExprKind::Index {
            base: Box::new(subst_expr(base, subst)),
            index: Box::new(subst_expr(index, subst)),
        },
        ExprKind::Fn { params, body, tail } => ExprKind::Fn {
            params: params
                .iter()
                .map(|(n, t)| (n.clone(), t.as_ref().map(|ty| subst_ty(ty, subst))))
                .collect(),
            body: body.iter().map(|s| subst_stmt(s, subst)).collect(),
            tail: tail.as_ref().map(|t| Box::new(subst_expr(t, subst))),
        },
        ExprKind::Truthy(a) => ExprKind::Truthy(Box::new(subst_expr(a, subst))),
        ExprKind::Not(a) => ExprKind::Not(Box::new(subst_expr(a, subst))),
        ExprKind::Neg(a) => ExprKind::Neg(Box::new(subst_expr(a, subst))),
        ExprKind::Add(a, b) => ExprKind::Add(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Sub(a, b) => ExprKind::Sub(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Mul(a, b) => ExprKind::Mul(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Div(a, b) => ExprKind::Div(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Eq(a, b) => ExprKind::Eq(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Ne(a, b) => ExprKind::Ne(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Lt(a, b) => ExprKind::Lt(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Le(a, b) => ExprKind::Le(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Gt(a, b) => ExprKind::Gt(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Ge(a, b) => ExprKind::Ge(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::And(a, b) => ExprKind::And(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::Or(a, b) => ExprKind::Or(Box::new(subst_expr(a, subst)), Box::new(subst_expr(b, subst))),
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => ExprKind::If {
            cond: Box::new(subst_expr(cond, subst)),
            then_br: Box::new(subst_expr(then_br, subst)),
            else_br: Box::new(subst_expr(else_br, subst)),
        },
        ExprKind::Block { stmts, expr } => ExprKind::Block {
            stmts: stmts.iter().map(|s| subst_stmt(s, subst)).collect(),
            expr: Box::new(subst_expr(expr, subst)),
        },
        ExprKind::Try {
            body,
            catch_name,
            catch_body,
        } => ExprKind::Try {
            body: Box::new(subst_expr(body, subst)),
            catch_name: catch_name.clone(),
            catch_body: Box::new(subst_expr(catch_body, subst)),
        },
        ExprKind::Match { subject, arms } => ExprKind::Match {
            subject: Box::new(subst_expr(subject, subst)),
            arms: arms.iter().map(|a| subst_arm(a, subst)).collect(),
        },
        ExprKind::New { proto, args } => ExprKind::New {
            proto: Box::new(subst_expr(proto, subst)),
            args: args.iter().map(|a| subst_expr(a, subst)).collect(),
        },
    };
    Expr::new(node, e.span)
}

/// Substitute a match arm with a substitution.
fn subst_arm(a: &MatchArm, subst: &HashMap<String, Ty>) -> MatchArm {
    MatchArm {
        pat: a.pat.clone(),
        when: a.when.as_ref().map(|w| subst_expr(w, subst)),
        body: a.body.iter().map(|s| subst_stmt(s, subst)).collect(),
        tail: a.tail.as_ref().map(|t| subst_expr(t, subst)),
    }
}

/// Substitute a statement with a substitution.
fn subst_stmt(s: &Stmt, subst: &HashMap<String, Ty>) -> Stmt {
    let node = match &s.node {
        StmtKind::Let {
            is_const,
            exported,
            name,
            type_params,
            ty,
            expr,
        } => StmtKind::Let {
            is_const: *is_const,
            exported: *exported,
            name: name.clone(),
            type_params: type_params.clone(),
            ty: ty.as_ref().map(|t| subst_ty(t, subst)),
            expr: subst_expr(expr, subst),
        },
        StmtKind::Prototype {
            exported,
            name,
            type_params,
            fields,
        } => StmtKind::Prototype {
            exported: *exported,
            name: name.clone(),
            type_params: type_params.clone(),
            fields: fields
                .iter()
                .map(|(k, v)| (k.clone(), subst_expr(v, subst)))
                .collect(),
        },
        StmtKind::ImportModule { path, alias } => StmtKind::ImportModule {
            path: path.clone(),
            alias: alias.clone(),
        },
        StmtKind::ImportFrom { type_only, items, from } => StmtKind::ImportFrom {
            type_only: *type_only,
            items: items.clone(),
            from: from.clone(),
        },
        StmtKind::Assign { name, expr } => StmtKind::Assign {
            name: name.clone(),
            expr: subst_expr(expr, subst),
        },
        StmtKind::MemberAssign { base, name, expr } => StmtKind::MemberAssign {
            base: subst_expr(base, subst),
            name: name.clone(),
            expr: subst_expr(expr, subst),
        },
        StmtKind::IndexAssign { base, index, expr } => StmtKind::IndexAssign {
            base: subst_expr(base, subst),
            index: subst_expr(index, subst),
            expr: subst_expr(expr, subst),
        },
        StmtKind::While { cond, body } => StmtKind::While {
            cond: subst_expr(cond, subst),
            body: body.iter().map(|st| subst_stmt(st, subst)).collect(),
        },
        StmtKind::Break => StmtKind::Break,
        StmtKind::Continue => StmtKind::Continue,
        StmtKind::Throw { expr } => StmtKind::Throw {
            expr: subst_expr(expr, subst),
        },
        StmtKind::Return { expr } => StmtKind::Return {
            expr: expr.as_ref().map(|e| subst_expr(e, subst)),
        },
        StmtKind::Expr { expr } => StmtKind::Expr {
            expr: subst_expr(expr, subst),
        },
    };
    Stmt::new(node, s.span)
}

/// Expander for templates.
/// This is used to expand templates into concrete specialized types and expressions.
struct Expander {
    templates: HashMap<String, TemplateDef>,
    emitted: HashSet<String>, // spec name
    stack: Vec<String>,       // spec name
    out_specs: Vec<Stmt>,
    known_vars: HashMap<String, Ty>, // top-level typed lets seen so far (MVP inference aid)
    tmp_counter: u32,
}

impl Expander {
    fn shift_span(sp: Span, delta: i64) -> Span {
        if delta == 0 {
            return sp;
        }
        if delta > 0 {
            let d = delta as usize;
            Span {
                start: sp.start.saturating_add(d),
                end: sp.end.saturating_add(d),
            }
        } else {
            let d = (-delta) as usize;
            Span {
                start: sp.start.saturating_sub(d),
                end: sp.end.saturating_sub(d),
            }
        }
    }

    fn shift_stmt_spans(&mut self, s: &mut Stmt, delta: i64) {
        s.span = Self::shift_span(s.span, delta);
        match &mut s.node {
            StmtKind::Let { ty, expr, .. } => {
                if let Some(t) = ty {
                    t.span = Self::shift_span(t.span, delta);
                }
                self.shift_expr_spans(expr, delta);
            }
            StmtKind::Assign { expr, .. } => self.shift_expr_spans(expr, delta),
            StmtKind::Expr { expr } => self.shift_expr_spans(expr, delta),
            StmtKind::While { cond, body } => {
                self.shift_expr_spans(cond, delta);
                for st in body {
                    self.shift_stmt_spans(st, delta);
                }
            }
            StmtKind::Return { expr } => {
                if let Some(e) = expr {
                    self.shift_expr_spans(e, delta);
                }
            }
            StmtKind::Throw { expr } => self.shift_expr_spans(expr, delta),
            StmtKind::MemberAssign { base, expr, .. } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(expr, delta);
            }
            StmtKind::IndexAssign { base, index, expr } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(index, delta);
                self.shift_expr_spans(expr, delta);
            }
            StmtKind::ImportModule { .. }
            | StmtKind::ImportFrom { .. }
            | StmtKind::Prototype { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    fn shift_expr_spans(&mut self, e: &mut Expr, delta: i64) {
        e.span = Self::shift_span(e.span, delta);
        match &mut e.node {
            ExprKind::Member { base, .. } => self.shift_expr_spans(base, delta),
            ExprKind::Call { callee, args, type_args, .. } => {
                self.shift_expr_spans(callee, delta);
                for a in args {
                    self.shift_expr_spans(a, delta);
                }
                for ta in type_args {
                    ta.span = Self::shift_span(ta.span, delta);
                }
            }
            ExprKind::TypeApp { base, type_args } => {
                self.shift_expr_spans(base, delta);
                for ta in type_args {
                    ta.span = Self::shift_span(ta.span, delta);
                }
            }
            ExprKind::Index { base, index } => {
                self.shift_expr_spans(base, delta);
                self.shift_expr_spans(index, delta);
            }
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
                for el in elems {
                    self.shift_expr_spans(el, delta);
                }
            }
            ExprKind::ObjLit(fields) => {
                for (_k, v) in fields {
                    self.shift_expr_spans(v, delta);
                }
            }
            ExprKind::Fn { params, body, tail } => {
                for (_pn, ann) in params {
                    if let Some(t) = ann {
                        t.span = Self::shift_span(t.span, delta);
                    }
                }
                for st in body {
                    self.shift_stmt_spans(st, delta);
                }
                if let Some(t) = tail.as_deref_mut() {
                    self.shift_expr_spans(t, delta);
                }
            }
            ExprKind::If { cond, then_br, else_br } => {
                self.shift_expr_spans(cond, delta);
                self.shift_expr_spans(then_br, delta);
                self.shift_expr_spans(else_br, delta);
            }
            ExprKind::Block { stmts, expr } => {
                for st in stmts {
                    self.shift_stmt_spans(st, delta);
                }
                self.shift_expr_spans(expr, delta);
            }
            ExprKind::Try { body, catch_body, .. } => {
                self.shift_expr_spans(body, delta);
                self.shift_expr_spans(catch_body, delta);
            }
            ExprKind::Match { subject, arms } => {
                self.shift_expr_spans(subject, delta);
                for a in arms {
                    a.pat.span = Self::shift_span(a.pat.span, delta);
                    // Patterns are nested; for now we only shift the top pattern span,
                    // which is sufficient to avoid cross-specialization collisions.
                    if let Some(w) = &mut a.when {
                        self.shift_expr_spans(w, delta);
                    }
                    for st in &mut a.body {
                        self.shift_stmt_spans(st, delta);
                    }
                    if let Some(t) = &mut a.tail {
                        self.shift_expr_spans(t, delta);
                    }
                }
            }
            ExprKind::New { proto, args } => {
                self.shift_expr_spans(proto, delta);
                for a in args {
                    self.shift_expr_spans(a, delta);
                }
            }
            ExprKind::Truthy(x)
            | ExprKind::Not(x)
            | ExprKind::Neg(x) => self.shift_expr_spans(x, delta),
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b)
            | ExprKind::And(a, b)
            | ExprKind::Or(a, b) => {
                self.shift_expr_spans(a, delta);
                self.shift_expr_spans(b, delta);
            }
            ExprKind::BytesLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::I32Lit(_)
            | ExprKind::I8Lit(_)
            | ExprKind::I16Lit(_)
            | ExprKind::I64Lit(_)
            | ExprKind::F16Lit(_)
            | ExprKind::F64Lit(_)
            | ExprKind::AtomLit(_)
            | ExprKind::Null
            | ExprKind::Var(_) => {}
        }
    }

    fn wrap_obj_with_template_meta(
        &mut self,
        obj_expr: Expr, // Expression for the object literal
        tmpl_name: &str, // Name of the template
        type_args: &[Ty], // Type arguments
        self_ty: &Ty, // Self type
        span: Span, // Span for the statement
    ) -> Result<Expr, CompileError> {
        // Template expansion introduces synthetic AST nodes. Our semantic tables are keyed by `Span`,
        // so we must avoid re-using the same span for multiple different nodes.
        let sp = |off: usize| Span::point(span.start.saturating_add(off));

        let tmp = format!("__tmpl{}", self.tmp_counter);
        self.tmp_counter += 1;

        let mut arg_s = String::new();
        for (i, ta) in type_args.iter().enumerate() {
            if i != 0 {
                arg_s.push(',');
            }
            arg_s.push_str(&type_key(ta)?);
        }

        let s_tmp = Stmt::new(
            StmtKind::Let {
                is_const: false,
                exported: false,
                name: tmp.clone(),
                type_params: Vec::new(),
                ty: Some(self_ty.clone()),
                expr: obj_expr,
            },
            sp(0),
        );
        let set_name = Stmt::new(
            StmtKind::MemberAssign {
                base: Expr::new(ExprKind::Var(tmp.clone()), sp(1)),
                name: "__template__".to_string(),
                expr: Expr::new(ExprKind::BytesLit(tmpl_name.as_bytes().to_vec()), sp(4)),
            },
            sp(1),
        );
        let set_args = Stmt::new(
            StmtKind::MemberAssign {
                base: Expr::new(ExprKind::Var(tmp.clone()), sp(2)),
                name: "__type_args__".to_string(),
                expr: Expr::new(ExprKind::BytesLit(arg_s.into_bytes()), sp(5)),
            },
            sp(2),
        );
        Ok(Expr::new(
            ExprKind::Block {
                stmts: vec![s_tmp, set_name, set_args],
                expr: Box::new(Expr::new(ExprKind::Var(tmp), sp(3))),
            },
            span,
        ))
    }

    /// Infer the type from an expression.
    /// This is used to infer the type of a literal expression.
    fn infer_ty_from_expr(&self, e: &Expr) -> Option<Ty> {
        match &e.node {
            ExprKind::I8Lit(_) => Some(Ty::new(TyKind::Named("I8".to_string()), e.span)),
            ExprKind::I16Lit(_) => Some(Ty::new(TyKind::Named("I16".to_string()), e.span)),
            ExprKind::I32Lit(_) => Some(Ty::new(TyKind::Named("I32".to_string()), e.span)),
            ExprKind::I64Lit(_) => Some(Ty::new(TyKind::Named("I64".to_string()), e.span)),
            ExprKind::F16Lit(_) => Some(Ty::new(TyKind::Named("F16".to_string()), e.span)),
            ExprKind::F64Lit(_) => Some(Ty::new(TyKind::Named("F32".to_string()), e.span)),
            ExprKind::BoolLit(_) => Some(Ty::new(TyKind::Named("Bool".to_string()), e.span)),
            ExprKind::BytesLit(_) => Some(Ty::new(TyKind::Named("Bytes".to_string()), e.span)),
            ExprKind::Var(n) => self.known_vars.get(n).cloned(),
            // MVP: no inference for `null` or non-trivial expressions.
            _ => None,
        }
    }

    /// Instantiate a template.
    /// This is used to instantiate a template with a given set of type arguments.
    fn instantiate(&mut self, base: &str, type_args: &[Ty], use_span: Span) -> Result<String, CompileError> {
        let def = self.templates.get(base).cloned().ok_or_else(|| {
            CompileError::new(ErrorKind::Type, use_span, format!("'{}' is not a template", base))
        })?;
        if type_args.len() != def.type_params.len() {
            return Err(CompileError::new(
                ErrorKind::Type,
                use_span,
                format!(
                    "template '{}' expects {} type args, got {}",
                    base,
                    def.type_params.len(),
                    type_args.len()
                ),
            ));
        }

        let nm = spec_name(base, type_args)?;
        if self.emitted.contains(&nm) {
            return Ok(nm);
        }
        if self.stack.contains(&nm) {
            return Err(CompileError::new(
                ErrorKind::Type,
                use_span,
                format!("template specialization cycle involving '{}'", nm),
            ));
        }

        self.stack.push(nm.clone());

        let mut subst: HashMap<String, Ty> = HashMap::new();
        for (tp, ta) in def.type_params.iter().zip(type_args.iter()) {
            subst.insert(tp.clone(), ta.clone());
        }

        let spec_ty = subst_ty(&def.ty, &subst);
        let spec_expr_raw = subst_expr(&def.expr, &subst);
        let mut spec_expr = self.rewrite_expr(&spec_expr_raw)?;

        // Specializations clone AST nodes from the template definition; ensure spans are
        // distinct across different specializations so semantic side tables (keyed by span)
        // don't collide.
        let spec_idx = self.emitted.len() as i64;
        self.shift_expr_spans(&mut spec_expr, -spec_idx);

        // Runtime template metadata (MVP):
        // If the specialization produces an object literal (commonly prototypes), attach
        // `.__template__` and `.__type_args__` fields on the object.
        if matches!(spec_expr.node, ExprKind::ObjLit(_)) {
            // Use the specialization expression span (not the instantiation site span) to avoid
            // span collisions with unrelated expressions in the use site.
            let meta_span = spec_expr.span;
            spec_expr = self.wrap_obj_with_template_meta(spec_expr, base, type_args, &spec_ty, meta_span)?;
        }

        self.out_specs.push(Stmt::new(
            StmtKind::Let {
                is_const: false,
                exported: false,
                name: nm.clone(),
                type_params: Vec::new(),
                ty: Some(spec_ty),
                expr: spec_expr,
            },
            // Use the instantiation site span so different specializations don't collide in
            // semantic side tables keyed by `Span`.
            use_span,
        ));

        self.emitted.insert(nm.clone());
        let _ = self.stack.pop();
        Ok(nm)
    }

    /// Rewrite an expression.
    /// This is used to rewrite an expression to a concrete specialized expression.
    fn rewrite_expr(&mut self, e: &Expr) -> Result<Expr, CompileError> {
        match &e.node {
            ExprKind::TypeApp { base, type_args } => {
                let base_name = match &base.node {
                    ExprKind::Var(n) => n,
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "type application is only supported on a template name (e.g. Name<I32>)",
                        ))
                    }
                };
                let nm = self.instantiate(base_name, type_args, e.span)?;
                Ok(Expr::new(ExprKind::Var(nm), e.span))
            }
            ExprKind::Var(n) if self.templates.contains_key(n) => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                format!("templated value '{}' must be specialized: use '{}<...>'", n, n),
            )),
            _ => {
                let node = match &e.node {
                    ExprKind::BytesLit(b) => ExprKind::BytesLit(b.clone()),
                    ExprKind::BoolLit(x) => ExprKind::BoolLit(*x),
                    ExprKind::I32Lit(x) => ExprKind::I32Lit(*x),
                    ExprKind::I8Lit(x) => ExprKind::I8Lit(*x),
                    ExprKind::I16Lit(x) => ExprKind::I16Lit(*x),
                    ExprKind::I64Lit(x) => ExprKind::I64Lit(*x),
                    ExprKind::F16Lit(x) => ExprKind::F16Lit(*x),
                    ExprKind::F64Lit(x) => ExprKind::F64Lit(*x),
                    ExprKind::AtomLit(n) => ExprKind::AtomLit(n.clone()),
                    ExprKind::Null => ExprKind::Null,
                    ExprKind::Var(n) => ExprKind::Var(n.clone()),
                    ExprKind::Member { base, name } => ExprKind::Member {
                        base: Box::new(self.rewrite_expr(base)?),
                        name: name.clone(),
                    },
                    ExprKind::Call { callee, type_args, args } => {
                        // Templated function call: `f<T>(...)`
                        if !type_args.is_empty() {
                            if let ExprKind::Var(base_name) = &callee.node {
                                if self.templates.contains_key(base_name) {
                                    let nm = self.instantiate(base_name, type_args, e.span)?;
                                    ExprKind::Call {
                                        callee: Box::new(Expr::new(ExprKind::Var(nm), callee.span)),
                                        type_args: Vec::new(),
                                        args: args.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?,
                                    }
                                } else {
                                    ExprKind::Call {
                                        callee: Box::new(self.rewrite_expr(callee)?),
                                        type_args: type_args.clone(),
                                        args: args.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?,
                                    }
                                }
                            } else {
                                ExprKind::Call {
                                    callee: Box::new(self.rewrite_expr(callee)?),
                                    type_args: type_args.clone(),
                                    args: args.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?,
                                }
                            }
                        } else {
                            ExprKind::Call {
                                callee: Box::new(self.rewrite_expr(callee)?),
                                type_args: Vec::new(),
                                args: args.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?,
                            }
                        }
                    }
                    ExprKind::TypeApp { .. } => unreachable!("handled above"),
                    ExprKind::ArrayLit(elems) => {
                        ExprKind::ArrayLit(elems.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?)
                    }
                    ExprKind::TupleLit(elems) => {
                        ExprKind::TupleLit(elems.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?)
                    }
                    ExprKind::ObjLit(fields) => ExprKind::ObjLit(
                        fields
                            .iter()
                            .map(|(k, v)| Ok((k.clone(), self.rewrite_expr(v)?)))
                            .collect::<Result<_, CompileError>>()?,
                    ),
                    ExprKind::Index { base, index } => ExprKind::Index {
                        base: Box::new(self.rewrite_expr(base)?),
                        index: Box::new(self.rewrite_expr(index)?),
                    },
                    ExprKind::Fn { params, body, tail } => ExprKind::Fn {
                        params: params.clone(),
                        body: body.iter().map(|s| self.rewrite_stmt(s)).collect::<Result<_, _>>()?,
                        tail: match tail {
                            Some(t) => Some(Box::new(self.rewrite_expr(t)?)),
                            None => None,
                        },
                    },
                    ExprKind::Truthy(a) => ExprKind::Truthy(Box::new(self.rewrite_expr(a)?)),
                    ExprKind::Not(a) => ExprKind::Not(Box::new(self.rewrite_expr(a)?)),
                    ExprKind::Neg(a) => ExprKind::Neg(Box::new(self.rewrite_expr(a)?)),
                    ExprKind::Add(a, b) => ExprKind::Add(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Sub(a, b) => ExprKind::Sub(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Mul(a, b) => ExprKind::Mul(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Div(a, b) => ExprKind::Div(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Eq(a, b) => ExprKind::Eq(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Ne(a, b) => ExprKind::Ne(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Lt(a, b) => ExprKind::Lt(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Le(a, b) => ExprKind::Le(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Gt(a, b) => ExprKind::Gt(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Ge(a, b) => ExprKind::Ge(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::And(a, b) => ExprKind::And(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::Or(a, b) => ExprKind::Or(Box::new(self.rewrite_expr(a)?), Box::new(self.rewrite_expr(b)?)),
                    ExprKind::If { cond, then_br, else_br } => ExprKind::If {
                        cond: Box::new(self.rewrite_expr(cond)?),
                        then_br: Box::new(self.rewrite_expr(then_br)?),
                        else_br: Box::new(self.rewrite_expr(else_br)?),
                    },
                    ExprKind::Block { stmts, expr } => ExprKind::Block {
                        stmts: stmts.iter().map(|s| self.rewrite_stmt(s)).collect::<Result<_, _>>()?,
                        expr: Box::new(self.rewrite_expr(expr)?),
                    },
                    ExprKind::Try { body, catch_name, catch_body } => ExprKind::Try {
                        body: Box::new(self.rewrite_expr(body)?),
                        catch_name: catch_name.clone(),
                        catch_body: Box::new(self.rewrite_expr(catch_body)?),
                    },
                    ExprKind::Match { subject, arms } => ExprKind::Match {
                        subject: Box::new(self.rewrite_expr(subject)?),
                        arms: arms.iter().map(|a| self.rewrite_arm(a)).collect::<Result<_, _>>()?,
                    },
                    ExprKind::New { proto, args } => ExprKind::New {
                        proto: {
                            // Template inference (MVP): allow `new Proto(args...)` where `Proto`
                            // is a templated `let Proto<T,...> = ...;`, and infer `<...>` from
                            // literal argument types.
                            //
                            // Rule: template arity must equal argument count, and each type arg
                            // must be inferrable (currently: I32/Bool/Bytes literals only).
                            if let ExprKind::Var(base_name) = &proto.node {
                                if let Some(def) = self.templates.get(base_name) {
                                    if def.type_params.len() != args.len() {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            e.span,
                                            format!(
                                                "cannot infer template args for 'new {}(...)': template expects {} type args, call provides {} value args",
                                                base_name,
                                                def.type_params.len(),
                                                args.len()
                                            ),
                                        ));
                                    }
                                    let mut targs: Vec<Ty> = Vec::with_capacity(args.len());
                                    for a in args {
                                        let Some(t) = self.infer_ty_from_expr(a) else {
                                            return Err(CompileError::new(
                                                ErrorKind::Type,
                                                a.span,
                                                format!(
                                                    "cannot infer template type for 'new {}(...)' from this argument; use 'new {}<...>(...)'",
                                                    base_name, base_name
                                                ),
                                            ));
                                        };
                                        targs.push(t);
                                    }
                                    let nm = self.instantiate(base_name, &targs, e.span)?;
                                    Box::new(Expr::new(ExprKind::Var(nm), proto.span))
                                } else {
                                    Box::new(self.rewrite_expr(proto)?)
                                }
                            } else {
                                Box::new(self.rewrite_expr(proto)?)
                            }
                        },
                        args: args.iter().map(|a| self.rewrite_expr(a)).collect::<Result<_, _>>()?,
                    },
                };
                Ok(Expr::new(node, e.span))
            }
        }
    }

    /// Rewrite a match arm.
    fn rewrite_arm(&mut self, a: &MatchArm) -> Result<MatchArm, CompileError> {
        Ok(MatchArm {
            pat: a.pat.clone(),
            when: match &a.when {
                Some(w) => Some(self.rewrite_expr(w)?),
                None => None,
            },
            body: a.body.iter().map(|s| self.rewrite_stmt(s)).collect::<Result<_, _>>()?,
            tail: match &a.tail {
                Some(t) => Some(self.rewrite_expr(t)?),
                None => None,
            },
        })
    }

    fn rewrite_stmt(&mut self, s: &Stmt) -> Result<Stmt, CompileError> {
        let node = match &s.node {
            StmtKind::Let {
                is_const: _,
                exported,
                name,
                type_params,
                ty,
                expr,
            } => {
                if !type_params.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "internal: template let must be removed before rewriting statements",
                    ));
                }
                if let Some(t) = ty {
                    self.known_vars.insert(name.clone(), t.clone());
                }
                StmtKind::Let {
                    is_const: false,
                    exported: *exported,
                    name: name.clone(),
                    type_params: Vec::new(),
                    ty: ty.clone(),
                    expr: self.rewrite_expr(expr)?,
                }
            }
            StmtKind::ImportModule { path, alias } => StmtKind::ImportModule {
                path: path.clone(),
                alias: alias.clone(),
            },
            StmtKind::ImportFrom { type_only, items, from } => StmtKind::ImportFrom {
                type_only: *type_only,
                items: items.clone(),
                from: from.clone(),
            },
            StmtKind::Prototype { .. } => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "internal: prototype must be desugared before rewriting statements",
                ))
            }
            StmtKind::Assign { name, expr } => StmtKind::Assign {
                name: name.clone(),
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::MemberAssign { base, name, expr } => StmtKind::MemberAssign {
                base: self.rewrite_expr(base)?,
                name: name.clone(),
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::IndexAssign { base, index, expr } => StmtKind::IndexAssign {
                base: self.rewrite_expr(base)?,
                index: self.rewrite_expr(index)?,
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::While { cond, body } => StmtKind::While {
                cond: self.rewrite_expr(cond)?,
                body: body.iter().map(|st| self.rewrite_stmt(st)).collect::<Result<_, _>>()?,
            },
            StmtKind::Break => StmtKind::Break,
            StmtKind::Continue => StmtKind::Continue,
            StmtKind::Throw { expr } => StmtKind::Throw {
                expr: self.rewrite_expr(expr)?,
            },
            StmtKind::Return { expr } => StmtKind::Return {
                expr: match expr {
                    Some(e) => Some(self.rewrite_expr(e)?),
                    None => None,
                },
            },
            StmtKind::Expr { expr } => StmtKind::Expr {
                expr: self.rewrite_expr(expr)?,
            },
        };
        Ok(Stmt::new(node, s.span))
    }
}

/// Expand templates in a program.
/// This is used to expand templates in a program into concrete specialized types and expressions.
/// This is the main entry point for template expansion.
pub fn expand_templates(prog: &mut Program) -> Result<(), CompileError> {
    // First, desugar `prototype Name<T> { ... }` into `let Name<T>: Object = {...};`
    let mut desugared: Vec<Stmt> = Vec::with_capacity(prog.stmts.len());
    for s in &prog.stmts {
        match &s.node {
            StmtKind::Prototype {
                exported,
                name,
                type_params,
                fields,
            } => {
                let obj_ty = if type_params.is_empty() {
                    Ty::new(TyKind::Named(name.clone()), s.span)
                } else {
                    let args: Vec<Ty> = type_params
                        .iter()
                        .map(|tp| Ty::new(TyKind::Named(tp.clone()), s.span))
                        .collect();
                    Ty::new(
                        TyKind::Generic {
                            base: name.clone(),
                            args,
                        },
                        s.span,
                    )
                };
                let obj_expr = Expr::new(ExprKind::ObjLit(fields.clone()), s.span);
                desugared.push(Stmt::new(
                    StmtKind::Let {
                        is_const: false,
                        exported: *exported,
                        name: name.clone(),
                        type_params: type_params.clone(),
                        ty: Some(obj_ty),
                        expr: obj_expr,
                    },
                    s.span,
                ));
            }
            _ => desugared.push(s.clone()),
        }
    }
    prog.stmts = desugared;

    // Collect template definitions (top-level only for MVP).
    let mut templates: HashMap<String, TemplateDef> = HashMap::new();
    for s in &prog.stmts {
        if let StmtKind::Let {
            is_const: _,
            exported,
            name,
            type_params,
            ty,
            expr,
        } = &s.node
        {
            if type_params.is_empty() {
                continue;
            }
            if *exported {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "exporting templated lets is not supported yet (export a concrete specialization instead)",
                ));
            }
            let ty = ty.clone().ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "templated let requires an explicit type annotation (MVP limitation)",
                )
            })?;
            templates.insert(
                name.clone(),
                TemplateDef {
                    span: s.span,
                    name: name.clone(),
                    type_params: type_params.clone(),
                    ty,
                    expr: expr.clone(),
                },
            );
        }
    }

    if templates.is_empty() {
        return Ok(());
    }

    let mut ex = Expander {
        templates,
        emitted: HashSet::new(),
        stack: Vec::new(),
        out_specs: Vec::new(),
        known_vars: HashMap::new(),
        tmp_counter: 0,
    };

    // Rewrite runtime stmts and expression, instantiating templates on demand.
    let mut out_runtime: Vec<Stmt> = Vec::new();
    for s in &prog.stmts {
        match &s.node {
            StmtKind::Let { type_params, .. } if !type_params.is_empty() => {
                // Drop template definitions from runtime.
            }
            StmtKind::Prototype { .. } => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    "internal: prototype must be desugared before template expansion",
                ));
            }
            _ => out_runtime.push(ex.rewrite_stmt(s)?),
        }
    }
    let out_expr = ex.rewrite_expr(&prog.expr)?;

    // Emit specializations before runtime code (MVP).
    let mut all: Vec<Stmt> = Vec::new();
    all.extend(ex.out_specs);
    all.extend(out_runtime);
    prog.stmts = all;
    prog.expr = out_expr;
    Ok(())
}

