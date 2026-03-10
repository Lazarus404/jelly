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

use crate::ast::{
    Expr, ExprKind, MatchArm, Pattern, PatternKind, Program, Stmt, StmtKind, Ty, TyKind,
};

use super::{Visitor, VisitorMut};

macro_rules! walk_program_body {
    ($v:expr, $stmts:expr, $expr:expr) => {{
        for st in $stmts {
            $v.visit_stmt(st)?;
        }
        $v.visit_expr($expr)
    }};
}

macro_rules! walk_stmt_body {
    ($v:expr, $node:expr) => {{
        match $node {
            StmtKind::Let { ty, expr, .. } => {
                if let Some(t) = ty {
                    $v.visit_ty(t)?;
                }
                $v.visit_expr(expr)
            }
            StmtKind::Prototype { fields, .. } => {
                for (_, e) in fields {
                    $v.visit_expr(e)?;
                }
                Ok(())
            }
            StmtKind::Assign { expr, .. } => $v.visit_expr(expr),
            StmtKind::MemberAssign { base, expr, .. } => {
                $v.visit_expr(base)?;
                $v.visit_expr(expr)
            }
            StmtKind::IndexAssign { base, index, expr } => {
                $v.visit_expr(base)?;
                $v.visit_expr(index)?;
                $v.visit_expr(expr)
            }
            StmtKind::While { cond, body } => {
                $v.visit_expr(cond)?;
                for st in body {
                    $v.visit_stmt(st)?;
                }
                Ok(())
            }
            StmtKind::DoWhile { body, cond } => {
                for st in body {
                    $v.visit_stmt(st)?;
                }
                $v.visit_expr(cond)
            }
            StmtKind::Throw { expr } => $v.visit_expr(expr),
            StmtKind::Return { expr } => {
                if let Some(e) = expr {
                    $v.visit_expr(e)?;
                }
                Ok(())
            }
            StmtKind::Expr { expr } => $v.visit_expr(expr),
            StmtKind::ImportModule { .. }
            | StmtKind::ImportFrom { .. }
            | StmtKind::Break
            | StmtKind::Continue => Ok(()),
        }
    }};
}

macro_rules! walk_expr_body {
    ($v:expr, $node:expr) => {{
        match $node {
            ExprKind::Member { base, .. } => $v.visit_expr(base),
            ExprKind::Call {
                callee,
                type_args,
                args,
            } => {
                $v.visit_expr(callee)?;
                for t in type_args {
                    $v.visit_ty(t)?;
                }
                for a in args {
                    $v.visit_expr(a)?;
                }
                Ok(())
            }
            ExprKind::TypeApp { base, type_args } => {
                $v.visit_expr(base)?;
                for t in type_args {
                    $v.visit_ty(t)?;
                }
                Ok(())
            }
            ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
                for x in elems {
                    $v.visit_expr(x)?;
                }
                Ok(())
            }
            ExprKind::ObjLit(fields) => {
                for (_, v0) in fields {
                    $v.visit_expr(v0)?;
                }
                Ok(())
            }
            ExprKind::Index { base, index } => {
                $v.visit_expr(base)?;
                $v.visit_expr(index)
            }
            ExprKind::IndexAssign { base, index, expr } => {
                $v.visit_expr(base)?;
                $v.visit_expr(index)?;
                $v.visit_expr(expr)
            }
            ExprKind::Fn { params, body, tail } => {
                for (_pn, pty) in params {
                    if let Some(t) = pty {
                        $v.visit_ty(t)?;
                    }
                }
                for st in body {
                    $v.visit_stmt(st)?;
                }
                if let Some(t) = tail {
                    $v.visit_expr(t)?;
                }
                Ok(())
            }
            ExprKind::Truthy(x) | ExprKind::Not(x) | ExprKind::Neg(x) => $v.visit_expr(x),
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Mod(a, b)
            | ExprKind::Shl(a, b)
            | ExprKind::Shr(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::Ne(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::Le(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::Ge(a, b)
            | ExprKind::And(a, b)
            | ExprKind::Or(a, b) => {
                $v.visit_expr(a)?;
                $v.visit_expr(b)
            }
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => {
                $v.visit_expr(cond)?;
                $v.visit_expr(then_br)?;
                $v.visit_expr(else_br)
            }
            ExprKind::Block { stmts, expr } => {
                for st in stmts {
                    $v.visit_stmt(st)?;
                }
                $v.visit_expr(expr)
            }
            ExprKind::Let { ty, expr, .. } => {
                if let Some(t) = ty {
                    $v.visit_ty(t)?;
                }
                $v.visit_expr(expr)
            }
            ExprKind::Assign { expr, .. } => $v.visit_expr(expr),
            ExprKind::MemberAssign { base, expr, .. } => {
                $v.visit_expr(base)?;
                $v.visit_expr(expr)
            }
            ExprKind::Try {
                body, catch_body, ..
            } => {
                $v.visit_expr(body)?;
                $v.visit_expr(catch_body)
            }
            ExprKind::Match { subject, arms } => {
                $v.visit_expr(subject)?;
                for a in arms {
                    $v.visit_match_arm(a)?;
                }
                Ok(())
            }
            ExprKind::With {
                clauses,
                body,
                else_arms,
            } => {
                for (pat, expr) in clauses {
                    $v.visit_pattern(pat)?;
                    $v.visit_expr(expr)?;
                }
                $v.visit_expr(body)?;
                if let Some(arms) = else_arms {
                    for a in arms {
                        $v.visit_match_arm(a)?;
                    }
                }
                Ok(())
            }
            ExprKind::New { proto, args } => {
                $v.visit_expr(proto)?;
                for a in args {
                    $v.visit_expr(a)?;
                }
                Ok(())
            }
            ExprKind::BytesLit(_)
            | ExprKind::BoolLit(_)
            | ExprKind::I32Lit(_)
            | ExprKind::I8Lit(_)
            | ExprKind::I16Lit(_)
            | ExprKind::I64Lit(_)
            | ExprKind::F64Lit(_)
            | ExprKind::F16Lit(_)
            | ExprKind::AtomLit(_)
            | ExprKind::Null
            | ExprKind::Var(_) => Ok(()),
        }
    }};
}

macro_rules! walk_match_arm_body {
    ($v:expr, $pat:expr, $when:expr, $body:expr, $tail:expr) => {{
        $v.visit_pattern($pat)?;
        if let Some(w) = $when {
            $v.visit_expr(w)?;
        }
        for st in $body {
            $v.visit_stmt(st)?;
        }
        if let Some(t) = $tail {
            $v.visit_expr(t)?;
        }
        Ok(())
    }};
}

macro_rules! walk_pattern_body {
    ($v:expr, $node:expr) => {{
        match $node {
            PatternKind::Obj(fields) => {
                for (_, pat) in fields {
                    $v.visit_pattern(pat)?;
                }
                Ok(())
            }
            PatternKind::TupleExact(elems) | PatternKind::ArrayExact(elems) => {
                for el in elems {
                    $v.visit_pattern(el)?;
                }
                Ok(())
            }
            PatternKind::ArrayHeadTail { head, .. } => $v.visit_pattern(head),
            PatternKind::ArrayPrefixRest { prefix, .. } => {
                for el in prefix {
                    $v.visit_pattern(el)?;
                }
                Ok(())
            }
            PatternKind::Wildcard
            | PatternKind::BoolLit(_)
            | PatternKind::I8Lit(_)
            | PatternKind::I16Lit(_)
            | PatternKind::I32Lit(_)
            | PatternKind::Bind(_)
            | PatternKind::Pin(_) => Ok(()),
        }
    }};
}

macro_rules! walk_ty_body {
    ($v:expr, $node:expr) => {{
        match $node {
            TyKind::Named(_) => Ok(()),
            TyKind::Generic { args, .. } | TyKind::Tuple(args) => {
                for a in args {
                    $v.visit_ty(a)?;
                }
                Ok(())
            }
            TyKind::Fun { args, ret } => {
                for a in args {
                    $v.visit_ty(a)?;
                }
                $v.visit_ty(ret)
            }
        }
    }};
}

pub fn walk_program<V: Visitor + ?Sized>(v: &mut V, p: &Program) -> Result<(), V::Err> {
    walk_program_body!(v, &p.stmts, &p.expr)
}

pub fn walk_program_mut<V: VisitorMut + ?Sized>(v: &mut V, p: &mut Program) -> Result<(), V::Err> {
    walk_program_body!(v, &mut p.stmts, &mut p.expr)
}

pub fn walk_stmt<V: Visitor + ?Sized>(v: &mut V, s: &Stmt) -> Result<(), V::Err> {
    walk_stmt_body!(v, &s.node)
}

pub fn walk_stmt_mut<V: VisitorMut + ?Sized>(v: &mut V, s: &mut Stmt) -> Result<(), V::Err> {
    walk_stmt_body!(v, &mut s.node)
}

pub fn walk_expr<V: Visitor + ?Sized>(v: &mut V, e: &Expr) -> Result<(), V::Err> {
    walk_expr_body!(v, &e.node)
}

pub fn walk_expr_mut<V: VisitorMut + ?Sized>(v: &mut V, e: &mut Expr) -> Result<(), V::Err> {
    walk_expr_body!(v, &mut e.node)
}

pub fn walk_match_arm<V: Visitor + ?Sized>(v: &mut V, a: &MatchArm) -> Result<(), V::Err> {
    walk_match_arm_body!(v, &a.pat, &a.when, &a.body, &a.tail)
}

pub fn walk_match_arm_mut<V: VisitorMut + ?Sized>(
    v: &mut V,
    a: &mut MatchArm,
) -> Result<(), V::Err> {
    walk_match_arm_body!(v, &mut a.pat, &mut a.when, &mut a.body, &mut a.tail)
}

pub fn walk_pattern<V: Visitor + ?Sized>(v: &mut V, p: &Pattern) -> Result<(), V::Err> {
    walk_pattern_body!(v, &p.node)
}

pub fn walk_pattern_mut<V: VisitorMut + ?Sized>(v: &mut V, p: &mut Pattern) -> Result<(), V::Err> {
    walk_pattern_body!(v, &mut p.node)
}

pub fn walk_ty<V: Visitor + ?Sized>(v: &mut V, t: &Ty) -> Result<(), V::Err> {
    walk_ty_body!(v, &t.node)
}

pub fn walk_ty_mut<V: VisitorMut + ?Sized>(v: &mut V, t: &mut Ty) -> Result<(), V::Err> {
    walk_ty_body!(v, &mut t.node)
}
