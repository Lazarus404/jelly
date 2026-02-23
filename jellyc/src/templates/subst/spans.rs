use crate::ast::{Expr, ExprKind, Span, Stmt, StmtKind};

#[allow(dead_code)]
pub(in crate::templates) fn subst_stmt_spans(
    s: &mut Stmt,
    shift_span: impl Fn(Span) -> Span + Copy,
) {
    s.span = shift_span(s.span);
    match &mut s.node {
        StmtKind::Let { ty, expr, .. } => {
            if let Some(t) = ty {
                t.span = shift_span(t.span);
            }
            subst_expr_spans(expr, shift_span);
        }
        StmtKind::Assign { expr, .. } => subst_expr_spans(expr, shift_span),
        StmtKind::Expr { expr } => subst_expr_spans(expr, shift_span),
        StmtKind::While { cond, body } => {
            subst_expr_spans(cond, shift_span);
            for st in body {
                subst_stmt_spans(st, shift_span);
            }
        }
        StmtKind::Return { expr } => {
            if let Some(e) = expr {
                subst_expr_spans(e, shift_span);
            }
        }
        StmtKind::Throw { expr } => subst_expr_spans(expr, shift_span),
        StmtKind::MemberAssign { base, expr, .. } => {
            subst_expr_spans(base, shift_span);
            subst_expr_spans(expr, shift_span);
        }
        StmtKind::IndexAssign { base, index, expr } => {
            subst_expr_spans(base, shift_span);
            subst_expr_spans(index, shift_span);
            subst_expr_spans(expr, shift_span);
        }
        StmtKind::ImportModule { .. }
        | StmtKind::ImportFrom { .. }
        | StmtKind::Prototype { .. }
        | StmtKind::Break
        | StmtKind::Continue => {}
    }
}

#[allow(dead_code)]
pub(in crate::templates) fn subst_expr_spans(
    e: &mut Expr,
    shift_span: impl Fn(Span) -> Span + Copy,
) {
    e.span = shift_span(e.span);
    match &mut e.node {
        ExprKind::Member { base, .. } => subst_expr_spans(base, shift_span),
        ExprKind::Call {
            callee,
            args,
            type_args,
            ..
        } => {
            subst_expr_spans(callee, shift_span);
            for a in args {
                subst_expr_spans(a, shift_span);
            }
            for ta in type_args {
                ta.span = shift_span(ta.span);
            }
        }
        ExprKind::TypeApp { base, type_args } => {
            subst_expr_spans(base, shift_span);
            for ta in type_args {
                ta.span = shift_span(ta.span);
            }
        }
        ExprKind::Index { base, index } => {
            subst_expr_spans(base, shift_span);
            subst_expr_spans(index, shift_span);
        }
        ExprKind::IndexAssign { base, index, expr } => {
            subst_expr_spans(base, shift_span);
            subst_expr_spans(index, shift_span);
            subst_expr_spans(expr, shift_span);
        }
        ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
            for el in elems {
                subst_expr_spans(el, shift_span);
            }
        }
        ExprKind::ObjLit(fields) => {
            for (_k, v) in fields {
                subst_expr_spans(v, shift_span);
            }
        }
        ExprKind::Fn { params, body, tail } => {
            for (_pn, ann) in params {
                if let Some(t) = ann {
                    t.span = shift_span(t.span);
                }
            }
            for st in body {
                subst_stmt_spans(st, shift_span);
            }
            if let Some(t) = tail.as_deref_mut() {
                subst_expr_spans(t, shift_span);
            }
        }
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => {
            subst_expr_spans(cond, shift_span);
            subst_expr_spans(then_br, shift_span);
            subst_expr_spans(else_br, shift_span);
        }
        ExprKind::Let { expr, .. } => subst_expr_spans(expr, shift_span),
        ExprKind::Block { stmts, expr } => {
            for st in stmts {
                subst_stmt_spans(st, shift_span);
            }
            subst_expr_spans(expr, shift_span);
        }
        ExprKind::Try {
            body, catch_body, ..
        } => {
            subst_expr_spans(body, shift_span);
            subst_expr_spans(catch_body, shift_span);
        }
        ExprKind::Match { subject, arms } => {
            subst_expr_spans(subject, shift_span);
            for a in arms {
                a.pat.span = shift_span(a.pat.span);
                if let Some(w) = &mut a.when {
                    subst_expr_spans(w, shift_span);
                }
                for st in &mut a.body {
                    subst_stmt_spans(st, shift_span);
                }
                if let Some(t) = &mut a.tail {
                    subst_expr_spans(t, shift_span);
                }
            }
        }
        ExprKind::New { proto, args } => {
            subst_expr_spans(proto, shift_span);
            for a in args {
                subst_expr_spans(a, shift_span);
            }
        }
        ExprKind::Truthy(x) | ExprKind::Not(x) | ExprKind::Neg(x) => {
            subst_expr_spans(x, shift_span)
        }
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
            subst_expr_spans(a, shift_span);
            subst_expr_spans(b, shift_span);
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
