use crate::ast::{Expr, ExprKind, Span};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::super::P;

use super::term;

pub(super) fn parse_or_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_and_expr(p)?;
    loop {
        if p.eat(TokenKind::PipePipe) {
            let rhs = parse_and_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Or(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_and_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_eq_expr(p)?;
    loop {
        if p.eat(TokenKind::AmpAmp) {
            let rhs = parse_eq_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::And(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_eq_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_rel_expr(p)?;
    loop {
        if p.eat(TokenKind::EqEq) {
            let rhs = parse_rel_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Eq(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::NotEq) {
            let rhs = parse_rel_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Ne(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_rel_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_add_expr(p)?;
    loop {
        if p.eat(TokenKind::Le) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Le(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Ge) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Ge(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Lt) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Lt(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Gt) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Gt(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_add_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_mul_expr(p)?;
    loop {
        if p.eat(TokenKind::Plus) {
            let lhs = e;
            let rhs = parse_mul_expr(p)?;
            let span = Span::new(lhs.span.start, rhs.span.end);
            e = match (lhs.node, rhs.node) {
                (ExprKind::BytesLit(mut a), ExprKind::BytesLit(b)) => {
                    a.extend_from_slice(&b);
                    Expr::new(ExprKind::BytesLit(a), span)
                }
                (a, b) => Expr::new(
                    ExprKind::Add(
                        Box::new(Expr::new(a, lhs.span)),
                        Box::new(Expr::new(b, rhs.span)),
                    ),
                    span,
                ),
            };
        } else if p.eat(TokenKind::Minus) {
            let rhs = parse_mul_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Sub(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_mul_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = term::parse_term(p)?;
    loop {
        if p.eat(TokenKind::Star) {
            let rhs = term::parse_term(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Mul(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Slash) {
            let rhs = term::parse_term(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Div(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}
