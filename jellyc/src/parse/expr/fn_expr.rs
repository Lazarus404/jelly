use crate::ast::{Expr, ExprKind, Span, Stmt, Ty};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::super::P;

pub(super) fn parse_fn_expr(p: &mut P, fn_span: Span) -> Result<Expr, CompileError> {
    p.expect_char('(')?;
    let mut params: Vec<(String, Option<Ty>)> = Vec::new();
    if p.peek_kind() != Some(&TokenKind::RParen) {
        loop {
            let name = p.parse_ident()?;
            let ty = if p.eat_char(':') {
                Some(p.parse_type()?)
            } else {
                None
            };
            params.push((name, ty));
            if !p.eat_char(',') {
                break;
            }
        }
    }
    p.expect_char(')')?;
    let (body, tail) = parse_fn_body(p)?;
    let span = Span::new(fn_span.start, p.last_span_end());
    Ok(Expr::new(
        ExprKind::Fn {
            params,
            body,
            tail: tail.map(Box::new),
        },
        span,
    ))
}

pub(super) fn parse_fn_body(p: &mut P) -> Result<(Vec<Stmt>, Option<Expr>), CompileError> {
    p.expect_char('{')?;
    let mut stmts: Vec<Stmt> = Vec::new();
    loop {
        if p.eat(TokenKind::RBrace) {
            return Ok((stmts, None));
        }
        let save = p.i;
        if let Some(s) = p.parse_stmt()? {
            stmts.push(s);
            continue;
        }
        p.i = save;
        let e = p.parse_expr()?;
        p.expect_char('}')?;
        return Ok((stmts, Some(e)));
    }
}
