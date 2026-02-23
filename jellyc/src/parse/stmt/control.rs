use crate::ast::{Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::helpers::expect_stmt_terminator;
use super::P;

pub(super) fn try_parse_control_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
    if let Some(while_span) = p.eat_kw_span("while") {
        return Ok(Some(parse_while_stmt(p, while_span)?));
    }
    if let Some(break_span) = p.eat_kw_span("break") {
        expect_stmt_terminator(p)?;
        return Ok(Some(Stmt::new(
            StmtKind::Break,
            Span::new(break_span.start, p.last_span_end()),
        )));
    }
    if let Some(cont_span) = p.eat_kw_span("continue") {
        expect_stmt_terminator(p)?;
        return Ok(Some(Stmt::new(
            StmtKind::Continue,
            Span::new(cont_span.start, p.last_span_end()),
        )));
    }
    if let Some(throw_span) = p.eat_kw_span("throw") {
        let expr = p.parse_expr()?;
        expect_stmt_terminator(p)?;
        return Ok(Some(Stmt::new(
            StmtKind::Throw { expr },
            Span::new(throw_span.start, p.last_span_end()),
        )));
    }
    if let Some(ret_span) = p.eat_kw_span("return") {
        if p.eat(TokenKind::Semicolon) {
            return Ok(Some(Stmt::new(
                StmtKind::Return { expr: None },
                Span::new(ret_span.start, p.last_span_end()),
            )));
        }
        let expr = p.parse_expr()?;
        expect_stmt_terminator(p)?;
        return Ok(Some(Stmt::new(
            StmtKind::Return { expr: Some(expr) },
            Span::new(ret_span.start, p.last_span_end()),
        )));
    }
    Ok(None)
}

fn parse_while_stmt(p: &mut P, while_span: Span) -> Result<Stmt, CompileError> {
    p.expect_char('(')?;
    let cond = p.parse_expr()?;
    p.expect_char(')')?;
    let body = parse_block_stmts(p)?;
    Ok(Stmt::new(
        StmtKind::While { cond, body },
        Span::new(while_span.start, p.last_span_end()),
    ))
}

pub(super) fn parse_block_stmts(p: &mut P) -> Result<Vec<Stmt>, CompileError> {
    p.expect_char('{')?;
    let mut out: Vec<Stmt> = Vec::new();
    loop {
        if p.eat(TokenKind::RBrace) {
            break;
        }
        let s = p
            .parse_stmt()?
            .ok_or_else(|| CompileError::at(ErrorKind::Parse, p.pos(), "expected statement"))?;
        out.push(s);
    }
    Ok(out)
}
