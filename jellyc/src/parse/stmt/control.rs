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

use crate::ast::{Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::helpers::expect_stmt_terminator;
use super::P;

pub(super) fn try_parse_control_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
    if let Some(do_span) = p.eat_kw_span("do") {
        return Ok(Some(parse_do_while_stmt(p, do_span)?));
    }
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

fn parse_do_while_stmt(p: &mut P, do_span: Span) -> Result<Stmt, CompileError> {
    let body = parse_block_stmts(p)?;
    if p.eat_kw_span("while").is_none() {
        return Err(CompileError::at(
            ErrorKind::Parse,
            p.pos(),
            "expected 'while' after do block",
        ));
    }
    p.expect_char('(')?;
    let cond = p.parse_expr()?;
    p.expect_char(')')?;
    expect_stmt_terminator(p)?;
    Ok(Stmt::new(
        StmtKind::DoWhile { body, cond },
        Span::new(do_span.start, p.last_span_end()),
    ))
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
