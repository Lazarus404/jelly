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

// Statement parsing.

use crate::ast::{Expr, ExprKind, Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::P;

fn expect_stmt_terminator(p: &mut P) -> Result<(), CompileError> {
    if p.eat(TokenKind::Semicolon) {
        return Ok(());
    }
    // Allow semicolon elision when the next token can't be part of the current
    // statement (or we're at a structural boundary).
    //
    // This keeps `expr;` required for expression statements, but allows:
    //   let x = if (...) { ... } else { ... }
    //   System.assert(...)
    // with a newline between them.
    if matches!(p.peek_kind(), Some(TokenKind::RBrace) | Some(TokenKind::Eof)) {
        return Ok(());
    }
    if p.peek_is_ident_start() {
        return Ok(());
    }
    Err(CompileError::at(
        ErrorKind::Parse,
        p.pos(),
        "expected ';'",
    ))
}

pub fn parse_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
    if let Some(import_span) = p.eat_kw_span("import") {
        let type_only = p.eat_kw("type");

        if p.eat(TokenKind::LBrace) {
            let mut items: Vec<(String, Option<String>)> = Vec::new();
            loop {
                if p.eat(TokenKind::RBrace) {
                    break;
                }
                let name = p.parse_ident()?;
                let alias = if p.eat_kw("as") {
                    Some(p.parse_ident()?)
                } else {
                    None
                };
                items.push((name, alias));
                if p.eat_char(',') {
                    continue;
                }
                if p.eat(TokenKind::RBrace) {
                    break;
                }
                return Err(CompileError::at(
                    ErrorKind::Parse,
                    p.pos(),
                    "expected ',' or '}' in import list",
                ));
            }
            if !p.eat_kw("from") {
                return Err(CompileError::at(
                    ErrorKind::Parse,
                    p.pos(),
                    "expected 'from' in import",
                ));
            }
            let from = p.parse_dotted_path()?;
            expect_stmt_terminator(p)?;
            let span = Span::new(import_span.start, p.last_span_end());
            return Ok(Some(Stmt::new(
                StmtKind::ImportFrom {
                    type_only,
                    items,
                    from,
                },
                span,
            )));
        }

        let path = p.parse_dotted_path()?;
        if !p.eat_kw("as") {
            return Err(CompileError::at(
                ErrorKind::Parse,
                p.pos(),
                "expected 'as' in import",
            ));
        }
        let alias = p.parse_ident()?;
        expect_stmt_terminator(p)?;
        let span = Span::new(import_span.start, p.last_span_end());
        return Ok(Some(Stmt::new(StmtKind::ImportModule { path, alias }, span)));
    }

    let export_span = p.eat_kw_span("export");
    let exported = export_span.is_some();

    if let Some(proto_span) = p.eat_kw_span("prototype") {
        let name = p.parse_ident()?;
        let mut type_params: Vec<String> = Vec::new();
        if p.eat(TokenKind::Lt) {
            type_params.push(p.parse_ident()?);
            while p.eat_char(',') {
                type_params.push(p.parse_ident()?);
            }
            p.expect_char('>')?;
        }

        p.expect_char('{')?;
        let mut fields: Vec<(String, Expr)> = Vec::new();
        loop {
            if p.eat(TokenKind::RBrace) {
                break;
            }
            let k = p.parse_ident()?;
            p.expect_char(':')?;
            let v = p.parse_expr()?;
            expect_stmt_terminator(p)?;
            fields.push((k, v));
        }
        expect_stmt_terminator(p)?;

        let span = Span::new(
            export_span.map(|s| s.start).unwrap_or(proto_span.start),
            p.last_span_end(),
        );
        return Ok(Some(Stmt::new(
            StmtKind::Prototype {
                exported,
                name,
                type_params,
                fields,
            },
            span,
        )));
    }

    if let Some(let_span) = p.eat_kw_span("let") {
        let name = p.parse_ident()?;
        let mut type_params: Vec<String> = Vec::new();
        if p.eat(TokenKind::Lt) {
            type_params.push(p.parse_ident()?);
            while p.eat_char(',') {
                type_params.push(p.parse_ident()?);
            }
            p.expect_char('>')?;
        }
        let ty = if p.eat_char(':') {
            Some(p.parse_type()?)
        } else {
            None
        };
        p.expect_char('=')?;
        let expr = p.parse_expr()?;
        expect_stmt_terminator(p)?;
        let span = Span::new(
            export_span.map(|s| s.start).unwrap_or(let_span.start),
            p.last_span_end(),
        );
        return Ok(Some(Stmt::new(
            StmtKind::Let {
                exported,
                name,
                type_params,
                ty,
                expr,
            },
            span,
        )));
    }
    if exported {
        return Err(CompileError::at(
            ErrorKind::Parse,
            export_span.unwrap().start,
            "expected 'let' or 'prototype' after 'export'",
        ));
    }
    if let Some(while_span) = p.eat_kw_span("while") {
        let s = parse_while_stmt(p, while_span)?;
        return Ok(Some(s));
    }
    if let Some(break_span) = p.eat_kw_span("break") {
        expect_stmt_terminator(p)?;
        return Ok(Some(Stmt::new(StmtKind::Break, Span::new(break_span.start, p.last_span_end()))));
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
    if let Some(s) = try_parse_assign_stmt(p)? {
        return Ok(Some(s));
    }

    // Expression statement: only consume if a trailing ';' exists.
    let save = p.i;
    let start = p.pos();
    if let Ok(expr) = p.parse_expr() {
        if p.eat(TokenKind::Semicolon) {
            return Ok(Some(Stmt::new(
                StmtKind::Expr { expr },
                Span::new(start, p.last_span_end()),
            )));
        }
    }
    p.i = save;
    Ok(None)
}

fn try_parse_assign_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
    if !p.peek_is_var_name() {
        return Ok(None);
    }
    let save = p.i;
    let name = p.parse_ident()?;
    let name_span = Span::new(p.last_span_start(), p.last_span_end());

    if p.eat_char('.') {
        let field = p.parse_ident()?;
        if !p.eat(TokenKind::Eq) {
            // Not an assignment; treat as an expression statement boundary (eg `Obj.get<T>(...)`).
            p.i = save;
            return Ok(None);
        }
        let expr = p.parse_expr()?;
        expect_stmt_terminator(p)?;
        let base = Expr::new(ExprKind::Var(name), name_span);
        let span = Span::new(name_span.start, p.last_span_end());
        return Ok(Some(Stmt::new(
            StmtKind::MemberAssign {
                base,
                name: field,
                expr,
            },
            span,
        )));
    }

    if p.eat(TokenKind::LBracket) {
        let index = p.parse_expr()?;
        p.expect_char(']')?;
        if !p.eat(TokenKind::Eq) {
            // Not an index assignment; rewind and let it parse as an expression.
            p.i = save;
            return Ok(None);
        }
        let expr = p.parse_expr()?;
        expect_stmt_terminator(p)?;
        let base = Expr::new(ExprKind::Var(name), name_span);
        let span = Span::new(name_span.start, p.last_span_end());
        return Ok(Some(Stmt::new(
            StmtKind::IndexAssign {
                base,
                index,
                expr,
            },
            span,
        )));
    }

    if p.peek_kind() == Some(&TokenKind::EqEq) {
        p.i = save;
        return Ok(None);
    }
    if !p.eat(TokenKind::Eq) {
        p.i = save;
        return Ok(None);
    }
    let expr = p.parse_expr()?;
    expect_stmt_terminator(p)?;
    let span = Span::new(name_span.start, p.last_span_end());
    Ok(Some(Stmt::new(StmtKind::Assign { name, expr }, span)))
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

pub fn parse_block_stmts(p: &mut P) -> Result<Vec<Stmt>, CompileError> {
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
