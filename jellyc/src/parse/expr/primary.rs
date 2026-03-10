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

use crate::ast::{Expr, ExprKind, Span, Stmt, Ty};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::super::{pattern, P};

use super::{control, fn_expr, literals};

pub(super) fn parse_primary(p: &mut P) -> Result<Expr, CompileError> {
    if let Some(let_span) = p.eat_kw_span("let") {
        return parse_let_expr(p, let_span);
    }
    if let Some(fn_span) = p.eat_kw_span("fn") {
        return fn_expr::parse_fn_expr(p, fn_span);
    }
    if p.peek_kind() == Some(&TokenKind::Colon) {
        let start = p.pos();
        p.expect_char(':')?;
        let name = p.parse_ident_or_keyword()?;
        return Ok(Expr::new(
            ExprKind::AtomLit(name),
            Span::new(start, p.last_span_end()),
        ));
    }
    if let Some(if_span) = p.eat_kw_span("if") {
        return control::parse_if_expr(p, if_span);
    }
    if let Some(match_span) = p.eat_kw_span("match") {
        return control::parse_match_expr(p, match_span);
    }
    if let Some(kw_span) = p.eat_kw_span("true") {
        return Ok(Expr::new(ExprKind::BoolLit(true), kw_span));
    }
    if let Some(kw_span) = p.eat_kw_span("false") {
        return Ok(Expr::new(ExprKind::BoolLit(false), kw_span));
    }
    if let Some(kw_span) = p.eat_kw_span("null") {
        return Ok(Expr::new(ExprKind::Null, kw_span));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I8Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::I8Lit(0))?;
        let v = match t.kind {
            TokenKind::I8Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Expr::new(
            ExprKind::I8Lit(v),
            Span::new(start, p.last_span_end()),
        ));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I16Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::I16Lit(0))?;
        let v = match t.kind {
            TokenKind::I16Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Expr::new(
            ExprKind::I16Lit(v),
            Span::new(start, p.last_span_end()),
        ));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I32Lit(_))) == Some(true) {
        let start = p.pos();
        let (v, span) = p.parse_i32_lit_signed(start)?;
        return Ok(Expr::new(ExprKind::I32Lit(v), span));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I64Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::I64Lit(0))?;
        let v = match t.kind {
            TokenKind::I64Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Expr::new(
            ExprKind::I64Lit(v),
            Span::new(start, p.last_span_end()),
        ));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::F16Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::F16Lit(0.0))?;
        let v = match t.kind {
            TokenKind::F16Lit(x) => x,
            _ => unreachable!(),
        };
        return Ok(Expr::new(
            ExprKind::F16Lit(v),
            Span::new(start, p.last_span_end()),
        ));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::F64Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::F64Lit(0.0))?;
        let v = match t.kind {
            TokenKind::F64Lit(x) => x,
            _ => unreachable!(),
        };
        return Ok(Expr::new(
            ExprKind::F64Lit(v),
            Span::new(start, p.last_span_end()),
        ));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::BytesLit(_))) == Some(true) {
        return literals::parse_bytes_lit_expr(p);
    }
    if let Some(TokenKind::BacktickString(_)) = p.peek_kind() {
        return literals::parse_backtick_interp(p);
    }
    if p.peek_kind() == Some(&TokenKind::LParen) {
        return parse_paren_expr(p);
    }
    if p.peek_kind() == Some(&TokenKind::LBrace) {
        return parse_brace_expr(p);
    }
    if p.peek_kind() == Some(&TokenKind::LBracket) {
        return literals::parse_array_lit(p, p.pos());
    }
    let start = p.pos();
    let name = p.parse_ident()?;
    Ok(Expr::new(
        ExprKind::Var(name),
        Span::new(start, p.last_span_end()),
    ))
}

fn parse_let_expr(p: &mut P, let_span: Span) -> Result<Expr, CompileError> {
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
    let span = Span::new(let_span.start, p.last_span_end());
    Ok(Expr::new(
        ExprKind::Let {
            is_const: false,
            name,
            type_params,
            ty,
            expr: Box::new(expr),
        },
        span,
    ))
}

fn parse_paren_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    p.expect_char('(')?;
    if p.eat(TokenKind::RParen) {
        return Ok(Expr::new(
            ExprKind::TupleLit(vec![]),
            Span::new(start, p.last_span_end()),
        ));
    }

    let first = p.parse_expr()?;
    if p.eat_char(',') {
        let mut elems: Vec<Expr> = vec![first];
        if p.peek_kind() != Some(&TokenKind::RParen) {
            loop {
                let e = p.parse_expr()?;
                elems.push(e);
                if p.eat_char(',') {
                    if p.peek_kind() == Some(&TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        p.expect_char(')')?;
        return Ok(Expr::new(
            ExprKind::TupleLit(elems),
            Span::new(start, p.last_span_end()),
        ));
    }

    p.expect_char(')')?;
    let mut e = first;
    e.span = Span::new(start, p.last_span_end());
    Ok(e)
}

pub(super) fn parse_block_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    p.expect_char('{')?;
    let mut stmts: Vec<Stmt> = Vec::new();
    loop {
        if p.peek_kind() == Some(&TokenKind::RBrace) {
            return p.err("expected expression before '}'");
        }
        let save = p.i;
        if let Some(s) = p.parse_stmt()? {
            stmts.push(s);
            continue;
        }
        p.i = save;
        let e = p.parse_expr()?;
        p.expect_char('}')?;
        let span = Span::new(start, p.last_span_end());
        return Ok(Expr::new(
            ExprKind::Block {
                stmts,
                expr: Box::new(e),
            },
            span,
        ));
    }
}

fn parse_brace_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    let save = p.i;
    p.expect_char('{')?;
    if p.eat(TokenKind::RBrace) {
        return Ok(Expr::new(
            ExprKind::ObjLit(vec![]),
            Span::new(start, p.last_span_end()),
        ));
    }
    if p.peek_is_ident_start() {
        let key = p.parse_ident_or_keyword()?;
        if p.eat_char(':') {
            let mut fields: Vec<(String, Expr)> = Vec::new();
            let v = p.parse_expr()?;
            fields.push((key, v));
            loop {
                if p.eat_char(',') {
                    let k = p.parse_ident_or_keyword()?;
                    p.expect_char(':')?;
                    let vv = p.parse_expr()?;
                    fields.push((k, vv));
                    continue;
                }
                break;
            }
            p.expect_char('}')?;
            return Ok(Expr::new(
                ExprKind::ObjLit(fields),
                Span::new(start, p.last_span_end()),
            ));
        }
    }
    p.i = save;
    let mut e = parse_block_expr(p)?;
    e.span = Span::new(start, e.span.end);
    Ok(e)
}

#[allow(dead_code)]
fn _pattern_marker(_p: &mut P) -> Result<(), CompileError> {
    // keep `pattern` import used when compiling with older rustc lints
    let _ = pattern::parse_pattern;
    Ok(())
}

#[allow(dead_code)]
type _Ty = Ty;
