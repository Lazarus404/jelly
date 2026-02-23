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
