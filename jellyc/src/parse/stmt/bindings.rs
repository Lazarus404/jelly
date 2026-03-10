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

use crate::ast::{Expr, Span, Stmt, StmtKind};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::helpers::expect_stmt_terminator;
use super::P;

pub(super) fn try_parse_exported_binding_stmt(
    p: &mut P,
    export_span: Option<Span>,
    exported: bool,
) -> Result<Option<Stmt>, CompileError> {
    if let Some(proto_span) = p.eat_kw_span("prototype") {
        return Ok(Some(parse_prototype_stmt(
            p,
            export_span,
            exported,
            proto_span,
        )?));
    }

    let let_span = p.eat_kw_span("let");
    let const_span = if let_span.is_none() {
        p.eat_kw_span("const")
    } else {
        None
    };
    if let Some(bind_span) = let_span.or(const_span) {
        let is_const = const_span.is_some();
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
            export_span.map(|s| s.start).unwrap_or(bind_span.start),
            p.last_span_end(),
        );
        return Ok(Some(Stmt::new(
            StmtKind::Let {
                is_const,
                exported,
                name,
                type_params,
                ty,
                expr,
            },
            span,
        )));
    }

    Ok(None)
}

fn parse_prototype_stmt(
    p: &mut P,
    export_span: Option<Span>,
    exported: bool,
    proto_span: Span,
) -> Result<Stmt, CompileError> {
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
    Ok(Stmt::new(
        StmtKind::Prototype {
            exported,
            name,
            type_params,
            fields,
        },
        span,
    ))
}
