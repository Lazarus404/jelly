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

pub(super) fn try_parse_import_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
    let Some(import_span) = p.eat_kw_span("import") else {
        return Ok(None);
    };

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
    Ok(Some(Stmt::new(
        StmtKind::ImportModule { path, alias },
        span,
    )))
}
