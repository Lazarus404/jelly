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

// Statement parsing.

use crate::ast::{Expr, ExprKind, Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::P;

mod assign;
mod bindings;
mod control;
mod helpers;
mod import;

pub fn parse_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
    if let Some(s) = import::try_parse_import_stmt(p)? {
        return Ok(Some(s));
    }

    // `export` is a modifier for `let` / `const` / `prototype`.
    let export_span = p.eat_kw_span("export");
    let exported = export_span.is_some();
    if let Some(s) = bindings::try_parse_exported_binding_stmt(p, export_span, exported)? {
        return Ok(Some(s));
    }
    if exported {
        return Err(CompileError::at(
            ErrorKind::Parse,
            export_span.unwrap().start,
            "expected 'let', 'const', or 'prototype' after 'export'",
        ));
    }

    if let Some(s) = control::try_parse_control_stmt(p)? {
        return Ok(Some(s));
    }

    if let Some(s) = assign::try_parse_assign_stmt(p)? {
        return Ok(Some(s));
    }

    // Expression statement: require ';' and more input after it. If we have "expr;" at EOF,
    // treat as final expression (not stmt) so REPL shows the value (e.g. "i + 3;" → 5).
    let save = p.i;
    let start = p.pos();
    if let Ok(expr) = p.parse_expr() {
        if p.eat(TokenKind::Semicolon) && !p.eof() {
            return Ok(Some(Stmt::new(
                StmtKind::Expr { expr },
                Span::new(start, p.last_span_end()),
            )));
        }
        // No semicolon: treat as stmt only if next token could start a new stmt (e.g. "i" then "i + 3;").
        if !p.eof() && p.peek_is_ident_start() {
            return Ok(Some(Stmt::new(
                StmtKind::Expr { expr },
                Span::new(start, p.last_span_end()),
            )));
        }
    }
    p.i = save;
    Ok(None)
}

// Keep these imports "used" from this module (they are referenced by submodules).
#[allow(dead_code)]
type _Expr = Expr;
#[allow(dead_code)]
type _ExprKind = ExprKind;
