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
