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
