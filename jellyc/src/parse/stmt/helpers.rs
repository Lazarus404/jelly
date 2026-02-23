use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::P;

pub(super) fn expect_stmt_terminator(p: &mut P) -> Result<(), CompileError> {
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
    if matches!(
        p.peek_kind(),
        Some(TokenKind::RBrace) | Some(TokenKind::Eof)
    ) {
        return Ok(());
    }
    if p.peek_is_ident_start() {
        return Ok(());
    }
    Err(CompileError::at(ErrorKind::Parse, p.pos(), "expected ';'"))
}
