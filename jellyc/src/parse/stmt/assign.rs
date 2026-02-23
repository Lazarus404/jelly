use crate::ast::{Expr, ExprKind, Span, Stmt, StmtKind};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::helpers::expect_stmt_terminator;
use super::P;

pub(super) fn try_parse_assign_stmt(p: &mut P) -> Result<Option<Stmt>, CompileError> {
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
            StmtKind::IndexAssign { base, index, expr },
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
