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
