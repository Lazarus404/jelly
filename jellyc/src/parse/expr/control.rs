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

use crate::ast::{Expr, ExprKind, MatchArm, Pattern, Span, Stmt};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::super::{pattern, P};

use super::fn_expr;
use super::primary;

pub(super) fn parse_try_expr(p: &mut P, try_span: Span) -> Result<Expr, CompileError> {
    let body = primary::parse_block_expr(p)?;
    let _catch_span = p
        .eat_kw_span("catch")
        .ok_or_else(|| CompileError::at(ErrorKind::Parse, p.pos(), "expected 'catch'"))?;
    let catch_name = if p.eat(TokenKind::LParen) {
        let name = p.parse_ident()?;
        p.expect_char(')')?;
        Some(name)
    } else {
        None
    };
    let catch_body = primary::parse_block_expr(p)?;
    let span = Span::new(try_span.start, catch_body.span.end);
    Ok(Expr::new(
        ExprKind::Try {
            body: Box::new(body),
            catch_name,
            catch_body: Box::new(catch_body),
        },
        span,
    ))
}

pub(super) fn parse_if_expr(p: &mut P, if_span: Span) -> Result<Expr, CompileError> {
    p.expect_char('(')?;
    let cond = p.parse_expr()?;
    p.expect_char(')')?;
    let then_br = primary::parse_block_expr(p)?;
    let _else_span = p
        .eat_kw_span("else")
        .ok_or_else(|| CompileError::at(ErrorKind::Parse, p.pos(), "expected 'else'"))?;
    let else_br = primary::parse_block_expr(p)?;
    let span = Span::new(if_span.start, else_br.span.end);
    Ok(Expr::new(
        ExprKind::If {
            cond: Box::new(cond),
            then_br: Box::new(then_br),
            else_br: Box::new(else_br),
        },
        span,
    ))
}

pub(super) fn parse_with_expr(p: &mut P, with_span: Span) -> Result<Expr, CompileError> {
    let mut clauses: Vec<(Pattern, Expr)> = Vec::new();
    loop {
        let pat = pattern::parse_pattern(p)?;
        if !p.eat(TokenKind::LtMinus) {
            return p.err("expected '<-' in with clause");
        }
        let expr = p.parse_expr()?;
        clauses.push((pat, expr));
        if !p.eat_char(',') {
            break;
        }
    }
    if clauses.is_empty() {
        return p.err("with must have at least one clause");
    }
    let body = primary::parse_block_expr(p)?;
    let else_arms = if p.eat_kw_span("else").is_some() {
        p.expect_char('{')?;
        let mut arms: Vec<MatchArm> = Vec::new();
        loop {
            if p.eat(TokenKind::RBrace) {
                break;
            }
            let pat = pattern::parse_pattern(p)?;
            let when = if p.eat_kw_span("when").is_some() {
                p.expect_char('(')?;
                let w = p.parse_expr()?;
                p.expect_char(')')?;
                Some(w)
            } else {
                None
            };
            let (body_stmts, tail) = if p.peek_kind() == Some(&TokenKind::Comma)
                || p.peek_kind() == Some(&TokenKind::RBrace)
            {
                (Vec::new(), None)
            } else {
                if !p.eat(TokenKind::FatArrow) {
                    return p.err("expected '=>' or ','");
                }
                if p.peek_kind() == Some(&TokenKind::LBrace) {
                    fn_expr::parse_fn_body(p)?
                } else {
                    let e = p.parse_expr()?;
                    (Vec::new(), Some(e))
                }
            };
            arms.push(MatchArm {
                pat,
                when,
                body: body_stmts,
                tail,
            });
            p.eat_char(',');
        }
        Some(arms)
    } else {
        None
    };
    let end = if let Some(ref _arms) = else_arms {
        p.last_span_end()
    } else {
        body.span.end
    };
    let span = Span::new(with_span.start, end);
    Ok(Expr::new(
        ExprKind::With {
            clauses,
            body: Box::new(body),
            else_arms,
        },
        span,
    ))
}

pub(super) fn parse_match_expr(p: &mut P, match_span: Span) -> Result<Expr, CompileError> {
    p.expect_char('(')?;
    let subject = p.parse_expr()?;
    p.expect_char(')')?;
    p.expect_char('{')?;
    let mut arms: Vec<MatchArm> = Vec::new();
    loop {
        if p.eat(TokenKind::RBrace) {
            break;
        }
        let pat = pattern::parse_pattern(p)?;
        let when = if p.eat_kw_span("when").is_some() {
            p.expect_char('(')?;
            let w = p.parse_expr()?;
            p.expect_char(')')?;
            Some(w)
        } else {
            None
        };
        let (body, tail) = if p.peek_kind() == Some(&TokenKind::Comma)
            || p.peek_kind() == Some(&TokenKind::RBrace)
        {
            // Fallthrough arm: pattern followed by comma (no =>, falls through to next arm)
            (Vec::new(), None)
        } else {
            if !p.eat(TokenKind::FatArrow) {
                return p.err("expected '=>' or ','");
            }
            if p.peek_kind() == Some(&TokenKind::LBrace) {
                // Full arm body: `=> { stmt* expr? }`
                fn_expr::parse_fn_body(p)?
            } else {
                // Expression-only arm: `=> expr`
                let e = p.parse_expr()?;
                (Vec::new(), Some(e))
            }
        };
        arms.push(MatchArm {
            pat,
            when,
            body,
            tail,
        });
        p.eat_char(',');
    }
    Ok(Expr::new(
        ExprKind::Match {
            subject: Box::new(subject),
            arms,
        },
        Span::new(match_span.start, p.last_span_end()),
    ))
}

// Re-export used type to keep imports localized (avoid warnings).
#[allow(dead_code)]
type _Stmt = Stmt;
