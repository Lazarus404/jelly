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

use crate::ast::{Expr, ExprKind, Span};
use crate::error::CompileError;
use crate::token::{BacktickPart, TokenKind};

use super::super::P;

pub(super) fn parse_bytes_lit_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    let b = p.expect_bytes_lit()?;
    Ok(Expr::new(
        ExprKind::BytesLit(b),
        Span::new(start, p.last_span_end()),
    ))
}

pub(super) fn parse_array_lit(p: &mut P, start: usize) -> Result<Expr, CompileError> {
    p.expect_char('[')?;
    let mut elems: Vec<Expr> = Vec::new();
    if p.peek_kind() != Some(&TokenKind::RBracket) {
        loop {
            let e = p.parse_expr()?;
            elems.push(e);
            if !p.eat_char(',') {
                break;
            }
        }
    }
    p.expect_char(']')?;
    Ok(Expr::new(
        ExprKind::ArrayLit(elems),
        Span::new(start, p.last_span_end()),
    ))
}

pub(super) fn parse_backtick_interp(p: &mut P) -> Result<Expr, CompileError> {
    let t = match p.bump() {
        Some(t) => t,
        None => return p.err("expected backtick string"),
    };
    let (parts, start) = match &t.kind {
        TokenKind::BacktickString(parts) => (parts.clone(), t.span.start),
        _ => return p.err("expected backtick string"),
    };

    let mut exprs: Vec<Expr> = Vec::new();
    let mut lit: Vec<u8> = Vec::new();
    let mut lit_start = start;

    let flush_lit = |exprs: &mut Vec<Expr>, lit: &mut Vec<u8>, lit_start: usize, lit_end: usize| {
        if !lit.is_empty() {
            let bytes = std::mem::take(lit);
            exprs.push(Expr::new(
                ExprKind::BytesLit(bytes),
                Span::new(lit_start, lit_end),
            ));
        }
    };

    let mut pos = start;
    for part in &parts {
        match part {
            BacktickPart::Literal(bytes) => {
                if !bytes.is_empty() {
                    let end = pos + bytes.len();
                    exprs.push(Expr::new(
                        ExprKind::BytesLit(bytes.clone()),
                        Span::new(pos, end),
                    ));
                    pos = end;
                }
            }
            BacktickPart::Interpolation(expr_src) => {
                flush_lit(&mut exprs, &mut lit, lit_start, pos);
                let e = p.parse_expr_from_src(expr_src)?;
                pos = e.span.end;
                lit_start = pos;
                match e.node {
                    ExprKind::BytesLit(b) => {
                        lit.extend_from_slice(&b);
                    }
                    _ => {
                        exprs.push(e);
                    }
                }
            }
        }
    }
    flush_lit(&mut exprs, &mut lit, lit_start, t.span.end);

    if exprs.is_empty() {
        return Ok(Expr::new(ExprKind::BytesLit(Vec::new()), t.span));
    }
    let mut it = exprs.into_iter();
    let mut acc = it.next().expect("non-empty parts");
    for part in it {
        let span = Span::new(acc.span.start, part.span.end);
        acc = match (acc.node, part.node) {
            (ExprKind::BytesLit(mut a), ExprKind::BytesLit(b)) => {
                a.extend_from_slice(&b);
                Expr::new(ExprKind::BytesLit(a), span)
            }
            (a, b) => Expr::new(
                ExprKind::Add(
                    Box::new(Expr::new(a, acc.span)),
                    Box::new(Expr::new(b, part.span)),
                ),
                span,
            ),
        };
    }
    acc.span = t.span;
    Ok(acc)
}
