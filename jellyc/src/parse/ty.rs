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

// Type parsing.

use crate::ast::{Span, Ty, TyKind};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::P;

pub fn parse_type(p: &mut P) -> Result<Ty, CompileError> {
    let left = parse_type_atom(p)?;
    if p.eat(TokenKind::Arrow) {
        let right = parse_type(p)?;
        if let TyKind::Generic { base, args } = left.node.clone() {
            if base == "__args__" {
                let span = Span::new(left.span.start, right.span.end);
                return Ok(Ty::new(TyKind::Fun { args, ret: Box::new(right) }, span));
            }
            let span = Span::new(left.span.start, right.span.end);
            return Ok(Ty::new(
                TyKind::Fun {
                    args: vec![Ty::new(TyKind::Generic { base, args }, left.span)],
                    ret: Box::new(right),
                },
                span,
            ));
        }
        let span = Span::new(left.span.start, right.span.end);
        Ok(Ty::new(TyKind::Fun { args: vec![left], ret: Box::new(right) }, span))
    } else {
        Ok(left)
    }
}

fn parse_type_atom(p: &mut P) -> Result<Ty, CompileError> {
    let start = p.pos();
    if p.eat(TokenKind::LParen) {
        // Zero-arg function type: `() -> T`
        //
        // We encode `()` as a special generic `__args__<>` so the existing
        // arrow parsing can build `TyKind::Fun { args: vec![], ... }`.
        if p.eat(TokenKind::RParen) {
            return Ok(Ty::new(
                TyKind::Generic {
                    base: "__args__".to_string(),
                    args: vec![],
                },
                Span::new(start, p.last_span_end()),
            ));
        }
        let first = parse_type(p)?;
        if p.eat(TokenKind::Comma) {
            let mut args = vec![first];
            loop {
                args.push(parse_type(p)?);
                if !p.eat(TokenKind::Comma) {
                    break;
                }
            }
            p.expect_char(')')?;
            return Ok(Ty::new(
                TyKind::Generic {
                    base: "__args__".to_string(),
                    args,
                },
                Span::new(start, p.last_span_end()),
            ));
        }
        p.expect_char(')')?;
        let mut out = first;
        out.span = Span::new(start, p.last_span_end());
        Ok(out)
    } else {
        let base_start = p.pos();
        let base = p.parse_ident()?;
        let base_end = p.last_span_end();
        if p.eat(TokenKind::Lt) {
            let mut args: Vec<Ty> = Vec::new();
            loop {
                let t = parse_type(p)?;
                args.push(t);
                if !p.eat(TokenKind::Comma) {
                    break;
                }
            }
            p.expect_char('>')?;
            Ok(Ty::new(
                TyKind::Generic { base, args },
                Span::new(base_start, p.last_span_end()),
            ))
        } else {
            Ok(Ty::new(TyKind::Named(base), Span::new(base_start, base_end)))
        }
    }
}
