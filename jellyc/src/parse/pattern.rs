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

// Pattern parsing for match arms.

use crate::ast::{Pattern, PatternKind, Span};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::P;

pub fn parse_pattern(p: &mut P) -> Result<Pattern, CompileError> {
    let start = p.pos();
    if p.eat(TokenKind::Caret) {
        let name = p.parse_ident()?;
        return Ok(Pattern::new(PatternKind::Pin(name), Span::new(start, p.last_span_end())));
    }
    if p.peek().and_then(|t| t.ident_str()) == Some("_") {
        p.bump();
        return Ok(Pattern::new(PatternKind::Wildcard, Span::new(start, p.last_span_end())));
    }
    if p.peek_kind() == Some(&TokenKind::LBrace) {
        return parse_obj_pattern(p);
    }
    if p.peek_kind() == Some(&TokenKind::LParen) {
        return parse_tuple_pattern(p);
    }
    if p.peek_kind() == Some(&TokenKind::LBracket) {
        return parse_array_pattern(p);
    }
    if let Some(sp) = p.eat_kw_span("true") {
        return Ok(Pattern::new(PatternKind::BoolLit(true), sp));
    }
    if let Some(sp) = p.eat_kw_span("false") {
        return Ok(Pattern::new(PatternKind::BoolLit(false), sp));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I8Lit(_))) == Some(true) {
        let t = p.expect(TokenKind::I8Lit(0))?;
        let v = match t.kind {
            TokenKind::I8Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Pattern::new(PatternKind::I8Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I16Lit(_))) == Some(true) {
        let t = p.expect(TokenKind::I16Lit(0))?;
        let v = match t.kind {
            TokenKind::I16Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Pattern::new(PatternKind::I16Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::Minus | TokenKind::I32Lit(_))) == Some(true) {
        if let Ok((v, span)) = p.parse_i32_lit_signed(start) {
            return Ok(Pattern::new(PatternKind::I32Lit(v), span));
        }
    }
    if p.peek_is_ident_start() {
        let name = p.parse_ident()?;
        return Ok(Pattern::new(PatternKind::Bind(name), Span::new(start, p.last_span_end())));
    }
    p.err("expected match pattern")
}

fn parse_tuple_pattern(p: &mut P) -> Result<Pattern, CompileError> {
    let start = p.pos();
    p.expect_char('(')?;
    if p.eat(TokenKind::RParen) {
        return Ok(Pattern::new(PatternKind::TupleExact(Vec::new()), Span::new(start, p.last_span_end())));
    }

    let first = parse_pattern(p)?;
    if !p.eat_char(',') {
        return Err(CompileError::at(
            ErrorKind::Parse,
            start,
            "tuple pattern requires at least one comma (parenthesized patterns are not supported)",
        ));
    }
    let mut elems: Vec<Pattern> = vec![first];
    elems.push(parse_pattern(p)?);
    while p.eat_char(',') {
        if p.peek_kind() == Some(&TokenKind::RParen) {
            break;
        }
        elems.push(parse_pattern(p)?);
    }
    p.expect_char(')')?;
    Ok(Pattern::new(PatternKind::TupleExact(elems), Span::new(start, p.last_span_end())))
}

fn parse_obj_pattern(p: &mut P) -> Result<Pattern, CompileError> {
    let start = p.pos();
    p.expect_char('{')?;
    let mut fields: Vec<(String, Pattern)> = Vec::new();
    loop {
        if p.eat(TokenKind::RBrace) {
            break;
        }
        let key = p.parse_ident()?;
        p.expect_char(':')?;
        let pat = parse_pattern(p)?;
        fields.push((key, pat));
        p.eat_char(',');
    }
    Ok(Pattern::new(PatternKind::Obj(fields), Span::new(start, p.last_span_end())))
}

fn parse_array_pattern(p: &mut P) -> Result<Pattern, CompileError> {
    let start = p.pos();
    p.expect_char('[')?;
    if p.eat(TokenKind::RBracket) {
        return Ok(Pattern::new(PatternKind::ArrayExact(Vec::new()), Span::new(start, p.last_span_end())));
    }

    let first = parse_pattern(p)?;
    if p.eat(TokenKind::Pipe) {
        let rest = p.parse_ident()?;
        p.expect_char(']')?;
        return Ok(Pattern::new(
            PatternKind::ArrayHeadTail {
                head: Box::new(first),
                rest,
            },
            Span::new(start, p.last_span_end()),
        ));
    }

    let mut prefix: Vec<Pattern> = vec![first];
    loop {
        if p.eat(TokenKind::DotDotDot) {
            let rest = p.parse_ident()?;
            p.expect_char(']')?;
            return Ok(Pattern::new(
                PatternKind::ArrayPrefixRest { prefix, rest },
                Span::new(start, p.last_span_end()),
            ));
        }
        if p.eat(TokenKind::RBracket) {
            return Ok(Pattern::new(PatternKind::ArrayExact(prefix), Span::new(start, p.last_span_end())));
        }
        p.expect_char(',')?;
        if p.eat(TokenKind::DotDotDot) {
            let rest = p.parse_ident()?;
            p.expect_char(']')?;
            return Ok(Pattern::new(
                PatternKind::ArrayPrefixRest { prefix, rest },
                Span::new(start, p.last_span_end()),
            ));
        }
        if p.eat(TokenKind::RBracket) {
            return Ok(Pattern::new(PatternKind::ArrayExact(prefix), Span::new(start, p.last_span_end())));
        }
        prefix.push(parse_pattern(p)?);
    }
}
