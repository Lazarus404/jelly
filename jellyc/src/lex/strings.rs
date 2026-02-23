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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::token::{BacktickPart, Token, TokenKind};

use super::escape::parse_escape_sequence_impl;
use super::Lexer;

impl<'a> Lexer<'a> {
    pub(super) fn parse_string_lit(&mut self) -> Result<Token, CompileError> {
        let start = self.i;
        let quote = if self.bump_char() == Some('"') {
            '"'
        } else {
            '\''
        };
        let mut out: Vec<u8> = Vec::new();
        loop {
            let ch = self.bump_char().ok_or_else(|| {
                CompileError::at(ErrorKind::Parse, self.i, "unterminated string literal")
            })?;
            if ch == quote {
                break;
            }
            if ch == '\\' {
                let esc_at = self.i - 1;
                let (bytes, consumed) = parse_escape_sequence_impl(self.src, self.i, esc_at)?;
                out.extend(bytes);
                self.i += consumed;
                continue;
            }
            if ch == '\n' || ch == '\r' {
                return Err(CompileError::at(
                    ErrorKind::Parse,
                    self.i - ch.len_utf8(),
                    "newline in string literal",
                ));
            }
            let mut buf = [0u8; 4];
            let s = ch.encode_utf8(&mut buf);
            out.extend_from_slice(s.as_bytes());
        }
        Ok(Token::new(
            TokenKind::BytesLit(out),
            Span::new(start, self.i),
        ))
    }

    pub(super) fn parse_backtick_interp(&mut self) -> Result<Option<Token>, CompileError> {
        let start = self.i;
        if self.bump_char() != Some('`') {
            return self.err("expected '`'");
        }
        let mut parts: Vec<BacktickPart> = Vec::new();
        let mut lit: Vec<u8> = Vec::new();
        loop {
            let ch = self.bump_char().ok_or_else(|| {
                CompileError::at(ErrorKind::Parse, self.i, "unterminated backtick string")
            })?;
            if ch == '`' {
                break;
            }
            if ch == '\\' {
                let esc_at = self.i - 1;
                let (bytes, consumed) = parse_escape_sequence_impl(self.src, self.i, esc_at)?;
                lit.extend_from_slice(&bytes);
                self.i += consumed;
                continue;
            }
            if ch == '$' && self.peek_char() == Some('{') {
                self.bump_char(); // '{'
                if !lit.is_empty() {
                    parts.push(BacktickPart::Literal(std::mem::take(&mut lit)));
                }
                let interp_start = self.i;
                let mut depth = 1u32;
                while depth > 0 {
                    let c = self.bump_char().ok_or_else(|| {
                        CompileError::at(
                            ErrorKind::Parse,
                            self.i,
                            "unterminated ${...} in backtick",
                        )
                    })?;
                    match c {
                        '{' => depth += 1,
                        '}' => depth -= 1,
                        _ => {}
                    }
                }
                let interp_end = self.i - 1; // exclude the '}'
                let expr_src = self.src[interp_start..interp_end].to_string();
                parts.push(BacktickPart::Interpolation(expr_src));
                continue;
            }
            if ch == '\n' || ch == '\r' {
                return Err(CompileError::at(
                    ErrorKind::Parse,
                    self.i - ch.len_utf8(),
                    "newline in string literal",
                ));
            }
            let mut buf = [0u8; 4];
            lit.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
        }
        if !lit.is_empty() {
            parts.push(BacktickPart::Literal(lit));
        }
        Ok(Some(Token::new(
            TokenKind::BacktickString(parts),
            Span::new(start, self.i),
        )))
    }
}
