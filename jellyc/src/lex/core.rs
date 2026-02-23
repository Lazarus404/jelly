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
use crate::token::{Token, TokenKind};

use super::keywords::KEYWORDS;
use super::Lexer;

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            i: 0,
            brace_depth: 0,
        }
    }

    fn eof(&self) -> bool {
        self.i >= self.src.len()
    }

    pub(super) fn peek_char(&self) -> Option<char> {
        if self.eof() {
            return None;
        }
        self.src[self.i..].chars().next()
    }

    pub(super) fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.i += ch.len_utf8();
        Some(ch)
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), CompileError> {
        loop {
            // Whitespace
            while let Some(ch) = self.peek_char() {
                if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                    self.bump_char();
                } else {
                    break;
                }
            }

            // Line comment: // ... \n
            if self
                .src
                .get(self.i..)
                .map(|s| s.starts_with("//"))
                .unwrap_or(false)
            {
                self.i += 2;
                while let Some(ch) = self.peek_char() {
                    if ch == '\n' || ch == '\r' {
                        break;
                    }
                    self.bump_char();
                }
                continue;
            }

            // Block comment: /* ... */
            if self
                .src
                .get(self.i..)
                .map(|s| s.starts_with("/*"))
                .unwrap_or(false)
            {
                let start = self.i;
                self.i += 2;
                loop {
                    if self.eof() {
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "unterminated block comment",
                        ));
                    }
                    if self
                        .src
                        .get(self.i..)
                        .map(|s| s.starts_with("*/"))
                        .unwrap_or(false)
                    {
                        self.i += 2;
                        break;
                    }
                    self.bump_char();
                }
                continue;
            }

            break;
        }
        Ok(())
    }

    fn is_ident_start(&self, ch: char) -> bool {
        ch == '_' || ch.is_ascii_alphabetic()
    }

    fn is_ident_continue(&self, ch: char) -> bool {
        ch == '_' || ch.is_ascii_alphanumeric()
    }

    pub(super) fn err<T>(&self, msg: &str) -> Result<T, CompileError> {
        Err(CompileError::at(ErrorKind::Parse, self.i, msg))
    }

    fn parse_ident_or_kw(&mut self) -> Result<Token, CompileError> {
        let start = self.i;
        let mut s = String::new();
        if let Some(ch) = self.bump_char() {
            if !self.is_ident_start(ch) {
                return self.err("expected identifier");
            }
            s.push(ch);
        }
        while let Some(ch) = self.peek_char() {
            if !self.is_ident_continue(ch) {
                break;
            }
            s.push(ch);
            self.bump_char();
        }
        for (kw, kind) in KEYWORDS {
            if s == *kw {
                return Ok(Token::new(kind.clone(), Span::new(start, self.i)));
            }
        }
        Ok(Token::new(TokenKind::Ident(s), Span::new(start, self.i)))
    }

    pub(super) fn next_token(&mut self) -> Result<Token, CompileError> {
        self.skip_ws_and_comments()?;
        let start = self.i;
        if self.eof() {
            return Ok(Token::new(TokenKind::Eof, Span::point(start)));
        }
        match self.peek_char() {
            Some('"') | Some('\'') => self.parse_string_lit(),
            Some('`') => self.parse_backtick_interp().map(|t| t.unwrap()),
            Some('(') => {
                self.bump_char();
                Ok(Token::new(TokenKind::LParen, Span::new(start, self.i)))
            }
            Some(')') => {
                self.bump_char();
                Ok(Token::new(TokenKind::RParen, Span::new(start, self.i)))
            }
            Some('{') => {
                self.bump_char();
                Ok(Token::new(TokenKind::LBrace, Span::new(start, self.i)))
            }
            Some('}') => {
                self.bump_char();
                Ok(Token::new(TokenKind::RBrace, Span::new(start, self.i)))
            }
            Some('[') => {
                self.bump_char();
                Ok(Token::new(TokenKind::LBracket, Span::new(start, self.i)))
            }
            Some(']') => {
                self.bump_char();
                Ok(Token::new(TokenKind::RBracket, Span::new(start, self.i)))
            }
            Some(';') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Semicolon, Span::new(start, self.i)))
            }
            Some(':') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Colon, Span::new(start, self.i)))
            }
            Some(',') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Comma, Span::new(start, self.i)))
            }
            Some('.') => {
                self.bump_char();
                if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with(".."))
                    .unwrap_or(false)
                {
                    self.i += 2;
                    Ok(Token::new(TokenKind::DotDotDot, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Dot, Span::new(start, self.i)))
                }
            }
            Some('=') => {
                self.bump_char();
                if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('='))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::EqEq, Span::new(start, self.i)))
                } else if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('>'))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::FatArrow, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Eq, Span::new(start, self.i)))
                }
            }
            Some('!') => {
                self.bump_char();
                if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('='))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::NotEq, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Not, Span::new(start, self.i)))
                }
            }
            Some('<') => {
                self.bump_char();
                if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('-'))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::LtMinus, Span::new(start, self.i)))
                } else if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('<'))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::Shl, Span::new(start, self.i)))
                } else if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('='))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::Le, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Lt, Span::new(start, self.i)))
                }
            }
            Some('>') => {
                self.bump_char();
                if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('>'))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::Shr, Span::new(start, self.i)))
                } else if self
                    .src
                    .get(self.i..)
                    .map(|s| s.starts_with('='))
                    .unwrap_or(false)
                {
                    self.i += 1;
                    Ok(Token::new(TokenKind::Ge, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Gt, Span::new(start, self.i)))
                }
            }
            Some('+') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Plus, Span::new(start, self.i)))
            }
            Some('*') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Star, Span::new(start, self.i)))
            }
            Some('/') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Slash, Span::new(start, self.i)))
            }
            Some('%') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Percent, Span::new(start, self.i)))
            }
            Some('-') => {
                if self
                    .src
                    .get(self.i + 1..)
                    .map(|s| s.starts_with('>'))
                    .unwrap_or(false)
                {
                    self.bump_char();
                    self.bump_char(); // '>'
                    Ok(Token::new(TokenKind::Arrow, Span::new(start, self.i)))
                } else if self
                    .src
                    .get(self.i + 1..)
                    .map(|s| s.chars().next().map(|c| c.is_ascii_digit()))
                    .flatten()
                    .unwrap_or(false)
                {
                    self.parse_negative_number()
                } else {
                    self.bump_char();
                    Ok(Token::new(TokenKind::Minus, Span::new(start, self.i)))
                }
            }
            Some('&') => {
                self.bump_char();
                if self.peek_char() == Some('&') {
                    self.bump_char();
                    Ok(Token::new(TokenKind::AmpAmp, Span::new(start, self.i)))
                } else {
                    self.err("expected '&&'")
                }
            }
            Some('|') => {
                self.bump_char();
                if self.peek_char() == Some('|') {
                    self.bump_char();
                    Ok(Token::new(TokenKind::PipePipe, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Pipe, Span::new(start, self.i)))
                }
            }
            Some('^') => {
                self.bump_char();
                Ok(Token::new(TokenKind::Caret, Span::new(start, self.i)))
            }
            Some('0'..='9') => self.parse_number(),
            Some(_) if self.is_ident_start(self.peek_char().unwrap()) => self.parse_ident_or_kw(),
            _ => self.err("unexpected character"),
        }
    }
}
