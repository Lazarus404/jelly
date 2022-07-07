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

// Token-based parser state (peek, bump, expect over tokens).

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::token::{Token, TokenKind};

/// Token-based parser with peek(), bump(), expect() over tokens.
pub struct TokenP {
    pub tokens: Vec<Token>,
    pub i: usize,
}

impl TokenP {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        let eof_pos = tokens.last().map(|t| t.span.end).unwrap_or(0);
        tokens.push(Token::new(TokenKind::Eof, Span::point(eof_pos)));
        Self { tokens, i: 0 }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.i)
    }

    pub fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek().map(|t| &t.kind)
    }

    pub fn bump(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.i)?.clone();
        self.i += 1;
        Some(t)
    }

    pub fn eof(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Eof) | None)
    }

    /// Current byte position (start of current token, or end of input).
    pub fn pos(&self) -> usize {
        self.peek()
            .map(|t| t.span.start)
            .unwrap_or_else(|| self.tokens.last().map(|t| t.span.end).unwrap_or(0))
    }

    /// Start of the last consumed token (for span building).
    pub fn last_span_start(&self) -> usize {
        if self.i > 0 {
            self.tokens[self.i - 1].span.start
        } else {
            self.pos()
        }
    }

    /// End of the last consumed token (for span building). Use after consuming tokens.
    pub fn last_span_end(&self) -> usize {
        if self.i > 0 {
            self.tokens[self.i - 1].span.end
        } else {
            self.pos()
        }
    }

    pub fn err<T>(&self, msg: &str) -> Result<T, CompileError> {
        Err(CompileError::at(ErrorKind::Parse, self.pos(), msg))
    }

    pub fn err_at<T>(&self, at: usize, msg: impl Into<String>) -> Result<T, CompileError> {
        Err(CompileError::at(ErrorKind::Parse, at, msg))
    }

    pub fn expect(&mut self, want: TokenKind) -> Result<Token, CompileError> {
        let t = self.bump().ok_or_else(|| {
            CompileError::at(ErrorKind::Parse, self.pos(), "expected token, got EOF")
        })?;
        if std::mem::discriminant(&t.kind) == std::mem::discriminant(&want) {
            Ok(t)
        } else {
            Err(CompileError::at(
                ErrorKind::Parse,
                t.span.start,
                format!("expected {:?}", want),
            ))
        }
    }

    pub fn expect_punct(&mut self, name: &str, kind: TokenKind) -> Result<(), CompileError> {
        let t = self.bump().ok_or_else(|| {
            CompileError::at(ErrorKind::Parse, self.pos(), format!("expected '{}'", name))
        })?;
        if std::mem::discriminant(&t.kind) == std::mem::discriminant(&kind) {
            Ok(())
        } else {
            Err(CompileError::at(
                ErrorKind::Parse,
                t.span.start,
                format!("expected '{}'", name),
            ))
        }
    }

    pub fn expect_ident(&mut self) -> Result<String, CompileError> {
        let t = self.expect(TokenKind::Ident(String::new()))?;
        match t.kind {
            TokenKind::Ident(s) => Ok(s),
            _ => unreachable!(),
        }
    }

    /// Parse identifier or keyword as a name (for member access, e.g Bytes.new).
    pub fn parse_ident_or_keyword(&mut self) -> Result<String, CompileError> {
        let t = self.bump().ok_or_else(|| {
            CompileError::at(ErrorKind::Parse, self.pos(), "expected identifier")
        })?;
        t.ident_or_keyword_str().ok_or_else(|| {
            CompileError::at(ErrorKind::Parse, t.span.start, "expected identifier")
        })
    }

    pub fn expect_bytes_lit(&mut self) -> Result<Vec<u8>, CompileError> {
        let t = self.expect(TokenKind::BytesLit(vec![]))?;
        match t.kind {
            TokenKind::BytesLit(b) => Ok(b),
            _ => unreachable!(),
        }
    }

    pub fn expect_i32_lit(&mut self) -> Result<i32, CompileError> {
        let t = self.expect(TokenKind::I32Lit(0))?;
        match t.kind {
            TokenKind::I32Lit(v) => Ok(v),
            _ => unreachable!(),
        }
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.peek_kind().map(|k| k == &kind).unwrap_or(false) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn eat_kw_span(&mut self, kw: &str) -> Option<Span> {
        let kind = kw_to_token_kind(kw)?;
        let span = self.peek().map(|t| t.span)?;
        if self.eat(kind) {
            Some(span)
        } else {
            None
        }
    }

    pub fn eat_kw(&mut self, kw: &str) -> bool {
        self.eat_kw_span(kw).is_some()
    }

    /// True only for Ident (variable names), not keywords. Use for assignment LHS.
    pub fn peek_is_var_name(&self) -> bool {
        matches!(self.peek_kind(), Some(TokenKind::Ident(_)))
    }

    pub fn peek_is_ident_start(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(TokenKind::Ident(_))
                | Some(TokenKind::KwLet)
                | Some(TokenKind::KwIf)
                | Some(TokenKind::KwElse)
                | Some(TokenKind::KwWhile)
                | Some(TokenKind::KwBreak)
                | Some(TokenKind::KwContinue)
                | Some(TokenKind::KwReturn)
                | Some(TokenKind::KwTry)
                | Some(TokenKind::KwCatch)
                | Some(TokenKind::KwThrow)
                | Some(TokenKind::KwNew)
                | Some(TokenKind::KwFn)
                | Some(TokenKind::KwMatch)
                | Some(TokenKind::KwWhen)
                | Some(TokenKind::KwPrototype)
                | Some(TokenKind::KwImport)
                | Some(TokenKind::KwExport)
                | Some(TokenKind::KwType)
                | Some(TokenKind::KwAs)
                | Some(TokenKind::KwFrom)
        )
    }

    pub fn expect_char(&mut self, want: char) -> Result<(), CompileError> {
        let kind = char_to_token_kind(want);
        self.expect_punct(&format!("{}", want), kind)
    }

    pub fn eat_char(&mut self, want: char) -> bool {
        let kind = char_to_token_kind(want);
        self.eat(kind)
    }
}

fn char_to_token_kind(c: char) -> TokenKind {
    match c {
        '(' => TokenKind::LParen,
        ')' => TokenKind::RParen,
        '{' => TokenKind::LBrace,
        '}' => TokenKind::RBrace,
        '[' => TokenKind::LBracket,
        ']' => TokenKind::RBracket,
        ';' => TokenKind::Semicolon,
        ':' => TokenKind::Colon,
        ',' => TokenKind::Comma,
        '.' => TokenKind::Dot,
        '=' => TokenKind::Eq,
        '<' => TokenKind::Lt,
        '>' => TokenKind::Gt,
        '+' => TokenKind::Plus,
        '-' => TokenKind::Minus,
        '^' => TokenKind::Caret,
        _ => TokenKind::Semicolon,
    }
}

fn kw_to_token_kind(kw: &str) -> Option<TokenKind> {
    Some(match kw {
        "let" => TokenKind::KwLet,
        "if" => TokenKind::KwIf,
        "else" => TokenKind::KwElse,
        "while" => TokenKind::KwWhile,
        "break" => TokenKind::KwBreak,
        "continue" => TokenKind::KwContinue,
        "return" => TokenKind::KwReturn,
        "try" => TokenKind::KwTry,
        "catch" => TokenKind::KwCatch,
        "throw" => TokenKind::KwThrow,
        "new" => TokenKind::KwNew,
        "fn" => TokenKind::KwFn,
        "match" => TokenKind::KwMatch,
        "when" => TokenKind::KwWhen,
        "prototype" => TokenKind::KwPrototype,
        "import" => TokenKind::KwImport,
        "export" => TokenKind::KwExport,
        "type" => TokenKind::KwType,
        "as" => TokenKind::KwAs,
        "from" => TokenKind::KwFrom,
        "true" => TokenKind::BoolLit(true),
        "false" => TokenKind::BoolLit(false),
        "null" => TokenKind::Null,
        _ => return None,
    })
}
