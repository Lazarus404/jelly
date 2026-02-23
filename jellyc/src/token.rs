#![allow(dead_code)]

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

use crate::ast::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Literals
    BytesLit(Vec<u8>),
    BoolLit(bool),
    I8Lit(i32),
    I16Lit(i32),
    I32Lit(i32),
    I64Lit(i64),
    F16Lit(f32),
    F32Lit(f32),
    F64Lit(f64),
    Null,

    // Identifiers and keywords (keywords are distinguished by kind)
    Ident(String),
    KwLet,
    KwConst,
    KwIf,
    KwElse,
    KwWhile,
    KwBreak,
    KwContinue,
    KwReturn,
    KwTry,
    KwCatch,
    KwThrow,
    KwNew,
    KwFn,
    KwMatch,
    KwWhen,
    KwPrototype,
    KwImport,
    KwExport,
    KwType,
    KwAs,
    KwFrom,

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,
    Eq,
    EqEq,   // ==
    NotEq,  // !=
    Lt,
    Le,     // <=
    Gt,
    Ge,     // >=
    Plus,
    Minus,
    Star,
    Slash,
    Not,
    AmpAmp,
    Pipe,
    PipePipe,
    Caret,
    DotDotDot,
    FatArrow,   // =>
    Arrow,      // -> (for function types)

    // Backtick interpolation
    BacktickStart,
    BacktickLiteral(Vec<u8>),
    InterpolationStart, // ${
    BacktickEnd,
    /// Full backtick string with structure for parser (literal bytes + interpolation expr sources)
    BacktickString(Vec<BacktickPart>),

    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BacktickPart {
    Literal(Vec<u8>),
    Interpolation(String), // expr source to parse
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Check if the token is an identifier or keyword.
    pub fn is_ident_or_kw(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Ident(_)
                | TokenKind::KwLet
                | TokenKind::KwConst
                | TokenKind::KwIf
                | TokenKind::KwElse
                | TokenKind::KwWhile
                | TokenKind::KwBreak
                | TokenKind::KwContinue
                | TokenKind::KwReturn
                | TokenKind::KwTry
                | TokenKind::KwCatch
                | TokenKind::KwThrow
                | TokenKind::KwNew
                | TokenKind::KwFn
                | TokenKind::KwMatch
                | TokenKind::KwWhen
                | TokenKind::KwPrototype
                | TokenKind::KwImport
                | TokenKind::KwExport
                | TokenKind::KwType
                | TokenKind::KwAs
                | TokenKind::KwFrom
        )
    }

    /// Get the string for an identifier token.
    pub fn ident_str(&self) -> Option<&str> {
        if let TokenKind::Ident(s) = &self.kind {
            Some(s)
        } else {
            None
        }
    }

    /// Returns the string for Ident or keyword tokens (for use as member names, eg Bytes.new).
    pub fn ident_or_keyword_str(&self) -> Option<String> {
        match &self.kind {
            TokenKind::Ident(s) => Some(s.clone()),
            TokenKind::KwLet => Some("let".to_string()),
            TokenKind::KwConst => Some("const".to_string()),
            TokenKind::KwIf => Some("if".to_string()),
            TokenKind::KwElse => Some("else".to_string()),
            TokenKind::KwWhile => Some("while".to_string()),
            TokenKind::KwBreak => Some("break".to_string()),
            TokenKind::KwContinue => Some("continue".to_string()),
            TokenKind::KwReturn => Some("return".to_string()),
            TokenKind::KwTry => Some("try".to_string()),
            TokenKind::KwCatch => Some("catch".to_string()),
            TokenKind::KwThrow => Some("throw".to_string()),
            TokenKind::KwNew => Some("new".to_string()),
            TokenKind::KwFn => Some("fn".to_string()),
            TokenKind::KwMatch => Some("match".to_string()),
            TokenKind::KwWhen => Some("when".to_string()),
            TokenKind::KwPrototype => Some("prototype".to_string()),
            TokenKind::KwImport => Some("import".to_string()),
            TokenKind::KwExport => Some("export".to_string()),
            TokenKind::KwType => Some("type".to_string()),
            TokenKind::KwAs => Some("as".to_string()),
            TokenKind::KwFrom => Some("from".to_string()),
            _ => None,
        }
    }
}
