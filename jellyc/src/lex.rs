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
use crate::error::{CompileError, ErrorKind};
use crate::token::{BacktickPart, Token, TokenKind};

enum IntSuffix {
    I8,
    I16,
    None,
}

/// Parse a single escape sequence starting after `\`. Returns the decoded bytes and the
/// number of source bytes consumed (excluding the leading `\`).
pub fn parse_escape_sequence(src: &str, i: usize, esc_at: usize) -> Result<(Vec<u8>, usize), CompileError> {
    let mut j = i;
    let ch = src[j..].chars().next().ok_or_else(|| {
        CompileError::at(ErrorKind::Parse, j, "unterminated escape")
    })?;
    j += ch.len_utf8();

    let (bytes, consumed) = match ch {
        '\\' => (vec![b'\\'], ch.len_utf8()),
        'n' => (vec![b'\n'], ch.len_utf8()),
        'r' => (vec![b'\r'], ch.len_utf8()),
        't' => (vec![b'\t'], ch.len_utf8()),
        '0' => (vec![0], ch.len_utf8()),
        '\'' => (vec![b'\''], ch.len_utf8()),
        '"' => (vec![b'"'], ch.len_utf8()),
        '`' => (vec![b'`'], ch.len_utf8()),
        'u' => {
            let (cp, _) = if src.get(j..).map(|s| s.starts_with('{')).unwrap_or(false) {
                j += 1;
                let start = j;
                let mut cp: u32 = 0;
                let mut digits = 0u32;
                while let Some(hc) = src[j..].chars().next() {
                    if hc == '}' {
                        break;
                    }
                    let h = hex_val(hc).ok_or_else(|| {
                        CompileError::at(ErrorKind::Parse, j, "invalid hex digit in \\u{...}")
                    })?;
                    if digits >= 6 {
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "too many hex digits in \\u{...}",
                        ));
                    }
                    cp = (cp << 4) | h;
                    digits += 1;
                    j += hc.len_utf8();
                }
                if src.get(j..).and_then(|s| s.chars().next()) != Some('}') {
                    return Err(CompileError::at(
                        ErrorKind::Parse,
                        start,
                        "unterminated \\u{...}",
                    ));
                }
                j += 1;
                if digits == 0 {
                    return Err(CompileError::at(ErrorKind::Parse, start, "empty \\u{} escape"));
                }
                (cp, 0)
            } else {
                let mut v = 0u32;
                for _ in 0..4 {
                    let ch = src[j..].chars().next().ok_or_else(|| {
                        CompileError::at(ErrorKind::Parse, j, "unexpected EOF in \\uXXXX")
                    })?;
                    let h = hex_val(ch).ok_or_else(|| {
                        CompileError::at(
                            ErrorKind::Parse,
                            j,
                            format!("invalid hex digit '{}' in \\uXXXX", ch),
                        )
                    })?;
                    v = (v << 4) | h;
                    j += ch.len_utf8();
                }
                (v, j - i - 1)
            };
            let mut out = Vec::new();
            push_scalar_utf8(&mut out, cp, esc_at)?;
            (out, j - i)
        }
        _ => return Err(CompileError::at(ErrorKind::Parse, esc_at, format!("unknown escape \\{}", ch))),
    };
    Ok((bytes, consumed))
}

/// Convert a hexadecimal character to a value.
fn hex_val(ch: char) -> Option<u32> {
    match ch {
        '0'..='9' => Some((ch as u32) - ('0' as u32)),
        'a'..='f' => Some((ch as u32) - ('a' as u32) + 10),
        'A'..='F' => Some((ch as u32) - ('A' as u32) + 10),
        _ => None,
    }
}

fn push_scalar_utf8(out: &mut Vec<u8>, cp: u32, at: usize) -> Result<(), CompileError> {
    if (0xD800..=0xDFFF).contains(&cp) {
        return Err(CompileError::at(ErrorKind::Parse, at, "invalid Unicode scalar (surrogate)"));
    }
    if cp > 0x10FFFF {
        return Err(CompileError::at(ErrorKind::Parse, at, "invalid Unicode scalar (>U+10FFFF)"));
    }
    let ch = char::from_u32(cp)
        .ok_or_else(|| CompileError::at(ErrorKind::Parse, at, "invalid Unicode scalar"))?;
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    out.extend_from_slice(s.as_bytes());
    Ok(())
}

const KEYWORDS: &[(&str, TokenKind)] = &[
    ("let", TokenKind::KwLet),
    ("if", TokenKind::KwIf),
    ("else", TokenKind::KwElse),
    ("while", TokenKind::KwWhile),
    ("break", TokenKind::KwBreak),
    ("continue", TokenKind::KwContinue),
    ("return", TokenKind::KwReturn),
    ("try", TokenKind::KwTry),
    ("catch", TokenKind::KwCatch),
    ("throw", TokenKind::KwThrow),
    ("new", TokenKind::KwNew),
    ("null", TokenKind::Null),
    ("true", TokenKind::BoolLit(true)),
    ("false", TokenKind::BoolLit(false)),
    ("fn", TokenKind::KwFn),
    ("match", TokenKind::KwMatch),
    ("when", TokenKind::KwWhen),
    ("prototype", TokenKind::KwPrototype),
    ("import", TokenKind::KwImport),
    ("export", TokenKind::KwExport),
    ("type", TokenKind::KwType),
    ("as", TokenKind::KwAs),
    ("from", TokenKind::KwFrom),
];

pub struct Lexer<'a> {
    src: &'a str,
    i: usize,
    /// When we're inside backtick interpolation, we need to track brace depth.
    brace_depth: i32,
}

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

    fn peek_char(&self) -> Option<char> {
        if self.eof() {
            return None;
        }
        self.src[self.i..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
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
            if self.src.get(self.i..).map(|s| s.starts_with("//")).unwrap_or(false) {
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
            if self.src.get(self.i..).map(|s| s.starts_with("/*")).unwrap_or(false) {
                let start = self.i;
                self.i += 2;
                loop {
                    if self.eof() {
                        return Err(CompileError::at(ErrorKind::Parse, start, "unterminated block comment"));
                    }
                    if self.src.get(self.i..).map(|s| s.starts_with("*/")).unwrap_or(false) {
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

    fn parse_string_lit(&mut self) -> Result<Token, CompileError> {
        let start = self.i;
        let quote = if self.bump_char() == Some('"') {
            '"'
        } else {
            '\''
        };
        let mut out: Vec<u8> = Vec::new();
        loop {
            let ch = self
                .bump_char()
                .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unterminated string literal"))?;
            if ch == quote {
                break;
            }
            if ch == '\\' {
                let esc_at = self.i - 1;
                let (bytes, consumed) = parse_escape_sequence(self.src, self.i, esc_at)?;
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
        Ok(Token::new(TokenKind::BytesLit(out), Span::new(start, self.i)))
    }

    fn parse_backtick_interp(&mut self) -> Result<Option<Token>, CompileError> {
        let start = self.i;
        if self.bump_char() != Some('`') {
            return self.err("expected '`'");
        }
        let mut parts: Vec<BacktickPart> = Vec::new();
        let mut lit: Vec<u8> = Vec::new();
        loop {
            let ch = self
                .bump_char()
                .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unterminated backtick string"))?;
            if ch == '`' {
                break;
            }
            if ch == '\\' {
                let esc_at = self.i - 1;
                let (bytes, consumed) = parse_escape_sequence(self.src, self.i, esc_at)?;
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
                    let c = self
                        .bump_char()
                        .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unterminated ${...} in backtick"))?;
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

    fn err<T>(&self, msg: &str) -> Result<T, CompileError> {
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

    fn try_int_suffix(&mut self) -> Option<IntSuffix> {
        let rest = self.src.get(self.i..)?;
        if rest.starts_with("i16") && !rest.get(3..).and_then(|s| s.chars().next()).map(|c| c.is_ascii_alphanumeric() || c == '_').unwrap_or(false) {
            self.i += 3;
            return Some(IntSuffix::I16);
        }
        if rest.starts_with("i8") && !rest.get(2..).and_then(|s| s.chars().next()).map(|c| c.is_ascii_alphanumeric() || c == '_').unwrap_or(false) {
            self.i += 2;
            return Some(IntSuffix::I8);
        }
        None
    }

    fn try_float_suffix(&mut self) -> bool {
        let rest = self.src.get(self.i..).unwrap_or("");
        if rest.starts_with("f16") && !rest.get(3..).and_then(|s| s.chars().next()).map(|c| c.is_ascii_alphanumeric() || c == '_').unwrap_or(false) {
            self.i += 3;
            return true;
        }
        false
    }

    fn parse_number(&mut self) -> Result<Token, CompileError> {
        let start = self.i;
        let mut s = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                s.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }
        // Float literal: digits '.' digits (optionally with exponent).
        if self.peek_char() == Some('.')
            && self
                .src
                .get(self.i + 1..)
                .and_then(|s| s.chars().next())
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
        {
            s.push('.');
            self.bump_char(); // '.'
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    s.push(ch);
                    self.bump_char();
                } else {
                    break;
                }
            }
            // Optional exponent: e[+/-]?digits
            if matches!(self.peek_char(), Some('e') | Some('E')) {
                let save = self.i;
                let mut exp = String::new();
                exp.push(self.bump_char().unwrap());
                if matches!(self.peek_char(), Some('+') | Some('-')) {
                    exp.push(self.bump_char().unwrap());
                }
                let mut any = false;
                while let Some(ch) = self.peek_char() {
                    if ch.is_ascii_digit() {
                        any = true;
                        exp.push(ch);
                        self.bump_char();
                    } else {
                        break;
                    }
                }
                if any {
                    s.push_str(&exp);
                } else {
                    // Not a real exponent; rewind to before 'e'.
                    self.i = save;
                }
            }
            let v: f64 = s
                .parse()
                .map_err(|_| CompileError::at(ErrorKind::Parse, start, "invalid float literal"))?;
            if self.try_float_suffix() {
                return Ok(Token::new(TokenKind::F16Lit(v as f32), Span::new(start, self.i)));
            }
            return Ok(Token::new(TokenKind::F64Lit(v), Span::new(start, self.i)));
        }

        let v: i64 = s
            .parse()
            .map_err(|_| CompileError::at(ErrorKind::Parse, start, "invalid integer literal"))?;
        if let Some(suffix) = self.try_int_suffix() {
            match suffix {
                IntSuffix::I8 => {
                    if v < -128 || v > 127 {
                        return Err(CompileError::at(ErrorKind::Parse, start, "i8 literal out of range -128..127"));
                    }
                    return Ok(Token::new(TokenKind::I8Lit(v as i32), Span::new(start, self.i)));
                }
                IntSuffix::I16 => {
                    if v < -32768 || v > 32767 {
                        return Err(CompileError::at(ErrorKind::Parse, start, "i16 literal out of range -32768..32767"));
                    }
                    return Ok(Token::new(TokenKind::I16Lit(v as i32), Span::new(start, self.i)));
                }
                IntSuffix::None => {}
            }
        }
        if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
            Ok(Token::new(TokenKind::I32Lit(v as i32), Span::new(start, self.i)))
        } else {
            Ok(Token::new(TokenKind::I64Lit(v), Span::new(start, self.i)))
        }
    }

    fn parse_negative_number(&mut self) -> Result<Token, CompileError> {
        let start = self.i;
        self.bump_char(); // '-'
        let num_start = self.i;
        let mut s = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() {
                s.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }
        // Float literal: -digits '.' digits (optionally with exponent).
        if self.peek_char() == Some('.')
            && self
                .src
                .get(self.i + 1..)
                .and_then(|s| s.chars().next())
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false)
        {
            s.push('.');
            self.bump_char(); // '.'
            while let Some(ch) = self.peek_char() {
                if ch.is_ascii_digit() {
                    s.push(ch);
                    self.bump_char();
                } else {
                    break;
                }
            }
            if matches!(self.peek_char(), Some('e') | Some('E')) {
                let save = self.i;
                let mut exp = String::new();
                exp.push(self.bump_char().unwrap());
                if matches!(self.peek_char(), Some('+') | Some('-')) {
                    exp.push(self.bump_char().unwrap());
                }
                let mut any = false;
                while let Some(ch) = self.peek_char() {
                    if ch.is_ascii_digit() {
                        any = true;
                        exp.push(ch);
                        self.bump_char();
                    } else {
                        break;
                    }
                }
                if any {
                    s.push_str(&exp);
                } else {
                    self.i = save;
                }
            }
            let v: f64 = s
                .parse()
                .map_err(|_| CompileError::at(ErrorKind::Parse, num_start, "invalid float literal"))?;
            if self.try_float_suffix() {
                return Ok(Token::new(TokenKind::F16Lit((-v) as f32), Span::new(start, self.i)));
            }
            return Ok(Token::new(TokenKind::F64Lit(-v), Span::new(start, self.i)));
        }

        let v: i64 = s
            .parse()
            .map_err(|_| CompileError::at(ErrorKind::Parse, num_start, "invalid integer literal"))?;
        let nv: i64 = -v;
        if let Some(suffix) = self.try_int_suffix() {
            match suffix {
                IntSuffix::I8 => {
                    if nv < -128 || nv > 127 {
                        return Err(CompileError::at(ErrorKind::Parse, start, "i8 literal out of range -128..127"));
                    }
                    return Ok(Token::new(TokenKind::I8Lit(nv as i32), Span::new(start, self.i)));
                }
                IntSuffix::I16 => {
                    if nv < -32768 || nv > 32767 {
                        return Err(CompileError::at(ErrorKind::Parse, start, "i16 literal out of range -32768..32767"));
                    }
                    return Ok(Token::new(TokenKind::I16Lit(nv as i32), Span::new(start, self.i)));
                }
                IntSuffix::None => {}
            }
        }
        if nv >= i32::MIN as i64 && nv <= i32::MAX as i64 {
            Ok(Token::new(TokenKind::I32Lit(nv as i32), Span::new(start, self.i)))
        } else {
            Ok(Token::new(TokenKind::I64Lit(nv), Span::new(start, self.i)))
        }
    }

    fn next_token(&mut self) -> Result<Token, CompileError> {
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
                if self.src.get(self.i..).map(|s| s.starts_with("..")).unwrap_or(false) {
                    self.i += 2;
                    Ok(Token::new(TokenKind::DotDotDot, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Dot, Span::new(start, self.i)))
                }
            }
            Some('=') => {
                self.bump_char();
                if self.src.get(self.i..).map(|s| s.starts_with('=')).unwrap_or(false) {
                    self.i += 1;
                    Ok(Token::new(TokenKind::EqEq, Span::new(start, self.i)))
                } else if self.src.get(self.i..).map(|s| s.starts_with('>')).unwrap_or(false) {
                    self.i += 1;
                    Ok(Token::new(TokenKind::FatArrow, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Eq, Span::new(start, self.i)))
                }
            }
            Some('!') => {
                self.bump_char();
                if self.src.get(self.i..).map(|s| s.starts_with('=')).unwrap_or(false) {
                    self.i += 1;
                    Ok(Token::new(TokenKind::NotEq, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Not, Span::new(start, self.i)))
                }
            }
            Some('<') => {
                self.bump_char();
                if self.src.get(self.i..).map(|s| s.starts_with('=')).unwrap_or(false) {
                    self.i += 1;
                    Ok(Token::new(TokenKind::Le, Span::new(start, self.i)))
                } else {
                    Ok(Token::new(TokenKind::Lt, Span::new(start, self.i)))
                }
            }
            Some('>') => {
                self.bump_char();
                if self.src.get(self.i..).map(|s| s.starts_with('=')).unwrap_or(false) {
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
            Some('-') => {
                if self.src.get(self.i + 1..).map(|s| s.starts_with('>')).unwrap_or(false) {
                    self.bump_char();
                    self.bump_char(); // '>'
                    Ok(Token::new(TokenKind::Arrow, Span::new(start, self.i)))
                } else if self.src.get(self.i + 1..).map(|s| s.chars().next().map(|c| c.is_ascii_digit())).flatten().unwrap_or(false) {
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

/// Lex the entire source into a vector of tokens (excluding Eof at the end).
pub fn lex(src: &str) -> Result<Vec<Token>, CompileError> {
    let mut lexer = Lexer::new(src);
    let mut tokens = Vec::new();
    loop {
        let t = lexer.next_token()?;
        if matches!(t.kind, TokenKind::Eof) {
            break;
        }
        tokens.push(t);
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_simple_program() {
        let src = "let x: I32 = 42; x";
        let tokens = lex(src).unwrap();
        assert!(!tokens.is_empty());
        assert!(matches!(tokens[0].kind, TokenKind::KwLet));
        assert!(matches!(tokens[1].kind, TokenKind::Ident(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Colon));
    }

    #[test]
    fn lex_string_literal() {
        let src = r#""hello\nworld""#;
        let tokens = lex(src).unwrap();
        assert_eq!(tokens.len(), 1);
        if let TokenKind::BytesLit(b) = &tokens[0].kind {
            assert_eq!(b, b"hello\nworld");
        } else {
            panic!("expected BytesLit");
        }
    }

    #[test]
    fn lex_keywords() {
        let src = "if else while break continue return try catch throw";
        let tokens = lex(src).unwrap();
        assert!(matches!(tokens[0].kind, TokenKind::KwIf));
        assert!(matches!(tokens[1].kind, TokenKind::KwElse));
        assert!(matches!(tokens[2].kind, TokenKind::KwWhile));
    }

    #[test]
    fn lex_skips_line_comments() {
        let src = "let x: I32 = 1; // hello\nx";
        let tokens = lex(src).unwrap();
        assert!(matches!(tokens[0].kind, TokenKind::KwLet));
        assert!(matches!(tokens.last().unwrap().kind, TokenKind::Ident(_)));
    }

    #[test]
    fn lex_skips_block_comments_multiline() {
        let src = "let x: I32 = 1; /* hello\nworld */ x";
        let tokens = lex(src).unwrap();
        assert!(matches!(tokens[0].kind, TokenKind::KwLet));
        assert!(matches!(tokens.last().unwrap().kind, TokenKind::Ident(_)));
    }

    #[test]
    fn unterminated_block_comment_errors() {
        let src = "let x = 1; /* oops";
        let err = lex(src).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("unterminated block comment"), "rendered:\n{rendered}");
    }
}