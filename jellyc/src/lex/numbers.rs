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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

use super::Lexer;

#[allow(dead_code)]
enum IntSuffix {
    I8,
    I16,
    #[allow(dead_code)]
    None,
}

impl<'a> Lexer<'a> {
    fn try_int_suffix(&mut self) -> Option<IntSuffix> {
        let rest = self.src.get(self.i..)?;
        if rest.starts_with("i16")
            && !rest
                .get(3..)
                .and_then(|s| s.chars().next())
                .map(|c| c.is_ascii_alphanumeric() || c == '_')
                .unwrap_or(false)
        {
            self.i += 3;
            return Some(IntSuffix::I16);
        }
        if rest.starts_with("i8")
            && !rest
                .get(2..)
                .and_then(|s| s.chars().next())
                .map(|c| c.is_ascii_alphanumeric() || c == '_')
                .unwrap_or(false)
        {
            self.i += 2;
            return Some(IntSuffix::I8);
        }
        None
    }

    fn try_float_suffix(&mut self) -> bool {
        let rest = self.src.get(self.i..).unwrap_or("");
        if rest.starts_with("f16")
            && !rest
                .get(3..)
                .and_then(|s| s.chars().next())
                .map(|c| c.is_ascii_alphanumeric() || c == '_')
                .unwrap_or(false)
        {
            self.i += 3;
            return true;
        }
        false
    }

    pub(super) fn parse_number(&mut self) -> Result<Token, CompileError> {
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
                return Ok(Token::new(
                    TokenKind::F16Lit(v as f32),
                    Span::new(start, self.i),
                ));
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
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "i8 literal out of range -128..127",
                        ));
                    }
                    return Ok(Token::new(
                        TokenKind::I8Lit(v as i32),
                        Span::new(start, self.i),
                    ));
                }
                IntSuffix::I16 => {
                    if v < -32768 || v > 32767 {
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "i16 literal out of range -32768..32767",
                        ));
                    }
                    return Ok(Token::new(
                        TokenKind::I16Lit(v as i32),
                        Span::new(start, self.i),
                    ));
                }
                IntSuffix::None => {}
            }
        }
        if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
            Ok(Token::new(
                TokenKind::I32Lit(v as i32),
                Span::new(start, self.i),
            ))
        } else {
            Ok(Token::new(TokenKind::I64Lit(v), Span::new(start, self.i)))
        }
    }

    pub(super) fn parse_negative_number(&mut self) -> Result<Token, CompileError> {
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
            let v: f64 = s.parse().map_err(|_| {
                CompileError::at(ErrorKind::Parse, num_start, "invalid float literal")
            })?;
            if self.try_float_suffix() {
                return Ok(Token::new(
                    TokenKind::F16Lit((-v) as f32),
                    Span::new(start, self.i),
                ));
            }
            return Ok(Token::new(TokenKind::F64Lit(-v), Span::new(start, self.i)));
        }

        let v: i64 = s.parse().map_err(|_| {
            CompileError::at(ErrorKind::Parse, num_start, "invalid integer literal")
        })?;
        let nv: i64 = -v;
        if let Some(suffix) = self.try_int_suffix() {
            match suffix {
                IntSuffix::I8 => {
                    if nv < -128 || nv > 127 {
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "i8 literal out of range -128..127",
                        ));
                    }
                    return Ok(Token::new(
                        TokenKind::I8Lit(nv as i32),
                        Span::new(start, self.i),
                    ));
                }
                IntSuffix::I16 => {
                    if nv < -32768 || nv > 32767 {
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "i16 literal out of range -32768..32767",
                        ));
                    }
                    return Ok(Token::new(
                        TokenKind::I16Lit(nv as i32),
                        Span::new(start, self.i),
                    ));
                }
                IntSuffix::None => {}
            }
        }
        if nv >= i32::MIN as i64 && nv <= i32::MAX as i64 {
            Ok(Token::new(
                TokenKind::I32Lit(nv as i32),
                Span::new(start, self.i),
            ))
        } else {
            Ok(Token::new(TokenKind::I64Lit(nv), Span::new(start, self.i)))
        }
    }
}
