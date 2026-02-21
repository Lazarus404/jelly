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

 mod expr;
mod pattern;
mod stmt;
mod ty;
mod token_p;

use crate::ast::{Expr, Pattern, Program, Span, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::lex;
use crate::token::TokenKind;

pub use token_p::TokenP as P;

impl P {
    fn skip_ws(&mut self) {
        // No-op: lexer already skips whitespace
    }

    fn parse_ident(&mut self) -> Result<String, CompileError> {
        if !self.peek_is_ident_start() {
            return self.err("expected identifier");
        }
        self.expect_ident()
    }

    fn parse_dotted_path(&mut self) -> Result<Vec<String>, CompileError> {
        let mut parts = vec![self.parse_ident()?];
        while self.eat_char('.') {
            parts.push(self.parse_ident()?);
        }
        Ok(parts)
    }

    fn parse_string_lit(&mut self) -> Result<Vec<u8>, CompileError> {
        self.expect_bytes_lit()
    }

    fn parse_i32_lit_signed(&mut self, _start: usize) -> Result<(i32, Span), CompileError> {
        let neg = self.eat(TokenKind::Minus);
        let t = self.expect(TokenKind::I32Lit(0))?;
        let v = match t.kind {
            TokenKind::I32Lit(n) => n,
            _ => unreachable!(),
        };
        let v = if neg { -v } else { v };
        Ok((v, t.span))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, CompileError> {
        pattern::parse_pattern(self)
    }

    fn parse_expr(&mut self) -> Result<Expr, CompileError> {
        expr::parse_expr(self)
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, CompileError> {
        stmt::parse_stmt(self)
    }

    fn parse_type(&mut self) -> Result<Ty, CompileError> {
        ty::parse_type(self)
    }

    fn parse_program(&mut self) -> Result<Program, CompileError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            let s = self.parse_stmt()?;
            if let Some(s) = s {
                stmts.push(s);
            } else {
                break;
            }
        }
        let expr = self.parse_expr()?;
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        Ok(Program { stmts, expr })
    }

    fn parse_bool_lit(&mut self) -> Result<bool, CompileError> {
        if self.eat(TokenKind::BoolLit(true)) {
            return Ok(true);
        }
        if self.eat(TokenKind::BoolLit(false)) {
            return Ok(false);
        }
        self.err("expected bool literal")
    }

    fn parse_var_plus_string_expr(&mut self) -> Result<(String, Vec<u8>), CompileError> {
        let name = self.parse_ident()?;
        self.expect_char('+')?;
        let rhs = self.parse_string_lit()?;
        Ok((name, rhs))
    }

    fn parse_expr_parts(&mut self) -> Result<Vec<Vec<u8>>, CompileError> {
        let mut parts = vec![self.parse_string_lit()?];
        while self.eat(TokenKind::Plus) {
            parts.push(self.parse_string_lit()?);
        }
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        Ok(parts)
    }

    fn parse_let_if_program(&mut self) -> Result<(String, Vec<u8>, bool, Vec<u8>, Vec<u8>), CompileError> {
        if !self.eat(TokenKind::KwLet) {
            return self.err("expected 'let'");
        }
        let name = self.parse_ident()?;
        self.expect_char('=')?;
        let init = self.parse_string_lit()?;
        self.expect_char(';')?;
        if !self.eat(TokenKind::KwIf) {
            return self.err("expected 'if'");
        }
        self.expect_char('(')?;
        let cond = self.parse_bool_lit()?;
        self.expect_char(')')?;
        self.expect_char('{')?;
        let (then_name, then_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        if !self.eat(TokenKind::KwElse) {
            return self.err("expected 'else'");
        }
        self.expect_char('{')?;
        let (else_name, else_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        if then_name != name || else_name != name {
            return self.err_at(self.pos(), format!("expected branches to use variable '{}'", name));
        }
        Ok((name, init, cond, then_rhs, else_rhs))
    }

    fn parse_let_y_if_program(
        &mut self,
    ) -> Result<(Vec<u8>, bool, Vec<u8>, Vec<u8>, Vec<u8>), CompileError> {
        if !self.eat(TokenKind::KwLet) {
            return self.err("expected 'let'");
        }
        let x = self.parse_ident()?;
        self.expect_char('=')?;
        let init = self.parse_string_lit()?;
        self.expect_char(';')?;
        if !self.eat(TokenKind::KwLet) {
            return self.err("expected 'let'");
        }
        let y = self.parse_ident()?;
        self.expect_char('=')?;
        if !self.eat(TokenKind::KwIf) {
            return self.err("expected 'if'");
        }
        self.expect_char('(')?;
        let cond = self.parse_bool_lit()?;
        self.expect_char(')')?;
        self.expect_char('{')?;
        let (then_name, then_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        if !self.eat(TokenKind::KwElse) {
            return self.err("expected 'else'");
        }
        self.expect_char('{')?;
        let (else_name, else_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        self.expect_char(';')?;
        let (use_y, tail_rhs) = self.parse_var_plus_string_expr()?;
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        if then_name != x || else_name != x {
            return self.err_at(self.pos(), format!("expected if-branches to use variable '{}'", x));
        }
        if use_y != y {
            return self.err_at(self.pos(), format!("expected trailing expression to use variable '{}'", y));
        }
        Ok((init, cond, then_rhs, else_rhs, tail_rhs))
    }

    fn parse_expr_from_src(&self, expr_src: &str) -> Result<Expr, CompileError> {
        let tokens = lex::lex(expr_src)?;
        let mut sub = P::new(tokens);
        let e = sub.parse_expr()?;
        if !sub.eof() {
            return Err(CompileError::at(
                ErrorKind::Parse,
                sub.pos(),
                "unexpected trailing input in interpolation",
            ));
        }
        Ok(e)
    }
}

pub fn parse_program(src: &str) -> Result<Program, CompileError> {
    let tokens = lex::lex(src)?;
    let mut p = P::new(tokens);
    p.parse_program()
}

