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
use crate::error::CompileError;
use crate::token::Token;

#[path = "lex/core.rs"]
mod core;
#[path = "lex/escape.rs"]
mod escape;
#[path = "lex/keywords.rs"]
mod keywords;
#[path = "lex/numbers.rs"]
mod numbers;
#[path = "lex/strings.rs"]
mod strings;
#[cfg(test)]
#[path = "lex/tests.rs"]
mod tests;

/// Parse a single escape sequence starting after `\`. Returns the decoded bytes and the
/// number of source bytes consumed (excluding the leading `\`).
#[allow(dead_code)] // Public API for external use
pub fn parse_escape_sequence(
    src: &str,
    i: usize,
    esc_at: usize,
) -> Result<(Vec<u8>, usize), CompileError> {
    escape::parse_escape_sequence_impl(src, i, esc_at)
}

use crate::token::TokenKind;

pub struct Lexer<'a> {
    src: &'a str,
    i: usize,
    /// When we're inside backtick interpolation, we need to track brace depth.
    #[allow(dead_code)] // Reserved for backtick literal support
    brace_depth: i32,
}

impl<'a> Lexer<'a> {
    // impl is split across `lex/*` modules.
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
