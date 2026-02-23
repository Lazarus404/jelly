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
    assert!(
        rendered.contains("unterminated block comment"),
        "rendered:\n{rendered}"
    );
}
