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
