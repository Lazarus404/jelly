use std::path::PathBuf;
use std::collections::{HashMap, HashSet};

mod jlyb;
mod regalloc;
mod ast;
mod error;
mod source;
mod ir;
mod lower;
mod ir_codegen;
mod phi;
mod resolve;
mod typectx;
mod templates;

use ast::{
    Expr, ExprKind, MatchArm, Pattern, PatternKind, Program, Span, Stmt, StmtKind, Ty, TyKind,
};
use error::{CompileError, ErrorKind};

fn usage() -> ! {
    eprintln!(
        "usage:\n  jellyc prelude --out <prelude.jlyb>\n  jellyc <input.jelly> [--out <output.jlyb>] [--backend ast|ir]"
    );
    std::process::exit(2);
}

fn read_to_string(path: &PathBuf) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("error: failed to read {}: {}", path.display(), e);
        std::process::exit(2);
    })
}

struct P<'a> {
    s: &'a str,
    i: usize, // byte offset
}

impl<'a> P<'a> {
    fn new(src: &'a str) -> Self {
        Self { s: src, i: 0 }
    }

    fn eof(&self) -> bool {
        self.i >= self.s.len()
    }

    fn peek_char(&self) -> Option<char> {
        if self.eof() { return None; }
        self.s[self.i..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.i += ch.len_utf8();
        Some(ch)
    }

    fn skip_ws(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                self.bump_char();
            } else {
                break;
            }
        }
    }

    fn err<T>(&self, msg: &str) -> Result<T, CompileError> {
        Err(CompileError::at(ErrorKind::Parse, self.i, msg))
    }

    fn err_at<T>(&self, at: usize, msg: impl Into<String>) -> Result<T, CompileError> {
        Err(CompileError::at(ErrorKind::Parse, at, msg))
    }

    fn hex_val(ch: char) -> Option<u32> {
        match ch {
            '0'..='9' => Some((ch as u32) - ('0' as u32)),
            'a'..='f' => Some((ch as u32) - ('a' as u32) + 10),
            'A'..='F' => Some((ch as u32) - ('A' as u32) + 10),
            _ => None,
        }
    }

    fn parse_u4(&mut self) -> Result<u32, CompileError> {
        let mut v = 0u32;
        for _ in 0..4 {
            let at = self.i;
            let ch = self
                .bump_char()
                .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unexpected EOF in \\uXXXX"))?;
            let h = Self::hex_val(ch)
                .ok_or_else(|| CompileError::at(ErrorKind::Parse, at, format!("invalid hex digit '{}' in \\uXXXX", ch)))?;
            v = (v << 4) | h;
        }
        Ok(v)
    }

    fn push_scalar_utf8(out: &mut Vec<u8>, cp: u32, at: usize) -> Result<(), CompileError> {
        if (0xD800..=0xDFFF).contains(&cp) {
            return Err(CompileError::at(
                ErrorKind::Parse,
                at,
                "invalid Unicode scalar (surrogate)",
            ));
        }
        if cp > 0x10FFFF {
            return Err(CompileError::at(
                ErrorKind::Parse,
                at,
                "invalid Unicode scalar (>U+10FFFF)",
            ));
        }
        let ch = char::from_u32(cp).ok_or_else(|| CompileError::at(ErrorKind::Parse, at, "invalid Unicode scalar"))?;
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        out.extend_from_slice(s.as_bytes());
        Ok(())
    }

    fn parse_string_lit(&mut self) -> Result<Vec<u8>, CompileError> {
        let quote = match self.bump_char() {
            Some('\'') => '\'',
            Some('"') => '"',
            _ => return self.err("expected string literal"),
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
                let e = self
                    .bump_char()
                    .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unterminated escape"))?;
                match e {
                    '\\' => out.push(b'\\'),
                    'n' => out.push(b'\n'),
                    'r' => out.push(b'\r'),
                    't' => out.push(b'\t'),
                    '0' => out.push(0),
                    '\'' => out.push(b'\''),
                    '"' => out.push(b'"'),
                    '`' => out.push(b'`'),
                    'u' => {
                        match self.peek_char() {
                            Some('{') => {
                                self.bump_char();
                                let start = self.i;
                                let mut cp: u32 = 0;
                                let mut digits = 0u32;
                                while let Some(hc) = self.peek_char() {
                                    if hc == '}' { break; }
                                    let h = Self::hex_val(hc).ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "invalid hex digit in \\u{...}"))?;
                                    if digits >= 6 {
                                        return Err(CompileError::at(
                                            ErrorKind::Parse,
                                            start,
                                            "too many hex digits in \\u{...}",
                                        ));
                                    }
                                    cp = (cp << 4) | h;
                                    digits += 1;
                                    self.bump_char();
                                }
                                if self.bump_char() != Some('}') {
                                    return Err(CompileError::at(
                                        ErrorKind::Parse,
                                        start,
                                        "unterminated \\u{...}",
                                    ));
                                }
                                if digits == 0 {
                                    return Err(CompileError::at(ErrorKind::Parse, start, "empty \\u{} escape"));
                                }
                                Self::push_scalar_utf8(&mut out, cp, esc_at)?;
                            }
                            _ => {
                                let cp = self.parse_u4()?;
                                Self::push_scalar_utf8(&mut out, cp, esc_at)?;
                            }
                        }
                    }
                    _ => return Err(CompileError::at(ErrorKind::Parse, esc_at, format!("unknown escape \\{}", e))),
                }
                continue;
            }
            if ch == '\n' || ch == '\r' {
                return Err(CompileError::at(
                    ErrorKind::Parse,
                    self.i - ch.len_utf8(),
                    "newline in string literal",
                ));
            }
            // Raw Unicode scalar value: encode to UTF-8.
            let mut buf = [0u8; 4];
            let s = ch.encode_utf8(&mut buf);
            out.extend_from_slice(s.as_bytes());
        }
        Ok(out)
    }

    fn parse_backtick_interp(&mut self) -> Result<Expr, CompileError> {
        self.skip_ws();
        let start = self.i;
        if self.bump_char() != Some('`') {
            return self.err("expected '`'");
        }

        let mut parts: Vec<Expr> = Vec::new();
        let mut lit: Vec<u8> = Vec::new();
        let mut lit_start: usize = self.i;

        let flush_lit = |parts: &mut Vec<Expr>, lit: &mut Vec<u8>, lit_start: &mut usize, at: usize| {
            if !lit.is_empty() {
                let bytes = std::mem::take(lit);
                parts.push(Expr::new(ExprKind::BytesLit(bytes), Span::new(*lit_start, at)));
            }
            *lit_start = at;
        };

        loop {
            let ch = self
                .bump_char()
                .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unterminated backtick string"))?;
            if ch == '`' {
                flush_lit(&mut parts, &mut lit, &mut lit_start, self.i);
                break;
            }
            if ch == '\\' {
                let esc_at = self.i - 1;
                let e = self
                    .bump_char()
                    .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "unterminated escape"))?;
                match e {
                    '\\' => lit.push(b'\\'),
                    'n' => lit.push(b'\n'),
                    'r' => lit.push(b'\r'),
                    't' => lit.push(b'\t'),
                    '0' => lit.push(0),
                    '\'' => lit.push(b'\''),
                    '"' => lit.push(b'"'),
                    '`' => lit.push(b'`'),
                    'u' => match self.peek_char() {
                        Some('{') => {
                            self.bump_char();
                            let start = self.i;
                            let mut cp: u32 = 0;
                            let mut digits = 0u32;
                            while let Some(hc) = self.peek_char() {
                                if hc == '}' {
                                    break;
                                }
                                let h = Self::hex_val(hc)
                                    .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "invalid hex digit in \\u{...}"))?;
                                if digits >= 6 {
                                    return Err(CompileError::at(ErrorKind::Parse, start, "too many hex digits in \\u{...}"));
                                }
                                cp = (cp << 4) | h;
                                digits += 1;
                                self.bump_char();
                            }
                            if self.bump_char() != Some('}') {
                                return Err(CompileError::at(ErrorKind::Parse, start, "unterminated \\u{...}"));
                            }
                            if digits == 0 {
                                return Err(CompileError::at(ErrorKind::Parse, start, "empty \\u{} escape"));
                            }
                            Self::push_scalar_utf8(&mut lit, cp, esc_at)?;
                        }
                        _ => {
                            let cp = self.parse_u4()?;
                            Self::push_scalar_utf8(&mut lit, cp, esc_at)?;
                        }
                    },
                    _ => return Err(CompileError::at(ErrorKind::Parse, esc_at, format!("unknown escape \\{}", e))),
                }
                continue;
            }
            if ch == '$' && self.peek_char() == Some('{') {
                self.bump_char(); // '{'
                flush_lit(&mut parts, &mut lit, &mut lit_start, self.i);
                let e = self.parse_expr()?;
                self.expect_char('}')?;
                // Compile-time folding: if the interpolated expression is itself a literal bytes
                // value, fold it into the adjacent literal segment.
                match e.node {
                    ExprKind::BytesLit(b) => {
                        if lit.is_empty() {
                            lit_start = e.span.start;
                        }
                        lit.extend_from_slice(&b);
                    }
                    _ => {
                        parts.push(e);
                        lit_start = self.i;
                    }
                }
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
            lit.extend_from_slice(s.as_bytes());
        }

        // Fold into a bytes concatenation chain (typechecked later as bytes).
        if parts.is_empty() {
            return Ok(Expr::new(ExprKind::BytesLit(Vec::new()), Span::new(start, self.i)));
        }
        let mut it = parts.into_iter();
        let mut acc = it.next().expect("non-empty parts");
        for p in it {
            // Compile-time folding: adjacent literal bytes pieces.
            let span = Span::new(acc.span.start, p.span.end);
            acc = match (acc.node, p.node) {
                (ExprKind::BytesLit(mut a), ExprKind::BytesLit(b)) => {
                    a.extend_from_slice(&b);
                    Expr::new(ExprKind::BytesLit(a), span)
                }
                (a, b) => Expr::new(
                    ExprKind::Add(
                        Box::new(Expr::new(a, acc.span)),
                        Box::new(Expr::new(b, p.span)),
                    ),
                    span,
                ),
            };
        }
        acc.span = Span::new(start, self.i);
        Ok(acc)
    }

    fn parse_expr_parts(&mut self) -> Result<Vec<Vec<u8>>, CompileError> {
        self.skip_ws();
        let mut parts = Vec::new();
        parts.push(self.parse_string_lit()?);
        loop {
            self.skip_ws();
            match self.peek_char() {
                Some('+') => {
                    self.bump_char();
                    self.skip_ws();
                    parts.push(self.parse_string_lit()?);
                }
                _ => break,
            }
        }
        self.skip_ws();
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        Ok(parts)
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, CompileError> {
        self.skip_ws();
        let start = self.i;
        self.expect_char('(')?;
        self.skip_ws();
        if self.peek_char() == Some(')') {
            self.bump_char();
            return Ok(Expr::new(ExprKind::TupleLit(vec![]), Span::new(start, self.i)));
        }

        let first = self.parse_expr()?;
        self.skip_ws();
        if self.peek_char() == Some(',') {
            // Tuple literal: (a, b, c)
            self.bump_char();
            let mut elems: Vec<Expr> = vec![first];
            self.skip_ws();
            if self.peek_char() != Some(')') {
                loop {
                    let e = self.parse_expr()?;
                    elems.push(e);
                    self.skip_ws();
                    if self.peek_char() == Some(',') {
                        self.bump_char();
                        self.skip_ws();
                        // Allow trailing comma.
                        if self.peek_char() == Some(')') {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            self.expect_char(')')?;
            return Ok(Expr::new(ExprKind::TupleLit(elems), Span::new(start, self.i)));
        }

        self.expect_char(')')?;
        let mut e = first;
        e.span = Span::new(start, self.i);
        Ok(e)
    }

    fn parse_if_expr(&mut self, if_span: Span) -> Result<Expr, CompileError> {
        // if (<expr>) { <stmt>* <expr> } else { <stmt>* <expr> }
        self.expect_char('(')?;
        let cond = self.parse_expr()?;
        self.expect_char(')')?;
        let then_br = self.parse_block_expr()?;
        self.skip_ws();
        let else_span = self.eat_kw_span("else").ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "expected 'else'"))?;
        let _ = else_span;
        let else_br = self.parse_block_expr()?;

        let span = Span::new(if_span.start, else_br.span.end);
        Ok(Expr::new(
            ExprKind::If {
                cond: Box::new(cond),
                then_br: Box::new(then_br),
                else_br: Box::new(else_br),
            },
            span,
        ))
    }

    fn parse_pattern(&mut self) -> Result<Pattern, CompileError> {
        self.skip_ws();
        let start = self.i;
        // Pin operator: `^name`
        if self.peek_char() == Some('^') {
            self.bump_char();
            let name = self.parse_ident()?;
            return Ok(Pattern::new(PatternKind::Pin(name), Span::new(start, self.i)));
        }
        if self.peek_char() == Some('_') {
            self.bump_char();
            return Ok(Pattern::new(PatternKind::Wildcard, Span::new(start, self.i)));
        }
        if self.peek_char() == Some('{') {
            return self.parse_obj_pattern();
        }
        if self.peek_char() == Some('(') {
            return self.parse_tuple_pattern();
        }
        if self.peek_char() == Some('[') {
            return self.parse_array_pattern();
        }
        if let Some(sp) = self.eat_kw_span("true") {
            return Ok(Pattern::new(PatternKind::BoolLit(true), sp));
        }
        if let Some(sp) = self.eat_kw_span("false") {
            return Ok(Pattern::new(PatternKind::BoolLit(false), sp));
        }
        // i32 literal (allow leading '-')
        let neg = if self.peek_char() == Some('-') {
            self.bump_char();
            true
        } else {
            false
        };
        if matches!(self.peek_char(), Some('0'..='9')) {
            let num_start = self.i;
            while matches!(self.peek_char(), Some('0'..='9')) {
                self.bump_char();
            }
            let s = &self.s[num_start..self.i];
            let mut v: i64 = s
                .parse()
                .map_err(|_| CompileError::at(ErrorKind::Parse, num_start, "invalid integer literal"))?;
            if neg {
                v = -v;
            }
            if v < i32::MIN as i64 || v > i32::MAX as i64 {
                return Err(CompileError::at(ErrorKind::Parse, start, "i32 literal out of range"));
            }
            return Ok(Pattern::new(PatternKind::I32Lit(v as i32), Span::new(start, self.i)));
        }
        // Binding identifier (introduces a name in the arm scope).
        if self.peek_is_ident_start() {
            let name = self.parse_ident()?;
            return Ok(Pattern::new(PatternKind::Bind(name), Span::new(start, self.i)));
        }
        self.err("expected match pattern")
    }

    fn parse_tuple_pattern(&mut self) -> Result<Pattern, CompileError> {
        self.skip_ws();
        let start = self.i;
        self.expect_char('(')?;
        self.skip_ws();
        if self.peek_char() == Some(')') {
            self.bump_char();
            return Ok(Pattern::new(PatternKind::TupleExact(Vec::new()), Span::new(start, self.i)));
        }

        let first = self.parse_pattern()?;
        self.skip_ws();
        if self.peek_char() != Some(',') {
            return Err(CompileError::at(
                ErrorKind::Parse,
                start,
                "tuple pattern requires at least one comma (parenthesized patterns are not supported)",
            ));
        }
        let mut elems: Vec<Pattern> = vec![first];
        while self.peek_char() == Some(',') {
            self.bump_char();
            self.skip_ws();
            if self.peek_char() == Some(')') {
                break;
            }
            elems.push(self.parse_pattern()?);
            self.skip_ws();
        }
        self.expect_char(')')?;
        Ok(Pattern::new(PatternKind::TupleExact(elems), Span::new(start, self.i)))
    }

    fn parse_obj_pattern(&mut self) -> Result<Pattern, CompileError> {
        self.skip_ws();
        let start = self.i;
        self.expect_char('{')?;
        let mut fields: Vec<(String, Pattern)> = Vec::new();
        loop {
            self.skip_ws();
            if self.peek_char() == Some('}') {
                self.bump_char();
                break;
            }
            let key = self.parse_ident()?;
            self.skip_ws();
            self.expect_char(':')?;
            let pat = self.parse_pattern()?;
            fields.push((key, pat));
            self.skip_ws();
            if self.peek_char() == Some(',') {
                self.bump_char();
            }
        }
        Ok(Pattern::new(PatternKind::Obj(fields), Span::new(start, self.i)))
    }

    fn parse_array_pattern(&mut self) -> Result<Pattern, CompileError> {
        self.skip_ws();
        let start = self.i;
        self.expect_char('[')?;
        self.skip_ws();
        if self.peek_char() == Some(']') {
            self.bump_char();
            return Ok(Pattern::new(PatternKind::ArrayExact(Vec::new()), Span::new(start, self.i)));
        }

        let first = self.parse_pattern()?;
        self.skip_ws();
        if self.peek_char() == Some('|') {
            self.bump_char();
            let rest = self.parse_ident()?;
            self.skip_ws();
            self.expect_char(']')?;
            return Ok(Pattern::new(
                PatternKind::ArrayHeadTail {
                    head: Box::new(first),
                    rest,
                },
                Span::new(start, self.i),
            ));
        }

        let mut prefix: Vec<Pattern> = vec![first];
        loop {
            self.skip_ws();
            if self.s[self.i..].starts_with("...") {
                self.i += 3;
                let rest = self.parse_ident()?;
                self.skip_ws();
                self.expect_char(']')?;
                return Ok(Pattern::new(
                    PatternKind::ArrayPrefixRest { prefix, rest },
                    Span::new(start, self.i),
                ));
            }
            if self.peek_char() == Some(']') {
                self.bump_char();
                return Ok(Pattern::new(PatternKind::ArrayExact(prefix), Span::new(start, self.i)));
            }
            self.expect_char(',')?;
            self.skip_ws();
            if self.s[self.i..].starts_with("...") {
                self.i += 3;
                let rest = self.parse_ident()?;
                self.skip_ws();
                self.expect_char(']')?;
                return Ok(Pattern::new(
                    PatternKind::ArrayPrefixRest { prefix, rest },
                    Span::new(start, self.i),
                ));
            }
            if self.peek_char() == Some(']') {
                self.bump_char();
                return Ok(Pattern::new(PatternKind::ArrayExact(prefix), Span::new(start, self.i)));
            }
            prefix.push(self.parse_pattern()?);
        }
    }

    fn parse_match_expr(&mut self, match_span: Span) -> Result<Expr, CompileError> {
        // match (<expr>) { <pat> (when(<expr>))? => { <stmt>* <expr>? } , ... }
        self.skip_ws();
        self.expect_char('(')?;
        let subject = self.parse_expr()?;
        self.expect_char(')')?;
        self.skip_ws();
        self.expect_char('{')?;
        let mut arms: Vec<MatchArm> = Vec::new();
        loop {
            self.skip_ws();
            if self.peek_char() == Some('}') {
                self.bump_char();
                break;
            }
            let pat = self.parse_pattern()?;
            self.skip_ws();
            let when = if self.eat_kw_span("when").is_some() {
                self.skip_ws();
                self.expect_char('(')?;
                let w = self.parse_expr()?;
                self.expect_char(')')?;
                Some(w)
            } else {
                None
            };
            self.skip_ws();
            if !self.s[self.i..].starts_with("=>") {
                return self.err("expected '=>'");
            }
            self.i += 2;
            self.skip_ws();
            let (body, tail) = self.parse_fn_body()?;
            arms.push(MatchArm { pat, when, body, tail });
            self.skip_ws();
            if self.peek_char() == Some(',') {
                self.bump_char();
            }
        }
        Ok(Expr::new(
            ExprKind::Match {
                subject: Box::new(subject),
                arms,
            },
            Span::new(match_span.start, self.i),
        ))
    }

    fn parse_block_expr(&mut self) -> Result<Expr, CompileError> {
        // '{' <stmt>* <expr> '}'
        self.skip_ws();
        let start = self.i;
        self.expect_char('{')?;
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            self.skip_ws();
            if self.peek_char() == Some('}') {
                return self.err("expected expression before '}'");
            }
            let save = self.i;
            if let Some(s) = self.parse_stmt()? {
                stmts.push(s);
                continue;
            }
            // Not a statement; parse final expression and close block.
            self.i = save;
            let e = self.parse_expr()?;
            self.expect_char('}')?;
            let span = Span::new(start, self.i);
            return Ok(Expr::new(ExprKind::Block { stmts, expr: Box::new(e) }, span));
        }
    }

    fn parse_fn_body(&mut self) -> Result<(Vec<Stmt>, Option<Expr>), CompileError> {
        // '{' <stmt>* <expr>? '}'
        self.expect_char('{')?;
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            self.skip_ws();
            if self.peek_char() == Some('}') {
                self.bump_char();
                return Ok((stmts, None));
            }
            let save = self.i;
            if let Some(s) = self.parse_stmt()? {
                stmts.push(s);
                continue;
            }
            self.i = save;
            let e = self.parse_expr()?;
            self.expect_char('}')?;
            return Ok((stmts, Some(e)));
        }
    }

    fn parse_brace_expr(&mut self) -> Result<Expr, CompileError> {
        // Disambiguate:
        // - Object literal: {a: expr, b: expr}
        // - Block expression: { stmt* expr }
        self.skip_ws();
        let start = self.i;
        let save = self.i;
        self.expect_char('{')?;
        self.skip_ws();
        if self.peek_char() == Some('}') {
            self.bump_char();
            return Ok(Expr::new(ExprKind::ObjLit(vec![]), Span::new(start, self.i)));
        }
        // If we see `ident :` then it's an object literal.
        if self.peek_is_ident_start() {
            let name_start = self.i;
            let key = self.parse_ident()?;
            self.skip_ws();
            if self.peek_char() == Some(':') {
                self.bump_char();
                let mut fields: Vec<(String, Expr)> = Vec::new();
                let v = self.parse_expr()?;
                fields.push((key, v));
                loop {
                    self.skip_ws();
                    if self.peek_char() == Some(',') {
                        self.bump_char();
                        self.skip_ws();
                        let k = self.parse_ident()?;
                        self.skip_ws();
                        self.expect_char(':')?;
                        let vv = self.parse_expr()?;
                        fields.push((k, vv));
                        continue;
                    }
                    break;
                }
                self.skip_ws();
                self.expect_char('}')?;
                return Ok(Expr::new(ExprKind::ObjLit(fields), Span::new(start, self.i)));
            }
            // Not an object; rewind and parse as block expression.
            self.i = name_start;
        }
        self.i = save;
        let mut e = self.parse_block_expr()?;
        e.span = Span::new(start, e.span.end);
        Ok(e)
    }

    fn parse_primary(&mut self) -> Result<Expr, CompileError> {
        self.skip_ws();

        if let Some(fn_span) = self.eat_kw_span("fn") {
            self.expect_char('(')?;
            let mut params: Vec<(String, Option<Ty>)> = Vec::new();
            self.skip_ws();
            if self.peek_char() != Some(')') {
                loop {
                    let name = self.parse_ident()?;
                    self.skip_ws();
                    let ty = if self.peek_char() == Some(':') {
                        self.bump_char();
                        Some(self.parse_type()?)
                    } else {
                        None
                    };
                    params.push((name, ty));
                    self.skip_ws();
                    if self.peek_char() == Some(',') {
                        self.bump_char();
                        continue;
                    }
                    break;
                }
            }
            self.expect_char(')')?;
            let (body, tail) = self.parse_fn_body()?;
            let span = Span::new(fn_span.start, self.i);
            return Ok(Expr::new(
                ExprKind::Fn {
                    params,
                    body,
                    tail: tail.map(Box::new),
                },
                span,
            ));
        }

        if let Some(if_span) = self.eat_kw_span("if") {
            return self.parse_if_expr(if_span);
        }
        if let Some(match_span) = self.eat_kw_span("match") {
            return self.parse_match_expr(match_span);
        }
        if let Some(kw_span) = self.eat_kw_span("true") {
            return Ok(Expr::new(ExprKind::BoolLit(true), kw_span));
        }
        if let Some(kw_span) = self.eat_kw_span("false") {
            return Ok(Expr::new(ExprKind::BoolLit(false), kw_span));
        }
        if let Some(kw_span) = self.eat_kw_span("null") {
            return Ok(Expr::new(ExprKind::Null, kw_span));
        }
        if matches!(self.peek_char(), Some('0'..='9')) {
            let start = self.i;
            while matches!(self.peek_char(), Some('0'..='9')) {
                self.bump_char();
            }
            let s = &self.s[start..self.i];
            let v: i64 = s
                .parse()
                .map_err(|_| CompileError::at(ErrorKind::Parse, start, "invalid integer literal"))?;
            if v > i32::MAX as i64 {
                return Err(CompileError::at(ErrorKind::Parse, start, "i32 literal out of range"));
            }
            return Ok(Expr::new(ExprKind::I32Lit(v as i32), Span::new(start, self.i)));
        }
        match self.peek_char() {
            Some('"') | Some('\'') => {
                let start = self.i;
                let b = self.parse_string_lit()?;
                Ok(Expr::new(ExprKind::BytesLit(b), Span::new(start, self.i)))
            }
            Some('`') => self.parse_backtick_interp(),
            Some('(') => self.parse_paren_expr(),
            Some('{') => self.parse_brace_expr(),
            Some('[') => {
                let start = self.i;
                self.bump_char();
                let mut elems: Vec<Expr> = Vec::new();
                self.skip_ws();
                if self.peek_char() != Some(']') {
                    loop {
                        let e = self.parse_expr()?;
                        elems.push(e);
                        self.skip_ws();
                        if self.peek_char() == Some(',') {
                            self.bump_char();
                            continue;
                        }
                        break;
                    }
                }
                self.expect_char(']')?;
                Ok(Expr::new(ExprKind::ArrayLit(elems), Span::new(start, self.i)))
            }
            _ => {
                let start = self.i;
                let name = self.parse_ident()?;
                Ok(Expr::new(ExprKind::Var(name), Span::new(start, self.i)))
            }
        }
    }

    fn parse_term(&mut self) -> Result<Expr, CompileError> {
        self.skip_ws();
        if let Some(new_span) = self.eat_kw_span("new") {
            // `new <expr>(args...)`
            //
            // Parse a "callee-like" expression (primary + member/index), but do not consume a
            // call suffix: the `(args...)` belongs to `new`.
            let mut proto = self.parse_primary()?;
            loop {
                self.skip_ws();
                match self.peek_char() {
                    Some('.') => {
                        self.bump_char();
                        self.skip_ws();
                        let name = if matches!(self.peek_char(), Some('0'..='9')) {
                            let ns = self.i;
                            while matches!(self.peek_char(), Some('0'..='9')) {
                                self.bump_char();
                            }
                            self.s[ns..self.i].to_string()
                        } else {
                            self.parse_ident()?
                        };
                        let start = proto.span.start;
                        proto = Expr::new(
                            ExprKind::Member {
                                base: Box::new(proto),
                                name,
                            },
                            Span::new(start, self.i),
                        );
                    }
                    Some('<') => {
                        // Allow `new Proto<T>(...)` by parsing a type application on the proto.
                        // This is ambiguous with `<` as a comparison operator in expressions,
                        // but in the `new` form we are explicitly parsing "callee-like" postfixes.
                        let save = self.i;
                        if let Ok(targs) = (|| -> Result<Vec<Ty>, CompileError> {
                            self.bump_char(); // '<'
                            self.skip_ws();
                            let mut args: Vec<Ty> = Vec::new();
                            args.push(self.parse_type()?);
                            self.skip_ws();
                            while self.peek_char() == Some(',') {
                                self.bump_char();
                                self.skip_ws();
                                args.push(self.parse_type()?);
                                self.skip_ws();
                            }
                            self.expect_char('>')?;
                            Ok(args)
                        })() {
                            let start = proto.span.start;
                            proto = Expr::new(
                                ExprKind::TypeApp {
                                    base: Box::new(proto),
                                    type_args: targs,
                                },
                                Span::new(start, self.i),
                            );
                        } else {
                            self.i = save;
                            break;
                        }
                    }
                    Some('[') => {
                        let start = proto.span.start;
                        self.bump_char();
                        let idx = self.parse_expr()?;
                        self.expect_char(']')?;
                        proto = Expr::new(
                            ExprKind::Index {
                                base: Box::new(proto),
                                index: Box::new(idx),
                            },
                            Span::new(start, self.i),
                        );
                    }
                    _ => break,
                }
            }

            self.skip_ws();
            self.expect_char('(')?;
            let mut args: Vec<Expr> = Vec::new();
            self.skip_ws();
            if self.peek_char() != Some(')') {
                loop {
                    let a = self.parse_expr()?;
                    args.push(a);
                    self.skip_ws();
                    if self.peek_char() == Some(',') {
                        self.bump_char();
                        continue;
                    }
                    break;
                }
            }
            self.expect_char(')')?;

            let span = Span::new(new_span.start, self.i);
            return Ok(Expr::new(
                ExprKind::New {
                    proto: Box::new(proto),
                    args,
                },
                span,
            ));
        }
        if self.peek_char() == Some('!') {
            let start = self.i;
            self.bump_char();
            let inner = self.parse_term()?;
            let end = inner.span.end;
            return Ok(Expr::new(
                ExprKind::Not(Box::new(inner)),
                Span::new(start, end),
            ));
        }
        if self.peek_char() == Some('-') {
            let at = self.i;
            self.bump_char();
            self.skip_ws();
            if matches!(self.peek_char(), Some('0'..='9')) {
                let start = self.i;
                while matches!(self.peek_char(), Some('0'..='9')) {
                    self.bump_char();
                }
                let s = &self.s[start..self.i];
                let v: i64 = s
                    .parse()
                    .map_err(|_| CompileError::at(ErrorKind::Parse, start, "invalid integer literal"))?;
                let nv = -v;
                if nv < i32::MIN as i64 || nv > i32::MAX as i64 {
                    return Err(CompileError::at(ErrorKind::Parse, at, "i32 literal out of range"));
                }
                return Ok(Expr::new(ExprKind::I32Lit(nv as i32), Span::new(at, self.i)));
            }
            let inner = self.parse_term()?;
            let end = inner.span.end;
            return Ok(Expr::new(
                ExprKind::Neg(Box::new(inner)),
                Span::new(at, end),
            ));
        }

        let mut e = self.parse_primary()?;
        loop {
            self.skip_ws();
            match self.peek_char() {
                Some('.') => {
                    self.bump_char();
                    self.skip_ws();
                    let name = if matches!(self.peek_char(), Some('0'..='9')) {
                        let ns = self.i;
                        while matches!(self.peek_char(), Some('0'..='9')) {
                            self.bump_char();
                        }
                        self.s[ns..self.i].to_string()
                    } else {
                        self.parse_ident()?
                    };
                    let start = e.span.start;
                    e = Expr::new(
                        ExprKind::Member {
                            base: Box::new(e),
                            name,
                        },
                        Span::new(start, self.i),
                    );
                }
                Some('<') => {
                    // Parse optional generic type args for a call: `callee<T1,T2>(...)`.
                    // This is ambiguous with `<` as a comparison operator, so we only treat it
                    // as type args if it is immediately followed by a `(...)` call.
                    let save = self.i;
                    if let Ok(targs) = (|| -> Result<Vec<Ty>, CompileError> {
                        self.bump_char(); // '<'
                        self.skip_ws();
                        let mut args: Vec<Ty> = Vec::new();
                        // Require at least one type.
                        args.push(self.parse_type()?);
                        self.skip_ws();
                        while self.peek_char() == Some(',') {
                            self.bump_char();
                            self.skip_ws();
                            args.push(self.parse_type()?);
                            self.skip_ws();
                        }
                        self.expect_char('>')?;
                        // Only accept as type args if a call follows.
                        self.skip_ws();
                        if self.peek_char() != Some('(') {
                            return Err(CompileError::at(ErrorKind::Parse, save, "not type args"));
                        }
                        Ok(args)
                    })() {
                        // Stash the parsed type args by wrapping into a synthetic call node later.
                        // We attach them when we see the following `(...)` case.
                        // Represent as a Call with no args yet, to be completed by the '(' arm.
                        let start = e.span.start;
                        e = Expr::new(
                            ExprKind::Call {
                                callee: Box::new(e),
                                type_args: targs,
                                args: Vec::new(),
                            },
                            Span::new(start, self.i),
                        );
                    } else {
                        // If it is not a call-style `<...>(...)`, it may still be a type
                        // application / specialization: `Name<T>`.
                        self.i = save;
                        if let Ok(targs) = (|| -> Result<Vec<Ty>, CompileError> {
                            self.bump_char(); // '<'
                            self.skip_ws();
                            let mut args: Vec<Ty> = Vec::new();
                            args.push(self.parse_type()?);
                            self.skip_ws();
                            while self.peek_char() == Some(',') {
                                self.bump_char();
                                self.skip_ws();
                                args.push(self.parse_type()?);
                                self.skip_ws();
                            }
                            self.expect_char('>')?;
                            // Reject if a call follows; the call-typed-args form handles that.
                            self.skip_ws();
                            if self.peek_char() == Some('(') {
                                return Err(CompileError::at(ErrorKind::Parse, save, "not type app"));
                            }
                            Ok(args)
                        })() {
                            let start = e.span.start;
                            e = Expr::new(
                                ExprKind::TypeApp {
                                    base: Box::new(e),
                                    type_args: targs,
                                },
                                Span::new(start, self.i),
                            );
                        } else {
                            self.i = save;
                            break;
                        }
                    }
                }
                Some('(') => {
                    self.bump_char();
                    let mut args: Vec<Expr> = Vec::new();
                    self.skip_ws();
                    if self.peek_char() != Some(')') {
                        loop {
                            let a = self.parse_expr()?;
                            args.push(a);
                            self.skip_ws();
                            if self.peek_char() == Some(',') {
                                self.bump_char();
                                continue;
                            }
                            break;
                        }
                    }
                    self.expect_char(')')?;
                    let start = e.span.start;
                    e = match e.node {
                        ExprKind::Call {
                            callee,
                            type_args,
                            args: ref existing_args,
                        } if existing_args.is_empty() => Expr::new(
                            ExprKind::Call {
                                callee,
                                type_args,
                                args,
                            },
                            Span::new(start, self.i),
                        ),
                        _ => Expr::new(
                            ExprKind::Call {
                                callee: Box::new(e),
                                type_args: Vec::new(),
                                args,
                            },
                            Span::new(start, self.i),
                        ),
                    };
                }
                Some('[') => {
                    self.bump_char();
                    let idx = self.parse_expr()?;
                    self.expect_char(']')?;
                    let start = e.span.start;
                    e = Expr::new(
                        ExprKind::Index { base: Box::new(e), index: Box::new(idx) },
                        Span::new(start, self.i),
                    );
                }
                _ => break,
            }
        }
        Ok(e)
    }

    fn parse_add_expr(&mut self) -> Result<Expr, CompileError> {
        let mut e = self.parse_term()?;
        loop {
            self.skip_ws();
            if self.peek_char() == Some('+') {
                self.bump_char();
                let lhs = e;
                let rhs = self.parse_term()?;
                let span = Span::new(lhs.span.start, rhs.span.end);
                // Compile-time fold adjacent literal bytes pieces.
                e = match (lhs.node, rhs.node) {
                    (ExprKind::BytesLit(mut a), ExprKind::BytesLit(b)) => {
                        a.extend_from_slice(&b);
                        Expr::new(ExprKind::BytesLit(a), span)
                    }
                    (a, b) => Expr::new(
                        ExprKind::Add(
                            Box::new(Expr::new(a, lhs.span)),
                            Box::new(Expr::new(b, rhs.span)),
                        ),
                        span,
                    ),
                };
                continue;
            }
            if self.peek_char() == Some('-') {
                self.bump_char();
                let rhs = self.parse_term()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Sub(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn parse_rel_expr(&mut self) -> Result<Expr, CompileError> {
        let mut e = self.parse_add_expr()?;
        loop {
            self.skip_ws();
            if self.s[self.i..].starts_with("<=") {
                self.i += 2;
                let rhs = self.parse_add_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Le(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            if self.s[self.i..].starts_with(">=") {
                self.i += 2;
                let rhs = self.parse_add_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Ge(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            if self.peek_char() == Some('<') {
                self.bump_char();
                let rhs = self.parse_add_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Lt(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            if self.peek_char() == Some('>') {
                self.bump_char();
                let rhs = self.parse_add_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Gt(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn parse_eq_expr(&mut self) -> Result<Expr, CompileError> {
        let mut e = self.parse_rel_expr()?;
        loop {
            self.skip_ws();
            if self.s[self.i..].starts_with("==") {
                self.i += 2;
                let rhs = self.parse_rel_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Eq(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            if self.s[self.i..].starts_with("!=") {
                self.i += 2;
                let rhs = self.parse_rel_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Ne(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, CompileError> {
        let mut e = self.parse_eq_expr()?;
        loop {
            self.skip_ws();
            if self.s[self.i..].starts_with("&&") {
                self.i += 2;
                let rhs = self.parse_eq_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::And(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn parse_or_expr(&mut self) -> Result<Expr, CompileError> {
        let mut e = self.parse_and_expr()?;
        loop {
            self.skip_ws();
            if self.s[self.i..].starts_with("||") {
                self.i += 2;
                let rhs = self.parse_and_expr()?;
                let span = Span::new(e.span.start, rhs.span.end);
                e = Expr::new(ExprKind::Or(Box::new(e), Box::new(rhs)), span);
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn parse_expr(&mut self) -> Result<Expr, CompileError> {
        self.skip_ws();
        if let Some(try_span) = self.eat_kw_span("try") {
            return self.parse_try_expr(try_span);
        }
        if let Some(if_span) = self.eat_kw_span("if") {
            return self.parse_if_expr(if_span);
        }
        self.parse_or_expr()
    }

    fn parse_try_expr(&mut self, try_span: Span) -> Result<Expr, CompileError> {
        // try { <stmt>* <expr> } catch (<ident>)? { <stmt>* <expr> }
        let body = self.parse_block_expr()?;
        self.skip_ws();
        let _catch_span =
            self.eat_kw_span("catch").ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "expected 'catch'"))?;
        self.skip_ws();
        let catch_name = if self.peek_char() == Some('(') {
            self.bump_char();
            let name = self.parse_ident()?;
            self.expect_char(')')?;
            Some(name)
        } else {
            None
        };
        let catch_body = self.parse_block_expr()?;
        let span = Span::new(try_span.start, catch_body.span.end);
        Ok(Expr::new(
            ExprKind::Try {
                body: Box::new(body),
                catch_name,
                catch_body: Box::new(catch_body),
            },
            span,
        ))
    }

    fn try_parse_assign_stmt(&mut self) -> Result<Option<Stmt>, CompileError> {
        // Lookahead:
        // - <ident> '=' ... ';'        (but not '==')
        // - <ident> '[' <expr> ']' '=' <expr> ';'
        // - <ident> '.' <ident> '=' <expr> ';'
        self.skip_ws();
        if !self.peek_is_ident_start() {
            return Ok(None);
        }
        let save = self.i;
        let name = self.parse_ident()?;
        let name_span = Span::new(save, self.i);
        self.skip_ws();

        if self.peek_char() == Some('.') {
            self.bump_char();
            self.skip_ws();
            let field = self.parse_ident()?;
            self.skip_ws();
            self.expect_char('=')?;
            let expr = self.parse_expr()?;
            self.expect_char(';')?;
            let base = Expr::new(ExprKind::Var(name), name_span);
            let span = Span::new(save, self.i);
            return Ok(Some(Stmt::new(StmtKind::MemberAssign { base, name: field, expr }, span)));
        }

        if self.peek_char() == Some('[') {
            self.bump_char();
            let index = self.parse_expr()?;
            self.expect_char(']')?;
            self.expect_char('=')?;
            let expr = self.parse_expr()?;
            self.expect_char(';')?;
            let base = Expr::new(ExprKind::Var(name), name_span);
            let span = Span::new(save, self.i);
            return Ok(Some(Stmt::new(StmtKind::IndexAssign { base, index, expr }, span)));
        }

        if self.s[self.i..].starts_with("==") {
            self.i = save;
            return Ok(None);
        }
        if self.peek_char() != Some('=') {
            self.i = save;
            return Ok(None);
        }
        self.bump_char(); // '='
        let expr = self.parse_expr()?;
        self.expect_char(';')?;
        let span = Span::new(save, self.i);
        Ok(Some(Stmt::new(StmtKind::Assign { name, expr }, span)))
    }

    fn parse_while_stmt(&mut self, while_span: Span) -> Result<Stmt, CompileError> {
        // while (<expr>) { <stmt>* }
        self.expect_char('(')?;
        let cond = self.parse_expr()?;
        self.expect_char(')')?;
        let body = self.parse_block_stmts()?;
        Ok(Stmt::new(
            StmtKind::While { cond, body },
            Span::new(while_span.start, self.i),
        ))
    }

    fn parse_block_stmts(&mut self) -> Result<Vec<Stmt>, CompileError> {
        self.expect_char('{')?;
        let mut out: Vec<Stmt> = Vec::new();
        loop {
            self.skip_ws();
            if self.peek_char() == Some('}') {
                self.bump_char();
                break;
            }
            let s = self
                .parse_stmt()?
                .ok_or_else(|| CompileError::at(ErrorKind::Parse, self.i, "expected statement"))?;
            out.push(s);
        }
        Ok(out)
    }

    fn parse_stmt(&mut self) -> Result<Option<Stmt>, CompileError> {
        self.skip_ws();
        if let Some(import_span) = self.eat_kw_span("import") {
            self.skip_ws();

            let type_only = self.eat_kw("type");
            self.skip_ws();

            if self.peek_char() == Some('{') {
                self.bump_char();
                let mut items: Vec<(String, Option<String>)> = Vec::new();
                loop {
                    self.skip_ws();
                    if self.peek_char() == Some('}') {
                        self.bump_char();
                        break;
                    }
                    let name = self.parse_ident()?;
                    self.skip_ws();
                    let alias = if self.eat_kw("as") {
                        Some(self.parse_ident()?)
                    } else {
                        None
                    };
                    items.push((name, alias));
                    self.skip_ws();
                    if self.peek_char() == Some(',') {
                        self.bump_char();
                        continue;
                    }
                    self.skip_ws();
                    if self.peek_char() == Some('}') {
                        self.bump_char();
                        break;
                    }
                    return Err(CompileError::at(ErrorKind::Parse, self.i, "expected ',' or '}' in import list"));
                }
                self.skip_ws();
                if !self.eat_kw("from") {
                    return Err(CompileError::at(ErrorKind::Parse, self.i, "expected 'from' in import"));
                }
                let from = self.parse_dotted_path()?;
                self.expect_char(';')?;
                let span = Span::new(import_span.start, self.i);
                return Ok(Some(Stmt::new(
                    StmtKind::ImportFrom {
                        type_only,
                        items,
                        from,
                    },
                    span,
                )));
            }

            let path = self.parse_dotted_path()?;
            self.skip_ws();
            if !self.eat_kw("as") {
                return Err(CompileError::at(ErrorKind::Parse, self.i, "expected 'as' in import"));
            }
            let alias = self.parse_ident()?;
            self.expect_char(';')?;
            let span = Span::new(import_span.start, self.i);
            return Ok(Some(Stmt::new(StmtKind::ImportModule { path, alias }, span)));
        }

        let export_span = self.eat_kw_span("export");
        let exported = export_span.is_some();
        self.skip_ws();

        if let Some(proto_span) = self.eat_kw_span("prototype") {
            let name = self.parse_ident()?;
            self.skip_ws();
            // Optional template type params: `prototype Name<T,U> { ... }`
            let mut type_params: Vec<String> = Vec::new();
            if self.peek_char() == Some('<') {
                self.bump_char();
                self.skip_ws();
                type_params.push(self.parse_ident()?);
                self.skip_ws();
                while self.peek_char() == Some(',') {
                    self.bump_char();
                    self.skip_ws();
                    type_params.push(self.parse_ident()?);
                    self.skip_ws();
                }
                self.expect_char('>')?;
                self.skip_ws();
            }

            self.expect_char('{')?;
            let mut fields: Vec<(String, Expr)> = Vec::new();
            loop {
                self.skip_ws();
                if self.peek_char() == Some('}') {
                    self.bump_char();
                    break;
                }
                let k = self.parse_ident()?;
                self.skip_ws();
                self.expect_char(':')?;
                let v = self.parse_expr()?;
                self.skip_ws();
                self.expect_char(';')?;
                fields.push((k, v));
            }
            self.skip_ws();
            self.expect_char(';')?;

            let span = Span::new(export_span.map(|s| s.start).unwrap_or(proto_span.start), self.i);
            return Ok(Some(Stmt::new(
                StmtKind::Prototype {
                    exported,
                    name,
                    type_params,
                    fields,
                },
                span,
            )));
        }

        if let Some(let_span) = self.eat_kw_span("let") {
            let name = self.parse_ident()?;
            self.skip_ws();
            // Optional template type params: `let Name<T,U> ...`
            let mut type_params: Vec<String> = Vec::new();
            if self.peek_char() == Some('<') {
                self.bump_char();
                self.skip_ws();
                // Require at least one identifier.
                type_params.push(self.parse_ident()?);
                self.skip_ws();
                while self.peek_char() == Some(',') {
                    self.bump_char();
                    self.skip_ws();
                    type_params.push(self.parse_ident()?);
                    self.skip_ws();
                }
                self.expect_char('>')?;
                self.skip_ws();
            }
            let ty = if self.peek_char() == Some(':') {
                self.bump_char();
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect_char('=')?;
            let expr = self.parse_expr()?;
            self.expect_char(';')?;
            let span = Span::new(export_span.map(|s| s.start).unwrap_or(let_span.start), self.i);
            return Ok(Some(Stmt::new(
                StmtKind::Let {
                    exported,
                    name,
                    type_params,
                    ty,
                    expr,
                },
                span,
            )));
        }
        if exported {
            return Err(CompileError::at(
                ErrorKind::Parse,
                export_span.unwrap().start,
                "expected 'let' or 'prototype' after 'export'",
            ));
        }
        if let Some(while_span) = self.eat_kw_span("while") {
            let s = self.parse_while_stmt(while_span)?;
            return Ok(Some(s));
        }
        if let Some(break_span) = self.eat_kw_span("break") {
            self.expect_char(';')?;
            return Ok(Some(Stmt::new(StmtKind::Break, Span::new(break_span.start, self.i))));
        }
        if let Some(cont_span) = self.eat_kw_span("continue") {
            self.expect_char(';')?;
            return Ok(Some(Stmt::new(StmtKind::Continue, Span::new(cont_span.start, self.i))));
        }
        if let Some(throw_span) = self.eat_kw_span("throw") {
            let expr = self.parse_expr()?;
            self.expect_char(';')?;
            return Ok(Some(Stmt::new(StmtKind::Throw { expr }, Span::new(throw_span.start, self.i))));
        }
        if let Some(ret_span) = self.eat_kw_span("return") {
            self.skip_ws();
            if self.peek_char() == Some(';') {
                self.bump_char();
                return Ok(Some(Stmt::new(StmtKind::Return { expr: None }, Span::new(ret_span.start, self.i))));
            }
            let expr = self.parse_expr()?;
            self.expect_char(';')?;
            return Ok(Some(Stmt::new(
                StmtKind::Return { expr: Some(expr) },
                Span::new(ret_span.start, self.i),
            )));
        }
        self.try_parse_assign_stmt()
    }

    fn parse_type_atom(&mut self) -> Result<Ty, CompileError> {
        self.skip_ws();
        let start = self.i;
        if self.peek_char() == Some('(') {
            self.bump_char();
            // Either (A,B)->C args grouping, or plain (T)
            let first = self.parse_type()?;
            self.skip_ws();
            if self.peek_char() == Some(',') {
                let mut args = vec![first];
                while self.peek_char() == Some(',') {
                    self.bump_char();
                    args.push(self.parse_type()?);
                    self.skip_ws();
                }
                self.expect_char(')')?;
                // Caller will parse -> ret.
                return Ok(Ty::new(
                    TyKind::Generic {
                        base: "__args__".to_string(),
                        args,
                    },
                    Span::new(start, self.i),
                ));
            }
            self.expect_char(')')?;
            let mut out = first;
            out.span = Span::new(start, self.i);
            Ok(out)
        } else {
            let base_start = self.i;
            let base = self.parse_ident()?;
            let base_end = self.i;
            self.skip_ws();
            if self.peek_char() == Some('<') {
                self.bump_char();
                let mut args: Vec<Ty> = Vec::new();
                loop {
                    let t = self.parse_type()?;
                    args.push(t);
                    self.skip_ws();
                    if self.peek_char() == Some(',') {
                        self.bump_char();
                        continue;
                    }
                    break;
                }
                self.expect_char('>')?;
                Ok(Ty::new(
                    TyKind::Generic { base, args },
                    Span::new(base_start, self.i),
                ))
            } else {
                Ok(Ty::new(TyKind::Named(base), Span::new(base_start, base_end)))
            }
        }
    }

    fn parse_type(&mut self) -> Result<Ty, CompileError> {
        // Right-associative: A -> B -> C  == A -> (B -> C)
        let left = self.parse_type_atom()?;
        self.skip_ws();
        if self.s[self.i..].starts_with("->") {
            self.i += 2;
            let right = self.parse_type()?;
            // Handle the special (A,B)->C form encoded as __args__ generic above.
            if let TyKind::Generic { base, args } = left.node.clone() {
                if base == "__args__" {
                    let span = Span::new(left.span.start, right.span.end);
                    return Ok(Ty::new(TyKind::Fun { args, ret: Box::new(right) }, span));
                }
                // Otherwise, treat as single arg.
                let span = Span::new(left.span.start, right.span.end);
                return Ok(Ty::new(
                    TyKind::Fun {
                        args: vec![Ty::new(TyKind::Generic { base, args }, left.span)],
                        ret: Box::new(right),
                    },
                    span,
                ));
            }
            let span = Span::new(left.span.start, right.span.end);
            Ok(Ty::new(TyKind::Fun { args: vec![left], ret: Box::new(right) }, span))
        } else {
            Ok(left)
        }
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
        self.skip_ws();
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        Ok(Program { stmts, expr })
    }

    fn peek_is_ident_start(&self) -> bool {
        match self.peek_char() {
            Some(ch) => ch == '_' || ch.is_ascii_alphabetic(),
            None => false,
        }
    }

    fn peek_is_ident_continue(&self) -> bool {
        match self.peek_char() {
            Some(ch) => ch == '_' || ch.is_ascii_alphanumeric(),
            None => false,
        }
    }

    fn parse_ident(&mut self) -> Result<String, CompileError> {
        self.skip_ws();
        if !self.peek_is_ident_start() {
            return self.err("expected identifier");
        }
        let start = self.i;
        self.bump_char();
        while self.peek_is_ident_continue() {
            self.bump_char();
        }
        Ok(self.s[start..self.i].to_string())
    }

    fn parse_dotted_path(&mut self) -> Result<Vec<String>, CompileError> {
        self.skip_ws();
        let mut parts: Vec<String> = Vec::new();
        parts.push(self.parse_ident()?);
        loop {
            self.skip_ws();
            if self.peek_char() != Some('.') {
                break;
            }
            self.bump_char();
            parts.push(self.parse_ident()?);
        }
        Ok(parts)
    }

    fn eat_kw_span(&mut self, kw: &str) -> Option<Span> {
        self.skip_ws();
        let start = self.i;
        if self.s[self.i..].starts_with(kw) {
            // ensure boundary
            let end = self.i + kw.len();
            let next = self.s.get(end..).and_then(|t| t.chars().next());
            if next.map(|c| c == '_' || c.is_ascii_alphanumeric()).unwrap_or(false) {
                return None;
            }
            self.i = end;
            return Some(Span::new(start, end));
        }
        None
    }

    fn eat_kw(&mut self, kw: &str) -> bool {
        self.eat_kw_span(kw).is_some()
    }

    fn expect_char(&mut self, want: char) -> Result<(), CompileError> {
        self.skip_ws();
        match self.bump_char() {
            Some(c) if c == want => Ok(()),
            _ => self.err(&format!("expected '{}'", want)),
        }
    }

    fn parse_bool_lit(&mut self) -> Result<bool, CompileError> {
        self.skip_ws();
        if self.eat_kw("true") {
            return Ok(true);
        }
        if self.eat_kw("false") {
            return Ok(false);
        }
        self.err("expected bool literal")
    }

    fn parse_var_plus_string_expr(&mut self) -> Result<(String, Vec<u8>), CompileError> {
        let name = self.parse_ident()?;
        self.skip_ws();
        self.expect_char('+')?;
        self.skip_ws();
        let rhs = self.parse_string_lit()?;
        Ok((name, rhs))
    }

    fn parse_let_if_program(&mut self) -> Result<(String, Vec<u8>, bool, Vec<u8>, Vec<u8>), CompileError> {
        self.skip_ws();
        if !self.eat_kw("let") {
            return self.err("expected 'let'");
        }
        let name = self.parse_ident()?;
        self.expect_char('=')?;
        self.skip_ws();
        let init = self.parse_string_lit()?;
        self.expect_char(';')?;

        self.skip_ws();
        if !self.eat_kw("if") {
            return self.err("expected 'if'");
        }
        self.expect_char('(')?;
        let cond = self.parse_bool_lit()?;
        self.expect_char(')')?;
        self.expect_char('{')?;
        let (then_name, then_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        self.skip_ws();
        if !self.eat_kw("else") {
            return self.err("expected 'else'");
        }
        self.expect_char('{')?;
        let (else_name, else_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        self.skip_ws();
        if !self.eof() {
            return self.err("unexpected trailing input");
        }
        if then_name != name || else_name != name {
            return self.err_at(self.i, format!("expected branches to use variable '{}'", name));
        }
        Ok((name, init, cond, then_rhs, else_rhs))
    }

    fn parse_let_y_if_program(
        &mut self,
    ) -> Result<(Vec<u8>, bool, Vec<u8>, Vec<u8>, Vec<u8>), CompileError> {
        // Accept only this MVP shape:
        // let x = "he";
        // let y = if (true) { x + "llo" } else { x + "no" };
        // y + "!"
        self.skip_ws();
        if !self.eat_kw("let") {
            return self.err("expected 'let'");
        }
        let x = self.parse_ident()?;
        self.expect_char('=')?;
        self.skip_ws();
        let init = self.parse_string_lit()?;
        self.expect_char(';')?;

        self.skip_ws();
        if !self.eat_kw("let") {
            return self.err("expected 'let'");
        }
        let y = self.parse_ident()?;
        self.expect_char('=')?;

        self.skip_ws();
        if !self.eat_kw("if") {
            return self.err("expected 'if'");
        }
        self.expect_char('(')?;
        let cond = self.parse_bool_lit()?;
        self.expect_char(')')?;
        self.expect_char('{')?;
        let (then_name, then_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        self.skip_ws();
        if !self.eat_kw("else") {
            return self.err("expected 'else'");
        }
        self.expect_char('{')?;
        let (else_name, else_rhs) = self.parse_var_plus_string_expr()?;
        self.expect_char('}')?;
        self.expect_char(';')?;

        // trailing expr: y + "..."
        let (use_y, tail_rhs) = self.parse_var_plus_string_expr()?;
        self.skip_ws();
        if !self.eof() {
            return self.err("unexpected trailing input");
        }

        if then_name != x || else_name != x {
            return self.err_at(self.i, format!("expected if-branches to use variable '{}'", x));
        }
        if use_y != y {
            return self.err_at(self.i, format!("expected trailing expression to use variable '{}'", y));
        }

        Ok((init, cond, then_rhs, else_rhs, tail_rhs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_error_reports_line_col_and_excerpt() {
        let src = "let x = \"ok\";\n\"\\u{110000}\"";
        let mut p = P::new(src);
        let err = p.parse_program().unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("parse error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("2:2"), "rendered:\n{rendered}");
        assert!(rendered.contains("^"), "rendered:\n{rendered}");
    }

    #[test]
    fn type_error_points_at_offending_expr() {
        let src = "while (1) { }\n\"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        let err = match jlyb::build_program_module(&prog) {
            Ok(_) => panic!("expected error, got Ok"),
            Err(e) => e,
        };
        let rendered = err.render(src, None);
        assert!(rendered.contains("type error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("while condition must be bool"), "rendered:\n{rendered}");
        // caret should be on the line with `while (1)`
        assert!(rendered.contains("while (1) { }"), "rendered:\n{rendered}");
        assert!(rendered.lines().any(|l| l.contains('^')), "rendered:\n{rendered}");
    }

    #[test]
    fn name_error_points_at_unknown_var() {
        let src = "\"a\" + x";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        let err = match jlyb::build_program_module(&prog) {
            Ok(_) => panic!("expected error, got Ok"),
            Err(e) => e,
        };
        let rendered = err.render(src, None);
        assert!(rendered.contains("name error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("unknown variable 'x'"), "rendered:\n{rendered}");
    }

    #[test]
    fn empty_array_literal_requires_and_uses_annotation() {
        let src = "let xs: Array<I32> = [];\n\"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        assert!(jlyb::build_program_module(&prog).is_ok());
    }

    #[test]
    fn unicode_surrogate_escape_is_rejected_u4() {
        let src = "\"\\uD800\"";
        let mut p = P::new(src);
        let err = p.parse_program().unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("invalid Unicode scalar (surrogate)"), "rendered:\n{rendered}");
    }

    #[test]
    fn unicode_surrogate_escape_is_rejected_braced() {
        let src = "\"\\u{D800}\"";
        let mut p = P::new(src);
        let err = p.parse_program().unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("invalid Unicode scalar (surrogate)"), "rendered:\n{rendered}");
    }

    #[test]
    fn block_scope_does_not_leak_bindings() {
        let src = "let y = if (true) { let x = 1; \"ok\" } else { \"bad\" }; x";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        let err = resolve::resolve_program(&prog).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("name error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("unknown variable 'x'"), "rendered:\n{rendered}");
    }

    #[test]
    fn fn_literal_capture_is_allowed() {
        let src = "let x = 1; let f : I32 -> I32 = fn(y) { return x; }; \"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn fn_literal_self_recursion_is_allowed() {
        let src = "let fib: I32 -> I32 = fn(n) { return fib(n); }; \"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn fn_literal_self_recursion_through_if_is_allowed() {
        let src = "let fib: I32 -> I32 = fn(n) { return if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }; }; \"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn ir_lowering_allows_self_recursion_through_if() {
        let src = "let fib: I32 -> I32 = fn(n) { return if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }; }; \"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        resolve::resolve_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&prog).unwrap();
    }

    #[test]
    fn import_from_binds_names_for_resolution() {
        let src = "import {add} from math; let r: I32 = add(1, 2); \"ok\"";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn import_from_binds_names_for_lowering_module_init() {
        let math_src = "export let add: (I32, I32) -> I32 = fn(a, b) { return a + b; }; \"math\"";
        let consts_src = "export let x: I32 = 4; \"consts\"";
        let entry_src = "import {add} from math;\nimport {x as y} from consts;\n\nlet r: I32 = add(y, 3);\nif (r == 7) { \"ok\" } else { \"bad\" }";

        let mut p = P::new(math_src);
        let math_prog = p.parse_program().unwrap();
        resolve::resolve_program(&math_prog).unwrap();
        let math_exports = collect_exports_from_program(&math_prog).unwrap();

        let mut p = P::new(consts_src);
        let consts_prog = p.parse_program().unwrap();
        resolve::resolve_program(&consts_prog).unwrap();
        let consts_exports = collect_exports_from_program(&consts_prog).unwrap();

        let mut p = P::new(entry_src);
        let entry_prog = p.parse_program().unwrap();
        resolve::resolve_program(&entry_prog).unwrap();

        let import_exports: HashMap<String, HashMap<String, lower::TypeRepr>> = HashMap::from([
            ("math".to_string(), math_exports),
            ("consts".to_string(), consts_exports),
        ]);

        lower::lower_module_init_to_ir("__entry__", &entry_prog, true, &import_exports).unwrap();
    }
}

#[derive(Clone)]
enum LoadedFile {
    Source { path: PathBuf, src: String, prog: Program },
    Bytecode { path: PathBuf, module: jlyb::Module, abi: jlyb::ModuleAbi },
}

#[derive(Clone)]
struct ModuleNode {
    key: String, // dotted module key, or "__entry__"
    file: LoadedFile,
    import_keys: Vec<String>, // module keys, in init param order
    exports: HashMap<String, lower::TypeRepr>,
}

enum ModuleLoadError {
    Io { path: PathBuf, msg: String },
    Compile { path: PathBuf, src: String, err: CompileError },
    Bytecode { path: PathBuf, msg: String },
    NotFound { key: String, tried: Vec<PathBuf> },
    Cycle { key: String },
}

impl ModuleLoadError {
    fn render(&self) -> String {
        match self {
            ModuleLoadError::Io { path, msg } => format!("error: failed to read {}: {}", path.display(), msg),
            ModuleLoadError::Compile { path, src, err } => err.render(src, Some(&path.display().to_string())),
            ModuleLoadError::Bytecode { path, msg } => format!("error: failed to load {}: {}", path.display(), msg),
            ModuleLoadError::NotFound { key, tried } => {
                let mut s = format!("name error: module not found '{}'\ntried:\n", key);
                for p in tried {
                    s.push_str(&format!("  {}\n", p.display()));
                }
                s
            }
            ModuleLoadError::Cycle { key } => format!("name error: import cycle involving '{}'", key),
        }
    }
}

fn collect_import_keys_from_program(p: &Program) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for s in &p.stmts {
        let key = match &s.node {
            StmtKind::ImportModule { path, .. } => Some(path.join(".")),
            StmtKind::ImportFrom { from, .. } => Some(from.join(".")),
            _ => None,
        };
        if let Some(k) = key {
            if seen.insert(k.clone()) {
                out.push(k);
            }
        }
    }
    out
}

fn collect_exports_from_program(p: &Program) -> Result<HashMap<String, lower::TypeRepr>, CompileError> {
    let mut out: HashMap<String, lower::TypeRepr> = HashMap::new();
    for s in &p.stmts {
        if let StmtKind::Let {
            exported: true,
            name,
            ty: Some(ty),
            ..
        } = &s.node
        {
            out.insert(name.clone(), lower::type_repr_from_ty(ty)?);
        }
    }
    Ok(out)
}

fn load_module_graph(entry_path: &PathBuf) -> Result<(Vec<ModuleNode>, usize, PathBuf), ModuleLoadError> {
    let root_dir = entry_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();

    let mut nodes: Vec<ModuleNode> = Vec::new();
    let mut ids: HashMap<String, usize> = HashMap::new();
    let mut visiting: HashSet<String> = HashSet::new();

    fn resolve_key_to_file(root: &PathBuf, key: &str) -> Result<(PathBuf, bool), ModuleLoadError> {
        let parts: Vec<&str> = key.split('.').collect();
        let mut rel = PathBuf::new();
        for p in parts {
            rel.push(p);
        }
        let base = root.join(rel);
        let jelly = base.with_extension("jelly");
        let jlyb = base.with_extension("jlyb");
        if std::fs::metadata(&jelly).is_ok() {
            return Ok((jelly, true));
        }
        if std::fs::metadata(&jlyb).is_ok() {
            return Ok((jlyb, false));
        }
        Err(ModuleLoadError::NotFound {
            key: key.to_string(),
            tried: vec![jelly, jlyb],
        })
    }

    fn load_one(
        root: &PathBuf,
        key: String,
        entry_path: &PathBuf,
        nodes: &mut Vec<ModuleNode>,
        ids: &mut HashMap<String, usize>,
        visiting: &mut HashSet<String>,
    ) -> Result<usize, ModuleLoadError> {
        if visiting.contains(&key) {
            return Err(ModuleLoadError::Cycle { key });
        }
        if let Some(&idx) = ids.get(&key) {
            return Ok(idx);
        }
        visiting.insert(key.clone());

        let (path, is_source) = if key == "__entry__" {
            (entry_path.clone(), true)
        } else {
            resolve_key_to_file(root, &key)?
        };

        let (file, import_keys, exports) = if is_source {
            let src = std::fs::read_to_string(&path).map_err(|e| ModuleLoadError::Io {
                path: path.clone(),
                msg: e.to_string(),
            })?;
            let mut p = P::new(&src);
            let mut prog = p
                .parse_program()
                .map_err(|e| ModuleLoadError::Compile { path: path.clone(), src: src.clone(), err: e })?;
            templates::expand_templates(&mut prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
            resolve::resolve_program(&prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
            let import_keys = collect_import_keys_from_program(&prog);
            let exports = collect_exports_from_program(&prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
            (LoadedFile::Source { path: path.clone(), src, prog }, import_keys, exports)
        } else {
            let data = std::fs::read(&path).map_err(|e| ModuleLoadError::Io {
                path: path.clone(),
                msg: e.to_string(),
            })?;
            let mut cur = std::io::Cursor::new(&data);
            let module = jlyb::Module::read_from(&mut cur)
                .map_err(|e| ModuleLoadError::Bytecode { path: path.clone(), msg: e.to_string() })?;
            let abi = jlyb::extract_module_abi(&module)
                .ok_or_else(|| ModuleLoadError::Bytecode { path: path.clone(), msg: "missing module ABI".to_string() })?;
            let mut exports: HashMap<String, lower::TypeRepr> = HashMap::new();
            for (name, tid) in &abi.exports {
                let tr = lower::type_repr_from_jlyb(&module, *tid).map_err(|e| ModuleLoadError::Bytecode {
                    path: path.clone(),
                    msg: e.message,
                })?;
                exports.insert(name.clone(), tr);
            }
            (LoadedFile::Bytecode { path: path.clone(), module, abi: abi.clone() }, abi.imports.clone(), exports)
        };

        let idx = nodes.len();
        nodes.push(ModuleNode {
            key: key.clone(),
            file,
            import_keys: import_keys.clone(),
            exports,
        });
        ids.insert(key.clone(), idx);

        // Load dependencies.
        for dep in import_keys {
            let _ = load_one(root, dep, entry_path, nodes, ids, visiting)?;
        }

        visiting.remove(&key);
        Ok(idx)
    }

    let entry_idx = load_one(&root_dir, "__entry__".to_string(), entry_path, &mut nodes, &mut ids, &mut visiting)?;

    // Toposort from entry.
    let mut order: Vec<usize> = Vec::new();
    let mut seen: Vec<bool> = vec![false; nodes.len()];
    fn dfs(i: usize, nodes: &Vec<ModuleNode>, ids: &HashMap<String, usize>, seen: &mut Vec<bool>, out: &mut Vec<usize>) {
        if seen[i] {
            return;
        }
        seen[i] = true;
        for k in &nodes[i].import_keys {
            if let Some(&j) = ids.get(k) {
                dfs(j, nodes, ids, seen, out);
            }
        }
        out.push(i);
    }
    dfs(entry_idx, &nodes, &ids, &mut seen, &mut order);

    // Rebuild nodes in topo order and remap indices.
    let mut remap: HashMap<usize, usize> = HashMap::new();
    for (new_i, &old_i) in order.iter().enumerate() {
        remap.insert(old_i, new_i);
    }
    let mut topo: Vec<ModuleNode> = Vec::with_capacity(order.len());
    for &old_i in &order {
        topo.push(nodes[old_i].clone());
    }
    let entry_new = *remap.get(&entry_idx).expect("entry remap");
    Ok((topo, entry_new, root_dir))
}

fn build_module_abi_blob(exports: &HashMap<String, u32>, import_keys: &[String]) -> Vec<u8> {
    fn wr_u32le(out: &mut Vec<u8>, v: u32) {
        out.extend_from_slice(&v.to_le_bytes());
    }

    let mut out: Vec<u8> = Vec::new();
    out.extend_from_slice(b"JLYMODABI1\0");

    let mut names: Vec<(&String, &u32)> = exports.iter().collect();
    names.sort_by(|a, b| a.0.cmp(b.0));
    wr_u32le(&mut out, names.len() as u32);
    for (name, tid) in names {
        wr_u32le(&mut out, name.len() as u32);
        out.extend_from_slice(name.as_bytes());
        wr_u32le(&mut out, *tid);
    }

    wr_u32le(&mut out, import_keys.len() as u32);
    for k in import_keys {
        wr_u32le(&mut out, k.len() as u32);
        out.extend_from_slice(k.as_bytes());
    }

    out
}

fn link_modules_and_build_entry(
    mods: &[jlyb::Module],
    import_lists: &[Vec<usize>],
    entry_idx: usize,
) -> Result<jlyb::Module, String> {
    const BASE_TYPES: usize = 10;
    const TUPLE_TAG: u32 = 0x8000_0000;

    if mods.len() != import_lists.len() {
        return Err("internal: mods/import_lists length mismatch".to_string());
    }

    let base_types = crate::typectx::TypeCtx::new_program_base().types;
    let mut out = jlyb::Module {
        types: base_types,
        sigs: Vec::new(),
        atoms: vec![b"__proto__".to_vec(), b"init".to_vec()],
        const_bytes: Vec::new(),
        funcs: jlyb::prelude_funcs_for_program(),
        entry: 0,
    };

    let mut init_func_indices: Vec<u32> = vec![0; mods.len()];
    let mut atom_ids: HashMap<String, u32> =
        HashMap::from([("__proto__".to_string(), 0u32), ("init".to_string(), 1u32)]);

    for (mi, m) in mods.iter().enumerate() {
        if (m.funcs.len() as u32) < jlyb::PRELUDE_FUN_COUNT {
            return Err("module missing prelude".to_string());
        }
        if m.types.len() < BASE_TYPES {
            return Err("module missing base types".to_string());
        }
        if m.atoms.len() < 2 {
            return Err("module missing base atoms".to_string());
        }

        let type_extra_base = (out.types.len() - BASE_TYPES) as u32;
        let sig_off = out.sigs.len() as u32;
        let bytes_off = out.const_bytes.len() as u32;
        let func_base = out.funcs.len() as u32;

        let map_tid = |tid: u32| -> u32 {
            if (tid as usize) < BASE_TYPES {
                tid
            } else {
                (BASE_TYPES as u32) + type_extra_base + (tid - (BASE_TYPES as u32))
            }
        };
        let map_sig = |sid: u32| -> u32 { sig_off + sid };
        let map_fun = |fid: u32| -> u32 {
            if fid < jlyb::PRELUDE_FUN_COUNT {
                fid
            } else {
                func_base + (fid - jlyb::PRELUDE_FUN_COUNT)
            }
        };

        // atoms: dedup by string so ObjSetAtom/ObjGetAtom agree across modules.
        let mut atom_map: Vec<u32> = Vec::with_capacity(m.atoms.len());
        for a in &m.atoms {
            let s = std::str::from_utf8(a).map_err(|_| "module atom is not UTF-8".to_string())?;
            if let Some(&id) = atom_ids.get(s) {
                atom_map.push(id);
            } else {
                let id = out.atoms.len() as u32;
                out.atoms.push(a.clone());
                atom_ids.insert(s.to_string(), id);
                atom_map.push(id);
            }
        }
        let map_atom = |aid: u32| -> Result<u32, String> {
            atom_map
                .get(aid as usize)
                .copied()
                .ok_or_else(|| "bad atom id".to_string())
        };

        // signatures
        for s in &m.sigs {
            out.sigs.push(jlyb::FunSig {
                ret_type: map_tid(s.ret_type),
                args: s.args.iter().map(|&a| map_tid(a)).collect(),
            });
        }

        // types (skip base 0..BASE_TYPES)
        for t in m.types.iter().skip(BASE_TYPES) {
            let mut p0 = t.p0;
            match t.kind {
                jlyb::TypeKind::Array | jlyb::TypeKind::List => {
                    p0 = map_tid(p0);
                }
                jlyb::TypeKind::Function => {
                    p0 = map_sig(p0);
                }
                jlyb::TypeKind::Object => {
                    if (p0 & TUPLE_TAG) != 0 {
                        let sid = p0 & !TUPLE_TAG;
                        p0 = TUPLE_TAG | map_sig(sid);
                    }
                }
                _ => {}
            }
            out.types.push(jlyb::TypeEntry { kind: t.kind, p0 });
        }

        // const bytes
        for b in &m.const_bytes {
            out.const_bytes.push(b.clone());
        }

        // funcs (skip prelude)
        for f in m.funcs.iter().skip(jlyb::PRELUDE_FUN_COUNT as usize) {
            let mut reg_types: Vec<u32> = Vec::with_capacity(f.reg_types.len());
            for &rt in &f.reg_types {
                reg_types.push(map_tid(rt));
            }
            let mut insns: Vec<jlyb::Insn> = Vec::with_capacity(f.insns.len());
            for ins in &f.insns {
                let mut outi = *ins;
                let op = outi.op;
                if op == jlyb::Op::ConstBytes as u8 {
                    outi.imm = bytes_off + outi.imm;
                } else if op == jlyb::Op::ConstAtom as u8
                    || op == jlyb::Op::ObjHasAtom as u8
                    || op == jlyb::Op::ObjGetAtom as u8
                    || op == jlyb::Op::ObjSetAtom as u8
                {
                    outi.imm = map_atom(outi.imm)?;
                } else if op == jlyb::Op::ConstFun as u8
                    || op == jlyb::Op::Call as u8
                    || op == jlyb::Op::Closure as u8
                {
                    outi.imm = map_fun(outi.imm);
                }
                insns.push(outi);
            }
            out.funcs.push(jlyb::Function { reg_types, insns });
        }

        init_func_indices[mi] = map_fun(m.entry);
    }

    // Build final entry wrapper.
    let nmods = mods.len() as u8;
    let max_imports = import_lists.iter().map(|v| v.len()).max().unwrap_or(0) as u8;
    let argwin = (1u8).saturating_add(max_imports);
    let r_dyn = nmods.saturating_add(argwin);
    let r_bytes = r_dyn.saturating_add(1);
    let nregs = (r_bytes as u32) + 1;
    if nregs > 256 {
        return Err("module linker: wrapper register allocation exceeded 256 regs".to_string());
    }

    let mut reg_types: Vec<u32> = Vec::with_capacity(nregs as usize);
    for _ in 0..nmods {
        reg_types.push(6); // Object
    }
    for _ in 0..argwin {
        reg_types.push(6); // Object
    }
    reg_types.push(3); // Dynamic
    reg_types.push(0); // Bytes

    let mut insns: Vec<jlyb::Insn> = Vec::new();
    // exports objects
    for i in 0..nmods {
        insns.push(jlyb::Insn { op: jlyb::Op::ObjNew as u8, a: i, b: 0, c: 0, imm: 0 });
    }

    let arg_base = nmods;
    for (i, deps) in import_lists.iter().enumerate() {
        // arg0 = exports[i]
        insns.push(jlyb::Insn { op: jlyb::Op::Mov as u8, a: arg_base, b: i as u8, c: 0, imm: 0 });
        for (j, &dep) in deps.iter().enumerate() {
            insns.push(jlyb::Insn {
                op: jlyb::Op::Mov as u8,
                a: arg_base + 1 + (j as u8),
                b: dep as u8,
                c: 0,
                imm: 0,
            });
        }
        let nargs = (1 + deps.len()) as u8;
        let dst = if i == entry_idx { r_bytes } else { r_dyn };
        insns.push(jlyb::Insn {
            op: jlyb::Op::Call as u8,
            a: dst,
            b: arg_base,
            c: nargs,
            imm: init_func_indices[i],
        });
    }

    insns.push(jlyb::Insn { op: jlyb::Op::Ret as u8, a: r_bytes, b: 0, c: 0, imm: 0 });

    let wrapper_index = out.funcs.len() as u32;
    out.funcs.push(jlyb::Function { reg_types, insns });
    out.entry = wrapper_index;
    Ok(out)
}

fn main() {
    let mut args = std::env::args().skip(1);
    let first = match args.next() {
        Some(a) => a,
        None => usage(),
    };

    if first == "prelude" {
        let mut out: Option<PathBuf> = None;
        while let Some(a) = args.next() {
            match a.as_str() {
                "--out" => {
                    let p = args.next().unwrap_or_else(|| usage());
                    out = Some(PathBuf::from(p));
                }
                "--help" | "-h" => usage(),
                _ => usage(),
            }
        }
        let out = out.unwrap_or_else(|| usage());
        let m = jlyb::build_prelude_module();
        let mut f = std::fs::File::create(&out).unwrap_or_else(|e| {
            eprintln!("error: failed to create {}: {}", out.display(), e);
            std::process::exit(2);
        });
        m.write_to(&mut f).unwrap_or_else(|e| {
            eprintln!("error: failed to write {}: {}", out.display(), e);
            std::process::exit(2);
        });
        return;
    }

    let input = if first.starts_with('-') {
        usage()
    } else {
        PathBuf::from(first)
    };

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Backend {
        Ast,
        Ir,
    }

    let mut out: Option<PathBuf> = None;
    let mut backend: Backend = Backend::Ast;
    while let Some(a) = args.next() {
        match a.as_str() {
            "--out" => {
                let p = args.next().unwrap_or_else(|| usage());
                out = Some(PathBuf::from(p));
            }
            "--backend" => {
                let b = args.next().unwrap_or_else(|| usage());
                backend = match b.as_str() {
                    "ast" => Backend::Ast,
                    "ir" => Backend::Ir,
                    _ => usage(),
                };
            }
            "--help" | "-h" => usage(),
            _ => usage(),
        }
    }

    let out = out.unwrap_or_else(|| {
        let mut p = input.clone();
        p.set_extension("jlyb");
        p
    });

    let m = match backend {
        Backend::Ast => {
            let src = read_to_string(&input);
            let mut p = P::new(&src);
            let mut prog = match p.parse_program() {
                Ok(x) => x,
                Err(e) => {
                    eprintln!("{}", e.render(&src, Some(&input.display().to_string())));
                    std::process::exit(1);
                }
            };
            if let Err(e) = templates::expand_templates(&mut prog) {
                eprintln!("{}", e.render(&src, Some(&input.display().to_string())));
                std::process::exit(1);
            }
            if let Err(e) = resolve::resolve_program(&prog) {
                eprintln!("{}", e.render(&src, Some(&input.display().to_string())));
                std::process::exit(1);
            }
            jlyb::build_program_module(&prog)
        }
        Backend::Ir => {
            let (nodes, entry_idx, _root_dir) = load_module_graph(&input).unwrap_or_else(|e| {
                eprintln!("{}", e.render());
                std::process::exit(1);
            });

            let mut key_to_index: HashMap<String, usize> = HashMap::new();
            for (i, n) in nodes.iter().enumerate() {
                key_to_index.insert(n.key.clone(), i);
            }

            let mut artifacts: Vec<jlyb::Module> = Vec::with_capacity(nodes.len());
            let mut import_lists: Vec<Vec<usize>> = Vec::with_capacity(nodes.len());

            for (i, n) in nodes.iter().enumerate() {
                let deps: Vec<usize> = n
                    .import_keys
                    .iter()
                    .map(|k| *key_to_index.get(k).expect("dep in graph"))
                    .collect();
                import_lists.push(deps);

                match &n.file {
                    LoadedFile::Source { path, src, prog } => {
                        // Build import interfaces from dependency export maps.
                        let mut import_exports: HashMap<String, HashMap<String, lower::TypeRepr>> = HashMap::new();
                        for k in &n.import_keys {
                            let di = *key_to_index.get(k).expect("dep index");
                            import_exports.insert(k.clone(), nodes[di].exports.clone());
                        }

                        let lowered = lower::lower_module_init_to_ir(&n.key, prog, i == entry_idx, &import_exports)
                            .unwrap_or_else(|e| {
                                eprintln!("{}", e.render(src, Some(&path.display().to_string())));
                                std::process::exit(1);
                            });

                        let mut irm = lowered.ir;
                        if let Err(e) = phi::eliminate_phis(&mut irm) {
                            eprintln!("{}", e.render(src, Some(&path.display().to_string())));
                            std::process::exit(1);
                        }
                        let mut bc = ir_codegen::emit_ir_module(&irm).unwrap_or_else(|e| {
                            eprintln!("{}", e.render(src, Some(&path.display().to_string())));
                            std::process::exit(1);
                        });
                        let abi_blob = build_module_abi_blob(&lowered.exports, &lowered.import_keys);
                        bc.const_bytes.push(abi_blob);
                        artifacts.push(bc);
                    }
                    LoadedFile::Bytecode { module, .. } => {
                        artifacts.push(module.clone());
                    }
                }
            }

            let linked = link_modules_and_build_entry(&artifacts, &import_lists, entry_idx).unwrap_or_else(|msg| {
                eprintln!("codegen error: {}", msg);
                std::process::exit(1);
            });
            Ok(linked)
        }
    }
    .unwrap_or_else(|e| {
        // Best-effort rendering: for IR modules, per-file errors should have been
        // rendered already during loading. For single-file AST backend, this path
        // uses the current input's source.
        let src = read_to_string(&input);
        eprintln!("{}", e.render(&src, Some(&input.display().to_string())));
        std::process::exit(1);
    });
    let mut f = std::fs::File::create(&out).unwrap_or_else(|e| {
        eprintln!("error: failed to create {}: {}", out.display(), e);
        std::process::exit(2);
    });
    m.write_to(&mut f).unwrap_or_else(|e| {
        eprintln!("error: failed to write {}: {}", out.display(), e);
        std::process::exit(2);
    });
}

