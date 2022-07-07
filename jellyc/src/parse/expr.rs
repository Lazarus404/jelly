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

// Expression parsing.

use crate::ast::{Expr, ExprKind, MatchArm, Span, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::token::{BacktickPart, TokenKind};

use super::P;
use super::pattern;

pub fn parse_expr(p: &mut P) -> Result<Expr, CompileError> {
    if let Some(try_span) = p.eat_kw_span("try") {
        return parse_try_expr(p, try_span);
    }
    if let Some(if_span) = p.eat_kw_span("if") {
        return parse_if_expr(p, if_span);
    }
    parse_or_expr(p)
}

fn parse_try_expr(p: &mut P, try_span: Span) -> Result<Expr, CompileError> {
    let body = parse_block_expr(p)?;
    let _catch_span =
        p.eat_kw_span("catch").ok_or_else(|| CompileError::at(ErrorKind::Parse, p.pos(), "expected 'catch'"))?;
    let catch_name = if p.eat(TokenKind::LParen) {
        let name = p.parse_ident()?;
        p.expect_char(')')?;
        Some(name)
    } else {
        None
    };
    let catch_body = parse_block_expr(p)?;
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

fn parse_or_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_and_expr(p)?;
    loop {
        if p.eat(TokenKind::PipePipe) {
            let rhs = parse_and_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Or(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_and_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_eq_expr(p)?;
    loop {
        if p.eat(TokenKind::AmpAmp) {
            let rhs = parse_eq_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::And(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_eq_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_rel_expr(p)?;
    loop {
        if p.eat(TokenKind::EqEq) {
            let rhs = parse_rel_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Eq(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::NotEq) {
            let rhs = parse_rel_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Ne(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_rel_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_add_expr(p)?;
    loop {
        if p.eat(TokenKind::Le) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Le(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Ge) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Ge(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Lt) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Lt(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Gt) {
            let rhs = parse_add_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Gt(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_add_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_mul_expr(p)?;
    loop {
        if p.eat(TokenKind::Plus) {
            let lhs = e;
            let rhs = parse_mul_expr(p)?;
            let span = Span::new(lhs.span.start, rhs.span.end);
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
        } else if p.eat(TokenKind::Minus) {
            let rhs = parse_mul_expr(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Sub(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_mul_expr(p: &mut P) -> Result<Expr, CompileError> {
    let mut e = parse_term(p)?;
    loop {
        if p.eat(TokenKind::Star) {
            let rhs = parse_term(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Mul(Box::new(e), Box::new(rhs)), span);
        } else if p.eat(TokenKind::Slash) {
            let rhs = parse_term(p)?;
            let span = Span::new(e.span.start, rhs.span.end);
            e = Expr::new(ExprKind::Div(Box::new(e), Box::new(rhs)), span);
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_term(p: &mut P) -> Result<Expr, CompileError> {
    if let Some(new_span) = p.eat_kw_span("new") {
        let mut proto = parse_primary(p)?;
        loop {
            if p.eat_char('.') {
                let name = parse_member_name(p)?;
                let start = proto.span.start;
                proto = Expr::new(
                    ExprKind::Member {
                        base: Box::new(proto),
                        name,
                    },
                    Span::new(start, p.last_span_end()),
                );
            } else if p.peek_kind() == Some(&TokenKind::Lt) {
                let save = p.i;
                if let Ok(targs) = parse_type_arg_list(p) {
                    let start = proto.span.start;
                    proto = Expr::new(
                        ExprKind::TypeApp {
                            base: Box::new(proto),
                            type_args: targs,
                        },
                        Span::new(start, p.last_span_end()),
                    );
                } else {
                    p.i = save;
                    break;
                }
            } else if p.eat(TokenKind::LBracket) {
                let start = proto.span.start;
                let idx = p.parse_expr()?;
                p.expect_char(']')?;
                proto = Expr::new(
                    ExprKind::Index {
                        base: Box::new(proto),
                        index: Box::new(idx),
                    },
                    Span::new(start, p.last_span_end()),
                );
            } else {
                break;
            }
        }

        let args = parse_paren_expr_list(p)?;
        let span = Span::new(new_span.start, p.last_span_end());
        return Ok(Expr::new(
            ExprKind::New {
                proto: Box::new(proto),
                args,
            },
            span,
        ));
    }
    if p.eat(TokenKind::Not) {
        let start = p.pos();
        let inner = parse_term(p)?;
        let end = inner.span.end;
        return Ok(Expr::new(
            ExprKind::Not(Box::new(inner)),
            Span::new(start, end),
        ));
    }
    if let Some(t) = p.peek() {
        if matches!(t.kind, TokenKind::Minus) {
            let minus_start = t.span.start;
            p.bump();
            let inner = parse_term(p)?;
            let end = inner.span.end;
            return Ok(Expr::new(
                ExprKind::Neg(Box::new(inner)),
                Span::new(minus_start, end),
            ));
        }
    }

    let mut e = parse_primary(p)?;
    loop {
        if p.eat_char('.') {
            let name = parse_member_name(p)?;
            let start = e.span.start;
            e = Expr::new(
                ExprKind::Member {
                    base: Box::new(e),
                    name,
                },
                Span::new(start, p.last_span_end()),
            );
        } else if p.peek_kind() == Some(&TokenKind::Lt) {
            let save = p.i;
            if let Ok(targs) = (|| -> Result<Vec<Ty>, CompileError> {
                let targs = parse_type_arg_list(p)?;
                if p.peek_kind() != Some(&TokenKind::LParen) {
                    return Err(CompileError::at(ErrorKind::Parse, save, "not type args"));
                }
                Ok(targs)
            })() {
                let start = e.span.start;
                let args = parse_paren_expr_list(p)?;
                e = Expr::new(
                    ExprKind::Call {
                        callee: Box::new(e),
                        type_args: targs,
                        args,
                    },
                    Span::new(start, p.last_span_end()),
                );
            } else {
                p.i = save;
                if let Ok(targs) = (|| -> Result<Vec<Ty>, CompileError> {
                    let targs = parse_type_arg_list(p)?;
                    if p.peek_kind() == Some(&TokenKind::LParen) {
                        return Err(CompileError::at(ErrorKind::Parse, save, "not type app"));
                    }
                    Ok(targs)
                })() {
                    let start = e.span.start;
                    e = Expr::new(
                        ExprKind::TypeApp {
                            base: Box::new(e),
                            type_args: targs,
                        },
                        Span::new(start, p.last_span_end()),
                    );
                } else {
                    p.i = save;
                    break;
                }
            }
        } else if p.peek_kind() == Some(&TokenKind::LParen) {
            let args = parse_paren_expr_list(p)?;
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
                    Span::new(start, p.last_span_end()),
                ),
                _ => Expr::new(
                    ExprKind::Call {
                        callee: Box::new(e),
                        type_args: Vec::new(),
                        args,
                    },
                    Span::new(start, p.last_span_end()),
                ),
            };
        } else if p.eat(TokenKind::LBracket) {
            let idx = p.parse_expr()?;
            p.expect_char(']')?;
            let start = e.span.start;
            e = Expr::new(
                ExprKind::Index {
                    base: Box::new(e),
                    index: Box::new(idx),
                },
                Span::new(start, p.last_span_end()),
            );
        } else {
            break;
        }
    }
    Ok(e)
}

fn parse_primary(p: &mut P) -> Result<Expr, CompileError> {
    if let Some(fn_span) = p.eat_kw_span("fn") {
        return parse_fn_expr(p, fn_span);
    }
    if p.peek_kind() == Some(&TokenKind::Colon) {
        let start = p.pos();
        p.expect_char(':')?;
        let name = p.parse_ident_or_keyword()?;
        return Ok(Expr::new(
            ExprKind::AtomLit(name),
            Span::new(start, p.last_span_end()),
        ));
    }
    if let Some(if_span) = p.eat_kw_span("if") {
        return parse_if_expr(p, if_span);
    }
    if let Some(match_span) = p.eat_kw_span("match") {
        return parse_match_expr(p, match_span);
    }
    if let Some(kw_span) = p.eat_kw_span("true") {
        return Ok(Expr::new(ExprKind::BoolLit(true), kw_span));
    }
    if let Some(kw_span) = p.eat_kw_span("false") {
        return Ok(Expr::new(ExprKind::BoolLit(false), kw_span));
    }
    if let Some(kw_span) = p.eat_kw_span("null") {
        return Ok(Expr::new(ExprKind::Null, kw_span));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I8Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::I8Lit(0))?;
        let v = match t.kind {
            TokenKind::I8Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Expr::new(ExprKind::I8Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I16Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::I16Lit(0))?;
        let v = match t.kind {
            TokenKind::I16Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Expr::new(ExprKind::I16Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I32Lit(_))) == Some(true) {
        let start = p.pos();
        let (v, span) = p.parse_i32_lit_signed(start)?;
        return Ok(Expr::new(ExprKind::I32Lit(v), span));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::I64Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::I64Lit(0))?;
        let v = match t.kind {
            TokenKind::I64Lit(n) => n,
            _ => unreachable!(),
        };
        return Ok(Expr::new(ExprKind::I64Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::F16Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::F16Lit(0.0))?;
        let v = match t.kind {
            TokenKind::F16Lit(x) => x,
            _ => unreachable!(),
        };
        return Ok(Expr::new(ExprKind::F16Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::F64Lit(_))) == Some(true) {
        let start = p.pos();
        let t = p.expect(TokenKind::F64Lit(0.0))?;
        let v = match t.kind {
            TokenKind::F64Lit(x) => x,
            _ => unreachable!(),
        };
        return Ok(Expr::new(ExprKind::F64Lit(v), Span::new(start, p.last_span_end())));
    }
    if p.peek_kind().map(|k| matches!(k, TokenKind::BytesLit(_))) == Some(true) {
        return parse_bytes_lit_expr(p);
    }
    if let Some(TokenKind::BacktickString(_)) = p.peek_kind() {
        return parse_backtick_interp(p);
    }
    if p.peek_kind() == Some(&TokenKind::LParen) {
        return parse_paren_expr(p);
    }
    if p.peek_kind() == Some(&TokenKind::LBrace) {
        return parse_brace_expr(p);
    }
    if p.peek_kind() == Some(&TokenKind::LBracket) {
        return parse_array_lit(p, p.pos());
    }
    let start = p.pos();
    let name = p.parse_ident()?;
    Ok(Expr::new(ExprKind::Var(name), Span::new(start, p.last_span_end())))
}

fn parse_paren_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    p.expect_char('(')?;
    if p.eat(TokenKind::RParen) {
        return Ok(Expr::new(ExprKind::TupleLit(vec![]), Span::new(start, p.last_span_end())));
    }

    let first = p.parse_expr()?;
    if p.eat_char(',') {
        let mut elems: Vec<Expr> = vec![first];
        if p.peek_kind() != Some(&TokenKind::RParen) {
            loop {
                let e = p.parse_expr()?;
                elems.push(e);
                if p.eat_char(',') {
                    if p.peek_kind() == Some(&TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        p.expect_char(')')?;
        return Ok(Expr::new(ExprKind::TupleLit(elems), Span::new(start, p.last_span_end())));
    }

    p.expect_char(')')?;
    let mut e = first;
    e.span = Span::new(start, p.last_span_end());
    Ok(e)
}

fn parse_if_expr(p: &mut P, if_span: Span) -> Result<Expr, CompileError> {
    p.expect_char('(')?;
    let cond = p.parse_expr()?;
    p.expect_char(')')?;
    let then_br = parse_block_expr(p)?;
    let _else_span =
        p.eat_kw_span("else").ok_or_else(|| CompileError::at(ErrorKind::Parse, p.pos(), "expected 'else'"))?;
    let else_br = parse_block_expr(p)?;
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

fn parse_match_expr(p: &mut P, match_span: Span) -> Result<Expr, CompileError> {
    p.expect_char('(')?;
    let subject = p.parse_expr()?;
    p.expect_char(')')?;
    p.expect_char('{')?;
    let mut arms: Vec<MatchArm> = Vec::new();
    loop {
        if p.eat(TokenKind::RBrace) {
            break;
        }
        let pat = pattern::parse_pattern(p)?;
        let when = if p.eat_kw_span("when").is_some() {
            p.expect_char('(')?;
            let w = p.parse_expr()?;
            p.expect_char(')')?;
            Some(w)
        } else {
            None
        };
        if !p.eat(TokenKind::FatArrow) {
            return p.err("expected '=>'");
        }
        let (body, tail) = if p.peek_kind() == Some(&TokenKind::LBrace) {
            // Full arm body: `=> { stmt* expr? }`
            parse_fn_body(p)?
        } else {
            // Expression-only arm: `=> expr`
            let e = p.parse_expr()?;
            (Vec::new(), Some(e))
        };
        arms.push(MatchArm {
            pat,
            when,
            body,
            tail,
        });
        p.eat_char(',');
    }
    Ok(Expr::new(
        ExprKind::Match {
            subject: Box::new(subject),
            arms,
        },
        Span::new(match_span.start, p.last_span_end()),
    ))
}

fn parse_block_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    p.expect_char('{')?;
    let mut stmts: Vec<Stmt> = Vec::new();
    loop {
        if p.peek_kind() == Some(&TokenKind::RBrace) {
            return p.err("expected expression before '}'");
        }
        let save = p.i;
        if let Some(s) = p.parse_stmt()? {
            stmts.push(s);
            continue;
        }
        p.i = save;
        let e = p.parse_expr()?;
        p.expect_char('}')?;
        let span = Span::new(start, p.last_span_end());
        return Ok(Expr::new(
            ExprKind::Block {
                stmts,
                expr: Box::new(e),
            },
            span,
        ));
    }
}

fn parse_fn_body(p: &mut P) -> Result<(Vec<Stmt>, Option<Expr>), CompileError> {
    p.expect_char('{')?;
    let mut stmts: Vec<Stmt> = Vec::new();
    loop {
        if p.eat(TokenKind::RBrace) {
            return Ok((stmts, None));
        }
        let save = p.i;
        if let Some(s) = p.parse_stmt()? {
            stmts.push(s);
            continue;
        }
        p.i = save;
        let e = p.parse_expr()?;
        p.expect_char('}')?;
        return Ok((stmts, Some(e)));
    }
}

fn parse_brace_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    let save = p.i;
    p.expect_char('{')?;
    if p.eat(TokenKind::RBrace) {
        return Ok(Expr::new(ExprKind::ObjLit(vec![]), Span::new(start, p.last_span_end())));
    }
    if p.peek_is_ident_start() {
        let key = p.parse_ident_or_keyword()?;
        if p.eat_char(':') {
            let mut fields: Vec<(String, Expr)> = Vec::new();
            let v = p.parse_expr()?;
            fields.push((key, v));
            loop {
                if p.eat_char(',') {
                    let k = p.parse_ident_or_keyword()?;
                    p.expect_char(':')?;
                    let vv = p.parse_expr()?;
                    fields.push((k, vv));
                    continue;
                }
                break;
            }
            p.expect_char('}')?;
            return Ok(Expr::new(ExprKind::ObjLit(fields), Span::new(start, p.last_span_end())));
        }
    }
    p.i = save;
    let mut e = parse_block_expr(p)?;
    e.span = Span::new(start, e.span.end);
    Ok(e)
}

fn parse_fn_expr(p: &mut P, fn_span: Span) -> Result<Expr, CompileError> {
    p.expect_char('(')?;
    let mut params: Vec<(String, Option<Ty>)> = Vec::new();
    if p.peek_kind() != Some(&TokenKind::RParen) {
        loop {
            let name = p.parse_ident()?;
            let ty = if p.eat_char(':') {
                Some(p.parse_type()?)
            } else {
                None
            };
            params.push((name, ty));
            if !p.eat_char(',') {
                break;
            }
        }
    }
    p.expect_char(')')?;
    let (body, tail) = parse_fn_body(p)?;
    let span = Span::new(fn_span.start, p.last_span_end());
    Ok(Expr::new(
        ExprKind::Fn {
            params,
            body,
            tail: tail.map(Box::new),
        },
        span,
    ))
}

fn parse_member_name(p: &mut P) -> Result<String, CompileError> {
    if let Some(t) = p.peek() {
        if let TokenKind::I32Lit(n) = &t.kind {
            let n_val = *n;
            p.bump();
            return Ok(n_val.to_string());
        }
        if let TokenKind::I64Lit(n) = &t.kind {
            let n_val = *n;
            p.bump();
            return Ok(n_val.to_string());
        }
        if t.ident_or_keyword_str().is_some() {
            return p.parse_ident_or_keyword();
        }
    }
    p.parse_ident()
}

fn parse_type_arg_list(p: &mut P) -> Result<Vec<Ty>, CompileError> {
    p.expect_char('<')?;
    let mut args = vec![p.parse_type()?];
    while p.eat_char(',') {
        args.push(p.parse_type()?);
    }
    p.expect_char('>')?;
    Ok(args)
}

fn parse_paren_expr_list(p: &mut P) -> Result<Vec<Expr>, CompileError> {
    p.expect_char('(')?;
    let mut args: Vec<Expr> = Vec::new();
    if p.peek_kind() != Some(&TokenKind::RParen) {
        loop {
            let a = p.parse_expr()?;
            args.push(a);
            if !p.eat_char(',') {
                break;
            }
        }
    }
    p.expect_char(')')?;
    Ok(args)
}

fn parse_bytes_lit_expr(p: &mut P) -> Result<Expr, CompileError> {
    let start = p.pos();
    let b = p.expect_bytes_lit()?;
    Ok(Expr::new(ExprKind::BytesLit(b), Span::new(start, p.last_span_end())))
}

fn parse_array_lit(p: &mut P, start: usize) -> Result<Expr, CompileError> {
    p.expect_char('[')?;
    let mut elems: Vec<Expr> = Vec::new();
    if p.peek_kind() != Some(&TokenKind::RBracket) {
        loop {
            let e = p.parse_expr()?;
            elems.push(e);
            if !p.eat_char(',') {
                break;
            }
        }
    }
    p.expect_char(']')?;
    Ok(Expr::new(ExprKind::ArrayLit(elems), Span::new(start, p.last_span_end())))
}

fn parse_backtick_interp(p: &mut P) -> Result<Expr, CompileError> {
    let t = match p.bump() {
        Some(t) => t,
        None => return p.err("expected backtick string"),
    };
    let (parts, start) = match &t.kind {
        TokenKind::BacktickString(parts) => (parts.clone(), t.span.start),
        _ => return p.err("expected backtick string"),
    };

    let mut exprs: Vec<Expr> = Vec::new();
    let mut lit: Vec<u8> = Vec::new();
    let mut lit_start = start;

    let flush_lit = |exprs: &mut Vec<Expr>, lit: &mut Vec<u8>, lit_start: usize, lit_end: usize| {
        if !lit.is_empty() {
            let bytes = std::mem::take(lit);
            exprs.push(Expr::new(ExprKind::BytesLit(bytes), Span::new(lit_start, lit_end)));
        }
    };

    let mut pos = start;
    for part in &parts {
        match part {
            BacktickPart::Literal(bytes) => {
                if !bytes.is_empty() {
                    let end = pos + bytes.len();
                    exprs.push(Expr::new(
                        ExprKind::BytesLit(bytes.clone()),
                        Span::new(pos, end),
                    ));
                    pos = end;
                }
            }
            BacktickPart::Interpolation(expr_src) => {
                flush_lit(&mut exprs, &mut lit, lit_start, pos);
                let e = p.parse_expr_from_src(expr_src)?;
                pos = e.span.end;
                lit_start = pos;
                match e.node {
                    ExprKind::BytesLit(b) => {
                        lit.extend_from_slice(&b);
                    }
                    _ => {
                        exprs.push(e);
                    }
                }
            }
        }
    }
    flush_lit(&mut exprs, &mut lit, lit_start, t.span.end);

    if exprs.is_empty() {
        return Ok(Expr::new(ExprKind::BytesLit(Vec::new()), t.span));
    }
    let mut it = exprs.into_iter();
    let mut acc = it.next().expect("non-empty parts");
    for part in it {
        let span = Span::new(acc.span.start, part.span.end);
        acc = match (acc.node, part.node) {
            (ExprKind::BytesLit(mut a), ExprKind::BytesLit(b)) => {
                a.extend_from_slice(&b);
                Expr::new(ExprKind::BytesLit(a), span)
            }
            (a, b) => Expr::new(
                ExprKind::Add(
                    Box::new(Expr::new(a, acc.span)),
                    Box::new(Expr::new(b, part.span)),
                ),
                span,
            ),
        };
    }
    acc.span = t.span;
    Ok(acc)
}
