use crate::ast::{Expr, ExprKind, Span, Ty};
use crate::error::CompileError;
use crate::token::TokenKind;

use super::super::P;

use super::{control, helpers, literals, primary};

pub(super) fn parse_term(p: &mut P) -> Result<Expr, CompileError> {
    if let Some(new_span) = p.eat_kw_span("new") {
        let mut proto = primary::parse_primary(p)?;
        loop {
            if p.eat_char('.') {
                let name = helpers::parse_member_name(p)?;
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
                if let Ok(targs) = helpers::parse_type_arg_list(p) {
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

        let args = helpers::parse_paren_expr_list(p)?;
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

    let mut e = primary::parse_primary(p)?;
    loop {
        if p.eat_char('.') {
            let name = helpers::parse_member_name(p)?;
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
            if let Ok(targs) = helpers::try_parse_call_type_args(p, save) {
                let start = e.span.start;
                let args = helpers::parse_paren_expr_list(p)?;
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
                if let Ok(targs) = helpers::try_parse_type_app(p, save) {
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
            let args = helpers::parse_paren_expr_list(p)?;
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
            if p.eat(TokenKind::Eq) {
                let rhs = p.parse_expr()?;
                e = Expr::new(
                    ExprKind::IndexAssign {
                        base: Box::new(e),
                        index: Box::new(idx),
                        expr: Box::new(rhs),
                    },
                    Span::new(start, p.last_span_end()),
                );
            } else {
                e = Expr::new(
                    ExprKind::Index {
                        base: Box::new(e),
                        index: Box::new(idx),
                    },
                    Span::new(start, p.last_span_end()),
                );
            }
        } else {
            break;
        }
    }
    Ok(e)
}

#[allow(dead_code)]
type _Ty = Ty;

#[allow(dead_code)]
fn _keep_imports(_: &mut P) {
    let _ = control::parse_if_expr;
    let _ = literals::parse_backtick_interp;
}
