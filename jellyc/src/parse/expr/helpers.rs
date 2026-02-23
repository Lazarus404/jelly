use crate::ast::{Expr, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::token::TokenKind;

use super::super::P;

pub(super) fn parse_member_name(p: &mut P) -> Result<String, CompileError> {
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

pub(super) fn parse_type_arg_list(p: &mut P) -> Result<Vec<Ty>, CompileError> {
    p.expect_char('<')?;
    let mut args = vec![p.parse_type()?];
    while p.eat_char(',') {
        args.push(p.parse_type()?);
    }
    p.expect_char('>')?;
    Ok(args)
}

pub(super) fn parse_paren_expr_list(p: &mut P) -> Result<Vec<Expr>, CompileError> {
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

pub(super) fn try_parse_call_type_args(p: &mut P, save: usize) -> Result<Vec<Ty>, CompileError> {
    let targs = parse_type_arg_list(p)?;
    if p.peek_kind() != Some(&TokenKind::LParen) {
        return Err(CompileError::at(ErrorKind::Parse, save, "not type args"));
    }
    Ok(targs)
}

pub(super) fn try_parse_type_app(p: &mut P, save: usize) -> Result<Vec<Ty>, CompileError> {
    let targs = parse_type_arg_list(p)?;
    if p.peek_kind() == Some(&TokenKind::LParen) {
        return Err(CompileError::at(ErrorKind::Parse, save, "not type app"));
    }
    Ok(targs)
}
