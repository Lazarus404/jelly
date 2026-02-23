use crate::ast::Expr;
use crate::error::CompileError;

use super::super::P;

use super::{control, precedence};

pub fn parse_expr(p: &mut P) -> Result<Expr, CompileError> {
    if let Some(try_span) = p.eat_kw_span("try") {
        return control::parse_try_expr(p, try_span);
    }
    if let Some(if_span) = p.eat_kw_span("if") {
        return control::parse_if_expr(p, if_span);
    }
    precedence::parse_or_expr(p)
}
