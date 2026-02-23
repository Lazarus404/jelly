use crate::ast::{Expr, MatchArm, Span};

use super::super::SemanticInfo;
use super::{render_expr, render_stmt};

pub(super) fn render_arm(a: &MatchArm, info: &SemanticInfo, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!("{pad}arm:\n"));
    if let Some(w) = &a.when {
        out.push_str(&format!("{pad}  when:\n"));
        render_expr(w, info, indent + 2, out);
    }
    for st in &a.body {
        render_stmt(st, info, indent + 1, out);
    }
    if let Some(t) = &a.tail {
        out.push_str(&format!("{pad}  tail:\n"));
        render_expr(t, info, indent + 2, out);
    }
}

pub(super) fn render_bin(
    op: &str,
    a: &Expr,
    b: &Expr,
    span: Span,
    ty_s: String,
    info: &SemanticInfo,
    indent: usize,
    out: &mut String,
) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!(
        "{pad}{op} : {ty_s} @{}..{}\n",
        span.start, span.end
    ));
    render_expr(a, info, indent + 1, out);
    render_expr(b, info, indent + 1, out);
}
