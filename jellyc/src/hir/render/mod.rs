use super::{HirProgram, SemanticInfo};

mod expr;
mod helpers;
mod stmt;

use expr::render_expr;
use stmt::render_stmt;

pub fn render_hir(p: &HirProgram, info: &SemanticInfo) -> String {
    let mut out = String::new();
    out.push_str("hir {\n");
    for s in &p.program.stmts {
        render_stmt(s, info, 1, &mut out);
    }
    out.push_str("  expr:\n");
    render_expr(&p.program.expr, info, 2, &mut out);
    out.push_str("}\n");
    out
}
