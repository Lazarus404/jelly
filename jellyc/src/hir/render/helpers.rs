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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
