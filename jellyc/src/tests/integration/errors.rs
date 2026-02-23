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

use crate::parse;

#[test]
fn parse_error_reports_line_col_and_excerpt() {
    let src = "let x = \"ok\";\n\"\\u{110000}\"";
    let err = parse::parse_program(src).unwrap_err();
    let rendered = err.render(src, None);
    assert!(rendered.contains("parse error:"), "rendered:\n{rendered}");
    assert!(rendered.contains("2:2"), "rendered:\n{rendered}");
    assert!(rendered.contains("^"), "rendered:\n{rendered}");
}

#[test]
fn type_error_points_at_offending_expr() {
    let src = "let x: I32 = \"ok\";\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let err = crate::semantic::analyze_prepared_program(prepared).unwrap_err();
    let rendered = err.render(src, None);
    assert!(rendered.contains("type error:"), "rendered:\n{rendered}");
    assert!(
        rendered.contains("type mismatch (no implicit conversion)"),
        "rendered:\n{rendered}"
    );
    assert!(
        rendered.contains("let x: I32 = \"ok\";"),
        "rendered:\n{rendered}"
    );
    assert!(
        rendered.lines().any(|l| l.contains('^')),
        "rendered:\n{rendered}"
    );
}

#[test]
fn name_error_points_at_unknown_var() {
    let src = "\"a\" + x";
    let mut prog = parse::parse_program(src).unwrap();
    let err = crate::frontend::prepare_program(&mut prog).unwrap_err();
    let rendered = err.render(src, None);
    assert!(rendered.contains("name error:"), "rendered:\n{rendered}");
    assert!(
        rendered.contains("unknown variable 'x'"),
        "rendered:\n{rendered}"
    );
}

#[test]
fn unicode_surrogate_escape_is_rejected_u4() {
    let src = "\"\\uD800\"";
    let err = parse::parse_program(src).unwrap_err();
    let rendered = err.render(src, None);
    assert!(
        rendered.contains("invalid Unicode scalar (surrogate)"),
        "rendered:\n{rendered}"
    );
}

#[test]
fn unicode_surrogate_escape_is_rejected_braced() {
    let src = "\"\\u{D800}\"";
    let err = parse::parse_program(src).unwrap_err();
    let rendered = err.render(src, None);
    assert!(
        rendered.contains("invalid Unicode scalar (surrogate)"),
        "rendered:\n{rendered}"
    );
}
