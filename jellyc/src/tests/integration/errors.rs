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
