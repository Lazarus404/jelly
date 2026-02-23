use super::*;

#[test]
fn normalize_eq_true_to_truthy() {
    let src = "let x = 1; if (x == true) { \"ok\" } else { \"bad\" }";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let (hir, _info) = analyze_prepared_program(_prepared).unwrap();
    let dumped = crate::hir::render_hir(&hir, &SemanticInfo::default());
    assert!(dumped.contains("Truthy"), "dumped:\n{dumped}");
}
