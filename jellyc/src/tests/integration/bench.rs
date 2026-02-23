use crate::parse;

#[test]
fn bench_programs_compile_through_semantic_and_ir_lowering() {
    // Regression coverage: these programs previously failed after tightening "no-guess" lowering.
    // Keep this list small and focused; `bench/bench.py` is the fuller integration run.
    for src in [
        include_str!("../../../../bench/fannkuch.jelly"),
        include_str!("../../../../bench/fp.jelly"),
        include_str!("../../../../bench/nbodies.jelly"),
    ] {
        let mut prog = parse::parse_program(src).unwrap();
        let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
        let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }
}
