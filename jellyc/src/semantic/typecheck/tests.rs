use super::*;

use crate::hir::{ConstInit, NodeId};
use crate::typectx::T_BYTES;

#[test]
fn typecheck_minimal_bytes_program() {
    let src = "\"ok\"";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let info = typecheck_program(&p).unwrap();
    assert_eq!(
        info.expr_types.get(&NodeId(p.expr.span)).copied(),
        Some(T_BYTES)
    );
}

#[test]
fn const_inits_are_recorded_and_folded() {
    let src = "const x = 1 + 2 * 3; \"ok\"";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let info = typecheck_program(&p).unwrap();

    let s0 = &p.stmts[0];
    let init = info.const_inits.get(&NodeId(s0.span)).expect("const init");
    match init {
        ConstInit::Value(crate::hir::ConstValue::Int(7)) => {}
        other => panic!("unexpected const init: {other:?}"),
    }
}

#[test]
fn const_bytes_var_init_records_alias() {
    let src = "const a = \"A\"; const b = a; \"ok\"";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let info = typecheck_program(&p).unwrap();

    let s1 = &p.stmts[1];
    let init = info.const_inits.get(&NodeId(s1.span)).expect("const init");
    assert_eq!(init, &ConstInit::Alias("a".to_string()));
}
