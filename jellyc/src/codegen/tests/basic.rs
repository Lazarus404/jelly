use crate::ast::{ExprKind, Program, Span, Spanned};
use crate::jlyb::Op;
use crate::lower::lower_program_to_ir;
use crate::parse;
use crate::phi;

use super::super::emit_ir_module;
use super::normalize_and_resolve;

#[test]
fn emit_minimal_bytes_ir() {
    let mut prog = Program {
        stmts: vec![],
        expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(0, 2)),
    };
    let _prepared = normalize_and_resolve(&mut prog);
    let (hir, info) = crate::semantic::analyze_prepared_program(_prepared).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
    // entry is logical index; funcs array index = entry - 1 (0 is native)
    let entry_func = (m.entry as usize).saturating_sub(1);
    assert_eq!(m.const_bytes.len(), 1);
    assert!(!m.funcs[entry_func].insns.is_empty());
    // We may emit additional prologue ops (eg. global object materialization),
    // but the function must still load the bytes constant and return it.
    assert!(
        m.funcs[entry_func]
            .insns
            .iter()
            .any(|i| i.op == Op::ConstBytes as u8),
        "expected ConstBytes in emitted bytecode"
    );
    assert_eq!(m.funcs[entry_func].insns.last().unwrap().op, Op::Ret as u8);
}

#[test]
fn emit_match_basic_ir() {
    let src = "let x = 2; match (x) { 1 => { \"bad\" }, 2 => { \"ok\" }, _ => { \"bad\" }, }";
    let mut prog = parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(_prepared).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap_or_else(|e| {
        panic!(
            "emit failed: {}\nIR:\n{:#?}",
            e.render(src, None),
            ir.funcs[0]
        );
    });
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
}

#[test]
fn emit_if_expression_ir_has_jumps() {
    let if_sp = Span::new(0, 1);
    let cond_sp = Span::new(2, 3);
    let then_sp = Span::new(4, 5);
    let else_sp = Span::new(6, 7);
    let mut prog = Program {
        stmts: vec![],
        expr: Spanned::new(
            ExprKind::If {
                cond: Box::new(Spanned::new(ExprKind::BoolLit(true), cond_sp)),
                then_br: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), then_sp)),
                else_br: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), else_sp)),
            },
            if_sp,
        ),
    };
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(_prepared).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
    let entry_func = (m.entry as usize).saturating_sub(1);
    assert!(m.funcs[entry_func]
        .insns
        .iter()
        .any(|i| i.op == Op::JmpIf as u8));
    assert!(m.funcs[entry_func]
        .insns
        .iter()
        .any(|i| i.op == Op::Jmp as u8));
}

#[test]
fn strict_validation_checks_direct_call_arg_types() {
    // Ensure the stricter validator sees at least one direct CALL and that caller arg types
    // match the callee's param register types.
    let src = r#"
let two: I32 = 2;
let f: (I32) -> I32 = fn(x: I32) { let one: I32 = 1; x + one };
let _y: I32 = f(two);
"ok"
"#;
    let prog = super::frontend_program_from_src(src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();

    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
    if let Err(msg) = crate::jlyb::validate_module_strict(&m) {
        panic!("strict bytecode validation failed: {msg}");
    }

    let saw_call = m
        .funcs
        .iter()
        .any(|f| f.insns.iter().any(|i| i.op == Op::Call as u8));
    assert!(
        saw_call,
        "expected at least one direct CALL (peephole should fold CONST_FUN+CALLR)"
    );
}

#[test]
fn const_fun_propagates_through_phi() {
    // When both branches of an if produce the same ConstFun, the phi-derived value
    // should be known at the merge, allowing direct CALL instead of CALLR.
    // Use a non-constant condition so the phi isn't folded away.
    let src = r#"
let f: (I32) -> I32 = fn(x: I32) { x };
let x: I32 = 1;
let h = if (x > 0) { f } else { f };
let _y: I32 = h(1);
"ok"
"#;
    let prog = super::frontend_program_from_src(src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();

    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
    // Some function should use direct CALL (not CALLR) for h(1) because phi
    // propagation knows h is ConstFun at the call site.
    let saw_direct_call = m
        .funcs
        .iter()
        .any(|f| f.insns.iter().any(|i| i.op == Op::Call as u8));
    let saw_indirect_call = m
        .funcs
        .iter()
        .any(|f| f.insns.iter().any(|i| i.op == Op::CallR as u8));
    assert!(
        saw_direct_call,
        "expected direct CALL when phi-derived callee is known ConstFun"
    );
    assert!(
        !saw_indirect_call,
        "phi propagation should avoid CALLR when both branches produce same ConstFun"
    );
}

#[test]
fn const_fun_for_method_refs() {
    // When obj has nominal type and method is in prototype table, emit ConstFun (direct CALL).
    let src = r#"
let init: (Object, I32) -> Object = fn(self, n) { self.n = n; return self; };
let get: (Object) -> I32 = fn(self) { return self.n; };
prototype P {
  init: init;
  get: get;
};
let o: P = new P(7);
let _h: I32 = o.get();
"ok"
"#;
    let prog = super::frontend_program_from_src(src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();

    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
    // Method call o.get() should use direct CALL (ConstFun) not CALLR.
    let saw_direct_call = m
        .funcs
        .iter()
        .any(|f| f.insns.iter().any(|i| i.op == Op::Call as u8));
    assert!(
        saw_direct_call,
        "expected direct CALL when method is resolvable (ConstFun for method refs)"
    );
}
