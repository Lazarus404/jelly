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

use std::collections::HashMap;

use crate::lower;
use crate::parse;
use crate::phi;

#[test]
fn empty_array_literal_requires_and_uses_annotation() {
    let src = "let xs: Array<I32> = [];\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn block_scope_does_not_leak_bindings() {
    let src = "let y = if (true) { let x = 1; \"ok\" } else { \"bad\" }; x";
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
fn fn_literal_capture_is_allowed() {
    let src = "let x = 1; let f : I32 -> I32 = fn(y) { return x; }; \"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
}

#[test]
fn fn_literal_self_recursion_is_allowed() {
    let src = "let fib: I32 -> I32 = fn(n) { return fib(n); }; \"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
}

#[test]
fn fn_literal_self_recursion_through_if_is_allowed() {
    let src = "let fib: I32 -> I32 = fn(n) { return if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }; }; \"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
}

#[test]
fn ir_lowering_allows_self_recursion_through_if() {
    let src = "let fib: I32 -> I32 = fn(n) { return if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }; }; \"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn ir_phi_elimination_handles_nested_if_in_branch() {
    // Regression test: `if` expression lowering must use the *branch end blocks*,
    // not the branch entry blocks, when creating Phi incomings.
    let src =
        "let x: I32 = if (true) { if (true) { 1 } else { 2 } } else { 3 };\nif (x == 1) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> = HashMap::new();
    let (hir, info) =
        crate::semantic::analyze_prepared_module_init("__entry__", prepared, true, false, &import_exports)
            .unwrap();
    let lowered =
        lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, false, &import_exports)
            .unwrap();
    let mut irm = lowered.ir;
    phi::eliminate_phis(&mut irm).unwrap();
}

#[test]
fn null_literal_can_flow_to_object_kind_types_in_ir_lowering() {
    // `null` can be used where pointer-typed values are expected; it lowers as ConstNull + FromDynPtr.
    let src = "let o: Object = null; let a: Array<I32> = null; \"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn float_relational_ops_lower_in_ir_backend() {
    let src = "let a: F64 = 1.0; let b: F64 = 2.0; if (a < b) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn numeric_conversion_builtins_lower_in_ir_backend() {
    let src = "let x: F64 = Float.to_f64(3); let y: I32 = Integer.to_i32(x); if (y == 3) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn object_get_can_use_let_annotation_without_type_arg_in_ir_backend() {
    // Regression: lowering used to force `Object.get` to return Dynamic unless `<T>` was provided,
    // which made `let v: Bytes = Object.get(o, k)` fail during lowering.
    let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, \"ok\");\nlet v: Bytes = Object.get(o, k);\nSystem.assert(Bytes.len(v) == 2);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn object_get_uses_contextual_expected_type_without_type_arg() {
    // Ensure semantic analysis pins `Object.get` return type from context so lowering can be mechanical.
    let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, 123);\nlet v: I32 = Object.get(o, k);\nif (v == 123) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn nominal_object_types_flow_through_semantic_and_lowering() {
    // Regression: nominal object kinds (e.g. `Foo`) should work with object literals, `new`,
    // member access, and Object.* builtins.
    let src = "let proto: Foo = { a: 1 };\nlet inst: Foo = new proto();\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(inst, k, 123);\nlet x: I32 = inst.a;\nlet y: I32 = Object.get<I32>(inst, k);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn let_binding_does_not_alias_initializer_vreg_in_ir_backend() {
    // Regression test: `let y = x; y = ...` must not mutate x (no aliasing).
    let src = "let x: I32 = 1; let y: I32 = x; y = 2; if (x == 1) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
}
