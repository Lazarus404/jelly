/*
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

use crate::link;
use crate::lower;
use crate::parse;

#[test]
fn truthiness_normalization_is_visible_in_hir_dump() {
    let src = "let x = 1; if (x == true) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let dumped = crate::hir::render_hir(&hir, &info);
    assert!(dumped.contains("Truthy"), "dumped:\n{dumped}");
    assert!(!dumped.contains("Eq :"), "dumped:\n{dumped}");
}

#[test]
fn dump_snapshots_hir_and_ir() {
    let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, \"ok\");\nlet v: Bytes = Object.get(o, k);\nSystem.assert(Bytes.len(v) == 2);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_basic", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_basic", ir_dump);
}

#[test]
fn dump_snapshot_truthiness_hir() {
    let src = "let x = 1; if (x == true) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_truthiness", hir_dump);
}

#[test]
fn dump_snapshots_new_nominal_object_hir_and_ir() {
    let src = "let proto: Foo = { a: 1 };\nlet inst: Foo = new proto(2, \"hi\");\nlet _ = inst.a;\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_new_nominal", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_new_nominal", ir_dump);
}

#[test]
fn dump_snapshots_try_hir_and_ir() {
    let src = "try { throw \"boom\"; \"no\" } catch(e) { e; \"ok\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_try", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_try", ir_dump);
}

#[test]
fn dump_snapshots_match_hir_and_ir() {
    let src = "let x: I32 = 1; match(x) { 0 => \"zero\", 1 => \"one\", _ => \"other\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_match", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_match", ir_dump);
}

#[test]
fn dump_snapshots_match_dynamic_scalar_fastpath_hir_and_ir() {
    // This should trigger the `Dynamic` scalar-pattern fast-path in lowering:
    // it uses `Kindof` + `SwitchKind` instead of an if-chain.
    let src =
        "let o = { x: 1 };\nlet d = o.x;\nmatch(d) { 0 => \"zero\", 1 => \"one\", _ => \"other\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_match_dynamic_scalar", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_match_dynamic_scalar", ir_dump);
}

#[test]
fn dump_snapshots_object_get_context_vs_type_arg_hir_and_ir() {
    // Lock in the contract that `Object.get` return type is decided by semantic analysis,
    // either from contextual expected type or explicit `<T>` type-arg.
    let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, 123);\nlet a: I32 = Object.get(o, k);\nlet b = Object.get<I32>(o, k);\nSystem.assert(a == b);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_object_get_context_vs_type_arg", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_object_get_context_vs_type_arg", ir_dump);
}

#[test]
fn dump_snapshot_closure_capture_hir() {
    let src =
        "let x: I32 = 1;\nlet f: I32 -> I32 = fn(y) { return x + y; };\nlet _ = f(2);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_closure_capture", hir_dump);
}

#[test]
fn dump_snapshots_method_bind_this_hir_and_ir() {
    let src = "let inc: (Object, I32) -> I32 = fn(self, x) { return x + 1; };\nlet o = { inc: inc };\nlet m: I32 -> I32 = o.inc;\nlet r: I32 = m(41);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_method_bind_this", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_method_bind_this", ir_dump);
}

#[test]
fn dump_snapshot_dynamic_numeric_coercion_hir() {
    let src = "let o = { x: 1 };\nif (o.x < 2) { \"ok\" } else { \"bad\" }";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_dynamic_numeric", hir_dump);
}

#[test]
fn dump_snapshots_module_namespace_call_hir_and_ir() {
    let math_src = "export let add: (I32, I32) -> I32 = fn(a, b) { return a + b; }; \"math\"";
    let entry_src =
        "import math as Math;\nlet r: I32 = Math.add(1, 2);\nif (r == 3) { \"ok\" } else { \"bad\" }";

    let mut math_prog = parse::parse_program(math_src).unwrap();
    let _prepared_math = crate::frontend::prepare_program(&mut math_prog).unwrap();
    let math_exports = link::collect_exports_from_program(&math_prog).unwrap();

    let mut entry_prog = parse::parse_program(entry_src).unwrap();
    let prepared_entry = crate::frontend::prepare_program(&mut entry_prog).unwrap();

    let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
        HashMap::from([("math".to_string(), math_exports)]);

    let (hir, info) = crate::semantic::analyze_prepared_module_init(
        "__entry__",
        prepared_entry,
        true,
        false,
        &import_exports,
    )
    .unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_module_namespace_call", hir_dump);

    let lowered =
        lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, false, &import_exports)
            .unwrap();
    let ir_dump = crate::ir::render_ir(&lowered.ir);
    assert_repo_snapshot!("ir_module_namespace_call", ir_dump);
}

#[test]
fn dump_snapshots_tuple_access_hir_and_ir() {
    let src = "let t = (1, \"hi\");\nlet a: I32 = t.0;\nlet b: Bytes = t.1;\nSystem.assert(a == 1);\nSystem.assert(Bytes.len(b) == 2);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_tuple_access", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_tuple_access", ir_dump);
}

#[test]
fn dump_snapshots_new_with_init_hir_and_ir() {
    let src = "let init: (Object, I32) -> Object = fn(self, x) { let k = Atom.intern(\"a\"); let _ = Object.set(self, k, x); return self; };\nlet proto = { init: init };\nlet inst = new proto(7);\nlet k = Atom.intern(\"a\");\nlet v: I32 = Object.get<I32>(inst, k);\nSystem.assert(v == 7);\n\"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();

    let hir_dump = crate::hir::render_hir(&hir, &info);
    assert_repo_snapshot!("hir_new_init", hir_dump);

    let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    let ir_dump = crate::ir::render_ir(&ir);
    assert_repo_snapshot!("ir_new_init", ir_dump);
}
