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

#[cfg(test)]
mod integration {
    use std::collections::HashMap;

    use crate::link;
    use crate::lower;
    use crate::parse;
    use crate::phi;
    use crate::resolve;
    use crate::templates;

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
        templates::expand_templates(&mut prog).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let err = crate::semantic::analyze_program(&prog).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("type error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("type mismatch (no implicit conversion)"), "rendered:\n{rendered}");
        assert!(rendered.contains("let x: I32 = \"ok\";"), "rendered:\n{rendered}");
        assert!(rendered.lines().any(|l| l.contains('^')), "rendered:\n{rendered}");
    }

    #[test]
    fn name_error_points_at_unknown_var() {
        let src = "\"a\" + x";
        let prog = parse::parse_program(src).unwrap();
        let err = resolve::resolve_program(&prog).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("name error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("unknown variable 'x'"), "rendered:\n{rendered}");
    }

    #[test]
    fn empty_array_literal_requires_and_uses_annotation() {
        let src = "let xs: Array<I32> = [];\n\"ok\"";
        let mut prog = parse::parse_program(src).unwrap();
        templates::expand_templates(&mut prog).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn unicode_surrogate_escape_is_rejected_u4() {
        let src = "\"\\uD800\"";
        let err = parse::parse_program(src).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("invalid Unicode scalar (surrogate)"), "rendered:\n{rendered}");
    }

    #[test]
    fn unicode_surrogate_escape_is_rejected_braced() {
        let src = "\"\\u{D800}\"";
        let err = parse::parse_program(src).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("invalid Unicode scalar (surrogate)"), "rendered:\n{rendered}");
    }

    #[test]
    fn block_scope_does_not_leak_bindings() {
        let src = "let y = if (true) { let x = 1; \"ok\" } else { \"bad\" }; x";
        let prog = parse::parse_program(src).unwrap();
        let err = resolve::resolve_program(&prog).unwrap_err();
        let rendered = err.render(src, None);
        assert!(rendered.contains("name error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("unknown variable 'x'"), "rendered:\n{rendered}");
    }

    #[test]
    fn fn_literal_capture_is_allowed() {
        let src = "let x = 1; let f : I32 -> I32 = fn(y) { return x; }; \"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn fn_literal_self_recursion_is_allowed() {
        let src = "let fib: I32 -> I32 = fn(n) { return fib(n); }; \"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn fn_literal_self_recursion_through_if_is_allowed() {
        let src = "let fib: I32 -> I32 = fn(n) { return if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }; }; \"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn ir_lowering_allows_self_recursion_through_if() {
        let src = "let fib: I32 -> I32 = fn(n) { return if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }; }; \"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn ir_phi_elimination_handles_nested_if_in_branch() {
        // Regression test: `if` expression lowering must use the *branch end blocks*,
        // not the branch entry blocks, when creating Phi incomings.
        let src =
            "let x: I32 = if (true) { if (true) { 1 } else { 2 } } else { 3 };\nif (x == 1) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> = HashMap::new();
        let (hir, info) = crate::semantic::analyze_module_init("__entry__", &prog, true, &import_exports).unwrap();
        let lowered = lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, &import_exports).unwrap();
        let mut irm = lowered.ir;
        phi::eliminate_phis(&mut irm).unwrap();
    }

    #[test]
    fn null_literal_can_flow_to_object_kind_types_in_ir_lowering() {
        // `null` can be used where pointer-typed values are expected; it lowers as ConstNull + FromDynPtr.
        let src = "let o: Object = null; let a: Array<I32> = null; \"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn float_relational_ops_lower_in_ir_backend() {
        let src = "let a: F64 = 1.0; let b: F64 = 2.0; if (a < b) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn numeric_conversion_builtins_lower_in_ir_backend() {
        let src = "let x: F64 = Float.to_f64(3); let y: I32 = Integer.to_i32(x); if (y == 3) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn object_get_can_use_let_annotation_without_type_arg_in_ir_backend() {
        // Regression: lowering used to force `Object.get` to return Dynamic unless `<T>` was provided,
        // which made `let v: Bytes = Object.get(o, k)` fail during lowering.
        let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, \"ok\");\nlet v: Bytes = Object.get(o, k);\nSystem.assert(Bytes.len(v) == 2);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn object_get_uses_contextual_expected_type_without_type_arg() {
        // Ensure semantic analysis pins `Object.get` return type from context so lowering can be mechanical.
        let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, 123);\nlet v: I32 = Object.get(o, k);\nif (v == 123) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn nominal_object_types_flow_through_semantic_and_lowering() {
        // Regression: nominal object kinds (e.g. `Foo`) should work with object literals, `new`,
        // member access, and Object.* builtins.
        let src = "let proto: Foo = { a: 1 };\nlet inst: Foo = new proto();\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(inst, k, 123);\nlet x: I32 = inst.a;\nlet y: I32 = Object.get<I32>(inst, k);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn truthiness_normalization_is_visible_in_hir_dump() {
        let src = "let x = 1; if (x == true) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let dumped = crate::hir::render_hir(&hir, &info);
        assert!(dumped.contains("Truthy"), "dumped:\n{dumped}");
        assert!(!dumped.contains("Eq :"), "dumped:\n{dumped}");
    }

    #[test]
    fn dump_snapshots_hir_and_ir() {
        let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, \"ok\");\nlet v: Bytes = Object.get(o, k);\nSystem.assert(Bytes.len(v) == 2);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_basic.snap");
        assert_eq!(hir_dump, hir_expected);

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_basic.snap");
        assert_eq!(ir_dump, ir_expected);
    }

    #[test]
    fn dump_snapshot_truthiness_hir() {
        let src = "let x = 1; if (x == true) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let hir_dump = crate::hir::render_hir(&hir, &info);
        let expected = include_str!("../../snapshots/hir_truthiness.snap");
        assert!(hir_dump == expected, "HIR dump:\n{hir_dump}");
    }

    #[test]
    fn dump_snapshots_new_nominal_object_hir_and_ir() {
        let src = "let proto: Foo = { a: 1 };\nlet inst: Foo = new proto(2, \"hi\");\nlet _ = inst.a;\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_new_nominal.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_new_nominal.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshots_try_hir_and_ir() {
        let src = "try { throw \"boom\"; \"no\" } catch(e) { e; \"ok\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_try.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_try.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshots_match_hir_and_ir() {
        let src = "let x: I32 = 1; match(x) { 0 => \"zero\", 1 => \"one\", _ => \"other\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_match.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_match.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshots_match_dynamic_scalar_fastpath_hir_and_ir() {
        // This should trigger the `Dynamic` scalar-pattern fast-path in lowering:
        // it uses `Kindof` + `SwitchKind` instead of an if-chain.
        let src = "let o = { x: 1 };\nlet d = o.x;\nmatch(d) { 0 => \"zero\", 1 => \"one\", _ => \"other\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_match_dynamic_scalar.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_match_dynamic_scalar.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshots_object_get_context_vs_type_arg_hir_and_ir() {
        // Lock in the contract that `Object.get` return type is decided by semantic analysis,
        // either from contextual expected type or explicit `<T>` type-arg.
        let src = "let o = { };\nlet k = Atom.intern(\"a\");\nlet _ = Object.set(o, k, 123);\nlet a: I32 = Object.get(o, k);\nlet b = Object.get<I32>(o, k);\nSystem.assert(a == b);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_object_get_context_vs_type_arg.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_object_get_context_vs_type_arg.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshot_closure_capture_hir() {
        let src = "let x: I32 = 1;\nlet f: I32 -> I32 = fn(y) { return x + y; };\nlet _ = f(2);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let hir_dump = crate::hir::render_hir(&hir, &info);
        let expected = include_str!("../../snapshots/hir_closure_capture.snap");
        assert!(hir_dump == expected, "HIR dump:\n{hir_dump}");
    }

    #[test]
    fn dump_snapshots_method_bind_this_hir_and_ir() {
        let src = "let inc: (Object, I32) -> I32 = fn(self, x) { return x + 1; };\nlet o = { inc: inc };\nlet m: I32 -> I32 = o.inc;\nlet r: I32 = m(41);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_method_bind_this.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_method_bind_this.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshot_dynamic_numeric_coercion_hir() {
        let src = "let o = { x: 1 };\nif (o.x < 2) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let hir_dump = crate::hir::render_hir(&hir, &info);
        let expected = include_str!("../../snapshots/hir_dynamic_numeric.snap");
        assert!(hir_dump == expected, "HIR dump:\n{hir_dump}");
    }

    #[test]
    fn dump_snapshots_module_namespace_call_hir_and_ir() {
        let math_src = "export let add: (I32, I32) -> I32 = fn(a, b) { return a + b; }; \"math\"";
        let entry_src = "import math as Math;\nlet r: I32 = Math.add(1, 2);\nif (r == 3) { \"ok\" } else { \"bad\" }";

        let math_prog = parse::parse_program(math_src).unwrap();
        resolve::resolve_program(&math_prog).unwrap();
        let math_exports = link::collect_exports_from_program(&math_prog).unwrap();

        let entry_prog = parse::parse_program(entry_src).unwrap();
        resolve::resolve_program(&entry_prog).unwrap();

        let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
            HashMap::from([("math".to_string(), math_exports)]);

        let (hir, info) =
            crate::semantic::analyze_module_init("__entry__", &entry_prog, true, &import_exports).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_module_namespace_call.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let lowered =
            lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, &import_exports).unwrap();
        let ir_dump = crate::ir::render_ir(&lowered.ir);
        let ir_expected = include_str!("../../snapshots/ir_module_namespace_call.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshots_tuple_access_hir_and_ir() {
        let src = "let t = (1, \"hi\");\nlet a: I32 = t.0;\nlet b: Bytes = t.1;\nSystem.assert(a == 1);\nSystem.assert(Bytes.len(b) == 2);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_tuple_access.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_tuple_access.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn dump_snapshots_new_with_init_hir_and_ir() {
        let src = "let init: (Object, I32) -> Object = fn(self, x) { let k = Atom.intern(\"a\"); let _ = Object.set(self, k, x); return self; };\nlet proto = { init: init };\nlet inst = new proto(7);\nlet k = Atom.intern(\"a\");\nlet v: I32 = Object.get<I32>(inst, k);\nSystem.assert(v == 7);\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();

        let hir_dump = crate::hir::render_hir(&hir, &info);
        let hir_expected = include_str!("../../snapshots/hir_new_init.snap");
        assert!(hir_dump == hir_expected, "HIR dump:\n{hir_dump}");

        let ir = crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        let ir_dump = crate::ir::render_ir(&ir);
        let ir_expected = include_str!("../../snapshots/ir_new_init.snap");
        assert!(ir_dump == ir_expected, "IR dump:\n{ir_dump}");
    }

    #[test]
    fn module_alias_can_shadow_builtin_namespace_in_lowering() {
        // Regression: `import foo as Bytes` must shadow the builtin `Bytes.*` namespace in lowering.
        // In particular, `Bytes.len(...)` must compile as a module export call (ObjGetAtom+Call),
        // not as the builtin BytesLen op.
        let mod_src = "export let len: (Bytes) -> I32 = fn(x) { return 0; }; \"m\"";
        let entry_src = "import m as Bytes;\nlet r: I32 = Bytes.len(\"hi\");\nif (r == 0) { \"ok\" } else { \"bad\" }";

        let mod_prog = parse::parse_program(mod_src).unwrap();
        resolve::resolve_program(&mod_prog).unwrap();
        let mod_exports = link::collect_exports_from_program(&mod_prog).unwrap();

        let entry_prog = parse::parse_program(entry_src).unwrap();
        resolve::resolve_program(&entry_prog).unwrap();

        let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
            HashMap::from([("m".to_string(), mod_exports)]);

        let (hir, info) =
            crate::semantic::analyze_module_init("__entry__", &entry_prog, true, &import_exports).unwrap();
        let lowered =
            lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, &import_exports).unwrap();
        let ir_dump = crate::ir::render_ir(&lowered.ir);

        assert!(
            !ir_dump.contains("BytesLen"),
            "expected module call, got builtin BytesLen:\n{ir_dump}"
        );
        assert!(
            ir_dump.contains("ObjGetAtom") && ir_dump.contains("Call {"),
            "expected module call sequence:\n{ir_dump}"
        );
    }

    #[test]
    fn bench_programs_compile_through_semantic_and_ir_lowering() {
        // Regression coverage: these programs previously failed after tightening "no-guess" lowering.
        // Keep this list small and focused; `bench/bench.py` is the fuller integration run.
        for src in [
            include_str!("../../bench/fannkuch.jelly"),
            include_str!("../../bench/fp.jelly"),
            include_str!("../../bench/nbodies.jelly"),
        ] {
            let prog = parse::parse_program(src).unwrap();
            resolve::resolve_program(&prog).unwrap();
            let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
            crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
        }
    }

    #[test]
    fn let_binding_does_not_alias_initializer_vreg_in_ir_backend() {
        // Regression test: `let y = x; y = ...` must not mutate x (no aliasing).
        let src = "let x: I32 = 1; let y: I32 = x; y = 2; if (x == 1) { \"ok\" } else { \"bad\" }";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        crate::lower::lower_program_to_ir(&hir.program, &info).unwrap();
    }

    #[test]
    fn import_from_binds_names_for_resolution() {
        let src = "import {add} from math; let r: I32 = add(1, 2); \"ok\"";
        let prog = parse::parse_program(src).unwrap();
        resolve::resolve_program(&prog).unwrap();
    }

    #[test]
    fn import_from_binds_names_for_lowering_module_init() {
        let math_src = "export let add: (I32, I32) -> I32 = fn(a, b) { return a + b; }; \"math\"";
        let consts_src = "export let x: I32 = 4; \"consts\"";
        let entry_src = "import {add} from math;\nimport {x as y} from consts;\n\nlet r: I32 = add(y, 3);\nif (r == 7) { \"ok\" } else { \"bad\" }";

        let math_prog = parse::parse_program(math_src).unwrap();
        resolve::resolve_program(&math_prog).unwrap();
        let math_exports = link::collect_exports_from_program(&math_prog).unwrap();

        let consts_prog = parse::parse_program(consts_src).unwrap();
        resolve::resolve_program(&consts_prog).unwrap();
        let consts_exports = link::collect_exports_from_program(&consts_prog).unwrap();

        let entry_prog = parse::parse_program(entry_src).unwrap();
        resolve::resolve_program(&entry_prog).unwrap();

        let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> = HashMap::from([
            ("math".to_string(), math_exports),
            ("consts".to_string(), consts_exports),
        ]);

        let (hir, info) = crate::semantic::analyze_module_init("__entry__", &entry_prog, true, &import_exports).unwrap();
        lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, &import_exports).unwrap();
    }
}
