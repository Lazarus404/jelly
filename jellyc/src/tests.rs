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

    use crate::jlyb;
    use crate::link;
    use crate::lower;
    use crate::parse;
    use crate::phi;
    use crate::resolve;

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
        let src = "while (1) { }\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        let err = match jlyb::build_program_module(&prog) {
            Ok(_) => panic!("expected error, got Ok"),
            Err(e) => e,
        };
        let rendered = err.render(src, None);
        assert!(rendered.contains("type error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("while condition must be bool"), "rendered:\n{rendered}");
        assert!(rendered.contains("while (1) { }"), "rendered:\n{rendered}");
        assert!(rendered.lines().any(|l| l.contains('^')), "rendered:\n{rendered}");
    }

    #[test]
    fn name_error_points_at_unknown_var() {
        let src = "\"a\" + x";
        let prog = parse::parse_program(src).unwrap();
        let err = match jlyb::build_program_module(&prog) {
            Ok(_) => panic!("expected error, got Ok"),
            Err(e) => e,
        };
        let rendered = err.render(src, None);
        assert!(rendered.contains("name error:"), "rendered:\n{rendered}");
        assert!(rendered.contains("unknown variable 'x'"), "rendered:\n{rendered}");
    }

    #[test]
    fn empty_array_literal_requires_and_uses_annotation() {
        let src = "let xs: Array<I32> = [];\n\"ok\"";
        let prog = parse::parse_program(src).unwrap();
        assert!(jlyb::build_program_module(&prog).is_ok());
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
