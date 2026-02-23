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

use crate::link;
use crate::lower;
use crate::parse;

#[test]
fn import_from_binds_names_for_resolution() {
    let src = "import {add} from math; let r: I32 = add(1, 2); \"ok\"";
    let mut prog = parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
}

#[test]
fn import_from_binds_names_for_lowering_module_init() {
    let math_src = "export let add: (I32, I32) -> I32 = fn(a, b) { return a + b; }; \"math\"";
    let consts_src = "export let x: I32 = 4; \"consts\"";
    let entry_src = "import {add} from math;\nimport {x as y} from consts;\n\nlet r: I32 = add(y, 3);\nif (r == 7) { \"ok\" } else { \"bad\" }";

    let mut math_prog = parse::parse_program(math_src).unwrap();
    let _prepared_math = crate::frontend::prepare_program(&mut math_prog).unwrap();
    let math_exports = link::collect_exports_from_program(&math_prog).unwrap();

    let mut consts_prog = parse::parse_program(consts_src).unwrap();
    let _prepared_consts = crate::frontend::prepare_program(&mut consts_prog).unwrap();
    let consts_exports = link::collect_exports_from_program(&consts_prog).unwrap();

    let mut entry_prog = parse::parse_program(entry_src).unwrap();
    let prepared_entry = crate::frontend::prepare_program(&mut entry_prog).unwrap();

    let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
        HashMap::from([
            ("math".to_string(), math_exports),
            ("consts".to_string(), consts_exports),
        ]);

    let (hir, info) = crate::semantic::analyze_prepared_module_init(
        "__entry__",
        prepared_entry,
        true,
        false,
        &import_exports,
    )
    .unwrap();
    lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, false, &import_exports)
        .unwrap();
}

#[test]
fn module_alias_can_shadow_builtin_namespace_in_lowering() {
    // Regression: `import foo as Bytes` must shadow the builtin `Bytes.*` namespace in lowering.
    // In particular, `Bytes.len(...)` must compile as a module export call (ObjGetAtom+Call),
    // not as the builtin BytesLen op.
    let mod_src = "export let len: (Bytes) -> I32 = fn(x) { return 0; }; \"m\"";
    let entry_src =
        "import m as Bytes;\nlet r: I32 = Bytes.len(\"hi\");\nif (r == 0) { \"ok\" } else { \"bad\" }";

    let mut mod_prog = parse::parse_program(mod_src).unwrap();
    let _prepared_mod = crate::frontend::prepare_program(&mut mod_prog).unwrap();
    let mod_exports = link::collect_exports_from_program(&mod_prog).unwrap();

    let mut entry_prog = parse::parse_program(entry_src).unwrap();
    let prepared_entry = crate::frontend::prepare_program(&mut entry_prog).unwrap();

    let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
        HashMap::from([("m".to_string(), mod_exports)]);

    let (hir, info) = crate::semantic::analyze_prepared_module_init(
        "__entry__",
        prepared_entry,
        true,
        false,
        &import_exports,
    )
    .unwrap();
    let lowered =
        lower::lower_module_init_to_ir("__entry__", &hir.program, &info, true, false, &import_exports)
            .unwrap();
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
