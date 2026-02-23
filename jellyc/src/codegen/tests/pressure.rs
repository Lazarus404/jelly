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

use crate::jlyb::Op;
use crate::lower::lower_program_to_ir;
use crate::phi;

use super::super::emit_ir_module;

#[test]
fn emits_spills_under_high_dynamic_pressure() {
    // Force spills in a nested function by creating 255 live Dynamic vregs.
    // (Entry module must return Bytes and also reserves extra vregs; a nested
    // function avoids those constraints.)
    //
    // Keep total vregs in the nested function <= 256:
    // - 1 self-binding vreg (inserted by lowering for typed `let f = fn...` to allow recursion)
    // - 255 `Dynamic` vregs (`a0..a254`)
    let n: usize = 255;
    let mut src = String::new();
    src.push_str("let f: () -> Any = fn() {\n");
    for i in 0..n {
        src.push_str(&format!("  let a{i} = null;\n"));
    }
    for i in 0..n {
        src.push_str(&format!("  throw a{i};\n"));
    }
    src.push_str("  a0\n");
    src.push_str("};\n");
    src.push_str("let _x = f();\n");
    src.push_str("\"ok\"");

    let prog = super::frontend_program_from_src(&src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap_or_else(|e| {
        let lens: Vec<usize> = ir.funcs.iter().map(|f| f.vreg_types.len()).collect();
        panic!("emit failed: {:?}; vreg lens={lens:?}", e)
    });
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }
    let any_spill_push = m
        .funcs
        .iter()
        .any(|f| f.insns.iter().any(|i| i.op == Op::SpillPush as u8));
    let any_spill_pop = m
        .funcs
        .iter()
        .any(|f| f.insns.iter().any(|i| i.op == Op::SpillPop as u8));
    assert!(
        any_spill_push,
        "expected SpillPush in some emitted function"
    );
    assert!(any_spill_pop, "expected SpillPop in some emitted function");

    // Spill stack is boxed-only: SpillPush/SpillPop must operate on Dynamic regs only.
    for f in &m.funcs {
        for ins in &f.insns {
            if ins.op != Op::SpillPush as u8 && ins.op != Op::SpillPop as u8 {
                continue;
            }
            let r = ins.a as usize;
            let rt = f.reg_types.get(r).copied().unwrap_or(0);
            assert_eq!(
                rt,
                crate::typectx::T_DYNAMIC,
                "spill op used non-Dynamic reg type (reg {r} tid={rt})"
            );
        }
    }
}

#[test]
fn typed_register_pressure_errors() {
    // Typed vregs are non-spillable. Force allocation failure by creating >256
    // simultaneously-live I32 vregs inside a nested function.
    let n: usize = 300;
    let mut src = String::new();
    src.push_str("let g: () -> I32 = fn() {\n");
    for i in 0..n {
        src.push_str(&format!("  let a{i}: I32 = {i};\n"));
    }
    // Use all locals in a linear chain to keep them live without creating a
    // deeply nested parse tree (which can overflow the Rust test stack).
    src.push_str("  let s0: I32 = 0;\n");
    for i in 0..n {
        src.push_str(&format!("  let s{}: I32 = s{} + a{};\n", i + 1, i, i));
    }
    src.push_str(&format!("  s{}\n", n));
    src.push_str("};\n");
    src.push_str("let _x = g();\n");
    src.push_str("\"ok\"");

    let prog = super::frontend_program_from_src(&src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let e = match emit_ir_module(&ir) {
        Ok(_) => panic!("expected codegen error due to typed register pressure"),
        Err(e) => e,
    };
    assert_eq!(
        e.kind,
        crate::error::ErrorKind::Codegen,
        "expected Codegen error, got {:?}",
        e.kind
    );
    assert!(
        e.message.contains("register pressure too high"),
        "unexpected error message: {}",
        e.message
    );
}
