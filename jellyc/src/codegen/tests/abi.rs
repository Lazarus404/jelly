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
fn try_catch_closure_and_call_windows_validate() {
    // Stress a few invariants together:
    // - try/catch lowering + exception-aware SSA/pinning
    // - closure creation (capture window pinning)
    // - a larger-arity call (arg window pinning)
    //
    // We mostly care that the emitted bytecode validates and includes the expected ops.
    let mut src = String::new();
    src.push_str("let h: (Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any) -> Any = fn(a,b,c,d,e,f,g,h,i,j,k,l) { a };\n");
    src.push_str("let f = fn() {\n");
    for i in 0..32 {
        src.push_str(&format!("  let d{i} = null;\n"));
    }
    // Closure with captures (forces CLOSURE + capture-window handling).
    src.push_str("  let g = fn() { d0; d1; d2; d3; d4; d5; d6; d7; d8; d9; d10; d11; d12; d13; d14; d15; d0 };\n");
    // Try/catch around a call + closure call.
    src.push_str("  let r = try {\n");
    src.push_str("    let x = h(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11);\n");
    src.push_str("    g();\n");
    src.push_str("    x\n");
    src.push_str("  } catch (e) { e };\n");
    src.push_str("  r\n");
    src.push_str("};\n");
    src.push_str("let _x = f();\n");
    src.push_str("\"ok\"");

    let prog = super::frontend_program_from_src(&src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }

    let mut saw_try = false;
    let mut saw_endtry = false;
    let mut saw_closure = false;
    let mut saw_call = false;
    for f in &m.funcs {
        for ins in &f.insns {
            if ins.op == Op::Try as u8 {
                saw_try = true;
            } else if ins.op == Op::EndTry as u8 {
                saw_endtry = true;
            } else if ins.op == Op::Closure as u8 {
                saw_closure = true;
            } else if ins.op == Op::Call as u8 || ins.op == Op::CallR as u8 {
                saw_call = true;
            }
        }
    }
    assert!(saw_try, "expected TRY in emitted bytecode");
    assert!(saw_endtry, "expected ENDTRY in emitted bytecode");
    assert!(saw_closure, "expected CLOSURE in emitted bytecode");
    assert!(saw_call, "expected a CALL/CALLR in emitted bytecode");
}

#[test]
fn spills_never_touch_call_or_capture_windows() {
    // This test enforces an important ABI invariant:
    // vregs pinned to call arg windows and closure capture windows must never spill.
    //
    // We check it at the bytecode level: any SpillPush/SpillPop reg must not be a reg that
    // participates in any Call/CallR arg window or Closure capture window in that function.
    let mut src = String::new();
    src.push_str("let h: (Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any) -> Any = fn(a,b,c,d,e,f,g,h,i,j,k,l) { a };\n");
    src.push_str("let f: () -> Any = fn() {\n");
    // Many Dynamic locals to force spills.
    let n: usize = 300;
    for i in 0..n {
        src.push_str(&format!("  let a{i} = null;\n"));
    }
    // Closure with captures (capture window).
    src.push_str("  let g = fn() { a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a0 };\n");
    // A big-arity call (call arg window).
    src.push_str("  let r = h(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);\n");
    // Use the closure so it's not trivially dead.
    src.push_str("  let _x = g();\n");
    // Add many throws to keep values live long enough to pressure regalloc.
    for i in 0..n {
        src.push_str(&format!("  throw a{i};\n"));
    }
    src.push_str("  r\n");
    src.push_str("};\n");
    // Call f so it is emitted.
    src.push_str("let _y = f();\n");
    src.push_str("\"ok\"");

    let prog = super::frontend_program_from_src(&src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }

    let mut saw_spill = false;
    for func in &m.funcs {
        // Collect "window regs" for this function from its insn stream.
        let mut window_regs: [bool; 256] = [false; 256];
        for ins in &func.insns {
            if ins.op == Op::Call as u8 {
                let base = ins.b as usize;
                let nargs = ins.c as usize;
                for r in base..base.saturating_add(nargs) {
                    if r < 256 {
                        window_regs[r] = true;
                    }
                }
            } else if ins.op == Op::CallR as u8 {
                let base = ins.imm as usize;
                let nargs = ins.c as usize;
                for r in base..base.saturating_add(nargs) {
                    if r < 256 {
                        window_regs[r] = true;
                    }
                }
            } else if ins.op == Op::Closure as u8 {
                let base = ins.b as usize;
                let ncaps = ins.c as usize;
                for r in base..base.saturating_add(ncaps) {
                    if r < 256 {
                        window_regs[r] = true;
                    }
                }
            }
        }

        for ins in &func.insns {
            if ins.op != Op::SpillPush as u8 && ins.op != Op::SpillPop as u8 {
                continue;
            }
            saw_spill = true;
            let r = ins.a as usize;
            assert!(
                r < 256 && !window_regs[r],
                "spill op used a call/capture window reg (r={r})"
            );
        }
    }
    assert!(
        saw_spill,
        "expected at least one spill op in emitted module"
    );
}

#[test]
fn callr_callee_reg_is_never_spilled_under_pressure() {
    // `CALLR` callee must never be spilled/reloaded (it can corrupt the closure object).
    // This is enforced in codegen invariants; here we assert it at the bytecode level under
    // high Dynamic pressure (so spills definitely occur).
    let mut src = String::new();
    src.push_str("let f: () -> Any = fn() {\n");
    let n: usize = 300;
    for i in 0..n {
        src.push_str(&format!("  let a{i} = null;\n"));
    }
    // Closure value lives in a local and is called indirectly (CALLR).
    src.push_str("  let g = fn() { a0 };\n");
    src.push_str("  let r = g();\n");
    // Keep lots of locals live to force spills around/after the call site.
    for i in 0..n {
        src.push_str(&format!("  throw a{i};\n"));
    }
    src.push_str("  r\n");
    src.push_str("};\n");
    src.push_str("let _x = f();\n");
    src.push_str("\"ok\"");

    let prog = super::frontend_program_from_src(&src);
    let (hir, info) =
        crate::semantic::analyze_prepared_program(crate::frontend::prepared(&prog)).unwrap();
    let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
    phi::eliminate_phis(&mut ir).unwrap();
    let m = emit_ir_module(&ir).unwrap();
    if let Err(msg) = crate::jlyb::validate_module(&m) {
        panic!("bytecode validation failed: {msg}");
    }

    let mut saw_callr = false;
    let mut saw_spill = false;
    for func in &m.funcs {
        let mut callee_regs: [bool; 256] = [false; 256];
        for ins in &func.insns {
            if ins.op == Op::CallR as u8 {
                saw_callr = true;
                let r = ins.b as usize;
                if r < 256 {
                    callee_regs[r] = true;
                }
            }
        }
        for ins in &func.insns {
            if ins.op != Op::SpillPush as u8 && ins.op != Op::SpillPop as u8 {
                continue;
            }
            saw_spill = true;
            let r = ins.a as usize;
            assert!(
                r < 256 && !callee_regs[r],
                "spill op used a CALLR callee reg (r={r})"
            );
        }
    }
    assert!(saw_callr, "expected CALLR in emitted module");
    assert!(
        saw_spill,
        "expected at least one spill op in emitted module"
    );
}
