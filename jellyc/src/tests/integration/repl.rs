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

// REPL integration tests: incremental compilation and session bindings.

use crate::repl::{compile_repl_incremental, compile_repl_source, ReplSession};
use crate::typectx::TypeRepr;

#[test]
fn compile_repl_source_returns_bindings() {
    let (linked, _artifact, bindings) = compile_repl_source("let i = 1 + 1", true).unwrap();
    assert!(bindings.contains_key("i"));
    assert!(matches!(bindings.get("i"), Some(TypeRepr::I32)));

    let mut bytecode = Vec::new();
    linked.write_to(&mut bytecode).unwrap();
    assert!(!bytecode.is_empty());
}

#[test]
fn compile_repl_incremental_imports_prior_bindings() {
    let (_linked1, artifact, bindings) = compile_repl_source("let i = 1 + 1", true).unwrap();
    assert!(bindings.contains_key("i"));

    let session = ReplSession {
        accumulated: "let i = 1 + 1".to_string(),
        bindings,
        prior_artifacts: vec![artifact],
        #[cfg(feature = "embed-vm")]
        session_exports: None,
        #[cfg(feature = "embed-vm")]
        last_line_had_assign: false,
    };

    let (linked2, _chunk, merged_bindings, _) = compile_repl_incremental("i", &session).unwrap();
    assert!(merged_bindings.contains_key("i"));

    let mut bytecode = Vec::new();
    linked2.write_to(&mut bytecode).unwrap();
    assert!(!bytecode.is_empty());

    #[cfg(feature = "embed-vm")]
    {
        let mut vm = crate::vm::ReplVm::new().unwrap();
        let out = vm.exec(&bytecode, u32::MAX, None).unwrap();
        assert_eq!(out.trim(), "2");
    }
}

#[test]
fn compile_repl_incremental_adds_new_bindings() {
    let (_, artifact, bindings) = compile_repl_source("let i = 1", true).unwrap();

    let session = ReplSession {
        accumulated: "let i = 1".to_string(),
        bindings,
        prior_artifacts: vec![artifact],
        #[cfg(feature = "embed-vm")]
        session_exports: None,
        #[cfg(feature = "embed-vm")]
        last_line_had_assign: false,
    };

    let (_, _, merged, _) = compile_repl_incremental("let j = i + 1", &session).unwrap();
    assert!(merged.contains_key("i"));
    assert!(merged.contains_key("j"));
}

/// Sustained incremental: prior_artifacts are preserved after incremental so the next line
/// can also use incremental (no full recompile).
#[test]
fn compile_repl_sustained_incremental() {
    let (_, artifact, bindings) = compile_repl_source("let i = 1", true).unwrap();

    let mut session = ReplSession {
        accumulated: "let i = 1".to_string(),
        bindings,
        prior_artifacts: vec![artifact],
        #[cfg(feature = "embed-vm")]
        session_exports: None,
        #[cfg(feature = "embed-vm")]
        last_line_had_assign: false,
    };

    // First incremental: "let j = i + 1"
    let (_linked2, _, merged, new_artifact) = compile_repl_incremental("let j = i + 1", &session).unwrap();
    session.accumulated = "let i = 1\nlet j = i + 1".to_string();
    session.bindings = merged;
    session.prior_artifacts.push(new_artifact);

    // Second incremental: "j" — should use incremental (prior_artifacts non-empty)
    let (linked3, _, _, _) = compile_repl_incremental("j", &session).unwrap();
    assert!(!linked3.funcs.is_empty());
}

/// Assignment as expression: i = i + 1 should output 13 (the assigned value).
#[cfg(feature = "embed-vm")]
#[test]
fn repl_assign_outputs_value() {
    let (linked1, artifact, bindings) = compile_repl_source("let i = 12", true).unwrap();
    let session = ReplSession {
        accumulated: "let i = 12".to_string(),
        bindings,
        prior_artifacts: vec![artifact],
        session_exports: None,
        last_line_had_assign: false,
    };
    let mut vm = crate::vm::ReplVm::new().unwrap();
    let mut exports = 0usize;
    let out1 = vm.exec(
        &{
            let mut b = Vec::new();
            linked1.write_to(&mut b).unwrap();
            b
        },
        0,
        Some(&mut exports),
    )
    .unwrap();
    assert_eq!(out1.trim(), "12");

    let (linked2, _chunk, _, _) = compile_repl_incremental("i = i + 1", &session).unwrap();
    let out2 = vm
        .exec(
            &{
                let mut b = Vec::new();
                linked2.write_to(&mut b).unwrap();
                b
            },
            1,
            None,
        )
        .unwrap();
    assert_eq!(out2.trim(), "13", "assignment should output the assigned value");
}

/// Single-module: let i = 12; i = i - 1; i should output 11 (assignments update exports).
#[cfg(feature = "embed-vm")]
#[test]
fn repl_assign_updates_exports_in_single_module() {
    let (linked, _artifact, _bindings) =
        compile_repl_source("let i = 12; i = i - 1; i", true).unwrap();
    let mut bytecode = Vec::new();
    linked.write_to(&mut bytecode).unwrap();
    let mut vm = crate::vm::ReplVm::new().unwrap();
    let out = vm.exec(&bytecode, 0, None).unwrap();
    assert_eq!(out.trim(), "11", "i after i=i-1 should be 11");
}

/// Member assignment as expression: obj.field = value should output the assigned value.
#[cfg(feature = "embed-vm")]
#[test]
fn repl_member_assign_outputs_value() {
    let (linked, _artifact, _bindings) =
        compile_repl_source("let o = { x: 10 }; o.x = 20", true).unwrap();
    let mut bytecode = Vec::new();
    linked.write_to(&mut bytecode).unwrap();
    let mut vm = crate::vm::ReplVm::new().unwrap();
    let out = vm.exec(&bytecode, 0, None).unwrap();
    assert_eq!(out.trim(), "20", "member assignment should output the assigned value");
}

/// Chunk mode: let o = {...}; o.one — read-only line uses fast chunk path (exec_chunk).
/// The chunk wrapper was fixed to pass init(exports, session) correctly (was passing init(exports, ret)).
#[cfg(feature = "embed-vm")]
#[test]
fn repl_chunk_mode_object_access() {
    let (linked1, artifact, bindings) =
        compile_repl_source("let o = {one: 1, two: 2}", true).unwrap();
    let mut session = ReplSession {
        accumulated: "let o = {one: 1, two: 2}".to_string(),
        bindings,
        prior_artifacts: vec![artifact],
        session_exports: None,
        last_line_had_assign: false,
    };
    let mut vm = crate::vm::ReplVm::new().unwrap();
    let mut exports = 0usize;
    let mut bytecode = Vec::new();
    linked1.write_to(&mut bytecode).unwrap();
    vm.exec(&bytecode, 0, Some(&mut exports)).unwrap();
    session.session_exports = Some(exports);

    // Full linked path: [prior, new] with entry_module_idx=1
    let (linked2, chunk, _, _) = compile_repl_incremental("o.one", &session).unwrap();
    let mut linked_bc = Vec::new();
    linked2.write_to(&mut linked_bc).unwrap();
    let out_full = vm.exec(&linked_bc, 1, None).unwrap();
    assert_eq!(out_full.trim(), "1", "full linked: o.one should return 1");

    // Chunk path (when session_exports valid)
    if exports != 0 {
        let mut chunk_bc = Vec::new();
        chunk.write_to(&mut chunk_bc).unwrap();
        let out_chunk = vm.exec_chunk(&chunk_bc, exports, None).unwrap();
        assert_eq!(out_chunk.trim(), "1", "chunk mode: o.one should return 1");
    }
}

/// Member assignment persists: o.one = 3; o.one should return 3 (not 1).
#[cfg(feature = "embed-vm")]
#[test]
fn repl_member_assign_persists() {
    let (linked1, artifact, bindings) =
        compile_repl_source("let o = {one: 1, two: 2}", true).unwrap();
    let mut session = ReplSession {
        accumulated: "let o = {one: 1, two: 2}".to_string(),
        bindings,
        prior_artifacts: vec![artifact],
        session_exports: None,
        last_line_had_assign: false,
    };
    let mut vm = crate::vm::ReplVm::new().unwrap();
    let mut exports = 0usize;

    // Line 1: let o = {...}
    let mut bc = Vec::new();
    linked1.write_to(&mut bc).unwrap();
    vm.exec(&bc, 0, Some(&mut exports)).unwrap();
    session.session_exports = Some(exports);

    // Line 2: o.one = 3
    let (linked2, _, _, new_artifact) = compile_repl_incremental("o.one = 3", &session).unwrap();
    session.prior_artifacts.push(new_artifact);
    let mut bc2 = Vec::new();
    linked2.write_to(&mut bc2).unwrap();
    vm.exec(&bc2, 1, Some(&mut exports)).unwrap();
    session.session_exports = Some(exports);
    session.accumulated = "let o = {one: 1, two: 2}\no.one = 3".to_string();
    session.last_line_had_assign = true;

    // Line 3: o.one — should return 3
    let (linked3, _, _, _) = compile_repl_incremental("o.one", &session).unwrap();
    let mut bc3 = Vec::new();
    linked3.write_to(&mut bc3).unwrap();
    let out = vm.exec(&bc3, 2, None).unwrap();
    assert_eq!(out.trim(), "3", "o.one after o.one=3 should return 3");
}

/// Regression: let i = 12; i = i + 1; i used to segfault or show stale value (12).
/// Assignments now update the exports object so the next line sees the updated value (13).
#[cfg(feature = "embed-vm")]
#[test]
fn repl_assign_then_read_no_segfault() {
    let manifest_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let jellyc = if manifest_dir.join("target/debug/jellyc").exists() {
        manifest_dir.join("target/debug/jellyc")
    } else if manifest_dir.join("target/release/jellyc").exists() {
        manifest_dir.join("target/release/jellyc")
    } else {
        eprintln!("skipping: jellyc binary not found");
        return;
    };
    let output = std::process::Command::new(&jellyc)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .current_dir(&manifest_dir)
        .spawn()
        .unwrap();
    use std::io::Write;
    let mut child = output;
    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(b"let i = 12;\ni = i + 1;\ni\nexit()\n").unwrap();
        stdin.flush().unwrap();
    }
    let result = child.wait_with_output().unwrap();
    assert!(
        result.status.success(),
        "REPL exited with {:?}, stderr: {}",
        result.status.code(),
        String::from_utf8_lossy(&result.stderr)
    );
    let out = String::from_utf8_lossy(&result.stdout);
    assert!(out.contains("13"), "expected 13 (i after i=i+1) in output, got: {}", out);
}
