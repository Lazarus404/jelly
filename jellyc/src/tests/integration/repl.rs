//! REPL integration tests: incremental compilation and session bindings.

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
        prior_artifact: Some(artifact),
    };

    let (linked2, merged_bindings) = compile_repl_incremental("i", &session).unwrap();
    assert!(merged_bindings.contains_key("i"));

    let mut bytecode = Vec::new();
    linked2.write_to(&mut bytecode).unwrap();
    assert!(!bytecode.is_empty());

    #[cfg(feature = "embed-vm")]
    {
        let mut vm = crate::vm::ReplVm::new().unwrap();
        let out = vm.exec(&bytecode).unwrap();
        assert_eq!(out.trim(), "2");
    }
}

#[test]
fn compile_repl_incremental_adds_new_bindings() {
    let (_, artifact, bindings) = compile_repl_source("let i = 1", true).unwrap();

    let session = ReplSession {
        accumulated: "let i = 1".to_string(),
        bindings,
        prior_artifact: Some(artifact),
    };

    let (_, merged) = compile_repl_incremental("let j = i + 1", &session).unwrap();
    assert!(merged.contains_key("i"));
    assert!(merged.contains_key("j"));
}
