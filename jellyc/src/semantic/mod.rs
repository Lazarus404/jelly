//! Semantic analysis: typechecking and capture analysis.
//!
//! Produces `(HirProgram, SemanticInfo)` from a prepared AST. Entry points:
//! `analyze_prepared_module_init` (module-graph), `analyze_prepared_program` (single-file).

mod const_eval;
mod entrypoint;
mod fn_infer;
mod typecheck;

#[allow(unused_imports)]
pub use entrypoint::{
    analyze_module_init, analyze_prepared_module_init, analyze_prepared_program, analyze_program,
    prepare_and_analyze_module_init, prepare_and_analyze_program,
};
