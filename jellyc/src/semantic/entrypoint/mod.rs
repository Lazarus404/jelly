// Semantic analysis entrypoints (typecheck + capture analysis).
//
// Note: semantic analysis expects the AST to already be "frontend-prepared":
// templates expanded, truthiness normalization applied, and names resolved.
// See `frontend::prepare_program` / `link::load_module_graph`.

use std::collections::HashMap;

use crate::ast::Program;
use crate::error::CompileError;
use crate::hir::{HirProgram, SemanticInfo};

mod capture;

#[cfg(test)]
mod tests;

pub fn analyze_program(p: &Program) -> Result<(HirProgram, SemanticInfo), CompileError> {
    let mut info = super::typecheck::typecheck_program(p)?;
    info.captures = capture::capture_analysis(p, &info);
    Ok((HirProgram { program: p.clone() }, info))
}

pub fn analyze_prepared_program(
    p: crate::frontend::PreparedProgram<'_>,
) -> Result<(HirProgram, SemanticInfo), CompileError> {
    analyze_program(&p)
}

/// Convenience entrypoint that runs the full frontend pipeline (templates + normalization + resolve)
/// before semantic analysis.
///
/// This is useful for tests and tools that want a single-call "compile frontend" step; the main
/// compiler path still prepares modules during module-graph loading.
#[allow(dead_code)]
pub fn prepare_and_analyze_program(
    p: &mut Program,
) -> Result<(HirProgram, SemanticInfo), CompileError> {
    let _prepared = crate::frontend::prepare_program(p)?;
    analyze_prepared_program(_prepared)
}

pub fn analyze_module_init(
    module_name: &str,
    p: &Program,
    is_entry: bool,
    is_repl: bool,
    import_exports: &HashMap<String, HashMap<String, crate::typectx::TypeRepr>>,
) -> Result<(HirProgram, SemanticInfo), CompileError> {
    // module_name/is_entry are reserved for future semantic differences (entry program constraints).
    let _ = module_name;
    let _ = is_entry;
    let mut info = super::typecheck::typecheck_module_init(p, import_exports, is_repl)?;
    info.captures = capture::capture_analysis(p, &info);
    Ok((HirProgram { program: p.clone() }, info))
}

pub fn analyze_prepared_module_init(
    module_name: &str,
    p: crate::frontend::PreparedProgram<'_>,
    is_entry: bool,
    is_repl: bool,
    import_exports: &HashMap<String, HashMap<String, crate::typectx::TypeRepr>>,
) -> Result<(HirProgram, SemanticInfo), CompileError> {
    analyze_module_init(module_name, &p, is_entry, is_repl, import_exports)
}

/// Module-init variant of `prepare_and_analyze_program`.
#[allow(dead_code)]
pub fn prepare_and_analyze_module_init(
    module_name: &str,
    p: &mut Program,
    is_entry: bool,
    is_repl: bool,
    import_exports: &HashMap<String, HashMap<String, crate::typectx::TypeRepr>>,
) -> Result<(HirProgram, SemanticInfo), CompileError> {
    let _prepared = crate::frontend::prepare_program(p)?;
    analyze_prepared_module_init(module_name, _prepared, is_entry, is_repl, import_exports)
}
