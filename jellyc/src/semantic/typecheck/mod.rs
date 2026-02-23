use std::collections::HashMap;

use crate::ast::Program;
use crate::error::CompileError;
use crate::hir::SemanticInfo;
use crate::typectx::TypeRepr;

mod call;
mod checker;
mod control;
mod dispatch;
mod expr_let;
mod fn_;
mod index;
mod lit;
mod match_;
mod member;
mod module_init;
mod new_;
mod numeric;
mod op;
mod stmt;

use crate::typectx::T_BYTES;
use checker::{TypeChecker, TypecheckInputs};
pub(in crate::semantic::typecheck) use numeric::{is_numeric, join_numeric};

pub fn typecheck_program(p: &Program) -> Result<SemanticInfo, CompileError> {
    let mut tc = TypeChecker::new(TypecheckInputs {
        module_alias_exports: HashMap::new(),
        prelude_env: HashMap::new(),
        expected_program_expr_type: T_BYTES,
    });
    tc.check_program(p)?;
    Ok(tc.finish())
}

#[cfg(test)]
mod tests;

pub fn typecheck_module_init(
    p: &Program,
    import_exports: &HashMap<String, HashMap<String, TypeRepr>>,
    is_repl: bool,
) -> Result<SemanticInfo, CompileError> {
    module_init::typecheck_module_init(p, import_exports, is_repl)
}
