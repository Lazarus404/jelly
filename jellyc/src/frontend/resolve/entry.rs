use crate::ast::Program;
use crate::error::CompileError;
use crate::visit::Visitor;

use super::Resolver;

pub(crate) fn resolve_program(p: &Program) -> Result<(), CompileError> {
    let mut r = Resolver::new();
    r.visit_program(p)
}
