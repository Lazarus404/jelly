use crate::ast::Program;
use crate::parse;

mod abi;
mod basic;
mod pressure;

fn frontend_program_from_src(src: &str) -> Program {
    let mut prog = parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    prog
}

fn normalize_and_resolve(prog: &mut Program) -> crate::frontend::PreparedProgram<'_> {
    crate::frontend::prepare_program(prog).unwrap()
}
