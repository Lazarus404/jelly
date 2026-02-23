use crate::error::CompileError;
use crate::ir::{IrFunction, IrModule};

use super::VirtualStream;

#[path = "phase1/const_fun_phi.rs"]
mod const_fun_phi;
#[path = "phase1/emit/mod.rs"]
mod emit;
#[path = "phase1/entry.rs"]
mod entry;
#[path = "phase1/layout.rs"]
mod layout;
#[path = "phase1/liveness.rs"]
mod liveness;
#[path = "phase1/term.rs"]
mod term;

pub(super) fn build_virtual_stream(
    ir: &IrModule,
    f: &IrFunction,
    map_func_index: &impl Fn(u32) -> u32,
) -> Result<VirtualStream, CompileError> {
    entry::build_virtual_stream(ir, f, map_func_index)
}
