mod alloc;
mod call_windows;
mod type_class;

use crate::error::CompileError;
use crate::ir::{IrFunction, IrModule};

use super::{Allocation, VirtualStream};

pub(super) fn allocate_registers(
    ir: &IrModule,
    f: &IrFunction,
    vs: &VirtualStream,
) -> Result<Allocation, CompileError> {
    alloc::allocate_registers(ir, f, vs)
}
