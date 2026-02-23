use crate::ast::Span;
use crate::jlyb::{FunSig, TypeEntry};

use super::ids::{BlockId, TypeId, VRegId};
use super::op::IrOp;
use super::terminator::IrTerminator;

#[derive(Clone, Debug)]
pub struct IrModule {
    pub types: Vec<TypeEntry>,
    pub sigs: Vec<FunSig>,
    pub const_i64: Vec<i64>,
    pub const_f64: Vec<f64>,
    pub const_bytes: Vec<Vec<u8>>,
    pub atoms: Vec<Vec<u8>>,
    pub funcs: Vec<IrFunction>,
    pub entry: usize,
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub name: Option<String>,
    pub param_count: u8,
    /// Callee capture slots (values provided by a closure object at call-time).
    ///
    /// These vregs correspond 1:1 with the closure capture order. In bytecode, captures are
    /// copied into the callee register file starting at `cap_start` when the module enables
    /// `JELLY_BC_FEAT_CAP_START` (otherwise the VM falls back to trailing `nregs-ncaps`).
    pub cap_vregs: Vec<VRegId>,
    pub entry: BlockId,
    pub blocks: Vec<IrBlock>,
    pub vreg_types: Vec<TypeId>,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
    pub label: Option<String>,
    pub insns: Vec<IrInsn>,
    pub term: IrTerminator,
}

#[derive(Clone, Debug)]
pub struct IrInsn {
    pub span: Span,
    pub op: IrOp,
}
