use crate::jlyb::{Module, TypeKind};

use super::helpers;

pub(super) struct ValidateCtx<'a> {
    pub(super) m: &'a Module,
    pub(super) func_i: usize,
    pub(super) pc: usize,
    pub(super) reg_types: &'a [u32],
    pub(super) nregs: u32,
    pub(super) nfuncs_logical_max: u32,
}

impl<'a> ValidateCtx<'a> {
    pub(super) fn err(&self, msg: impl Into<String>) -> String {
        helpers::err(self.func_i, self.pc, msg)
    }

    pub(super) fn type_kind(&self, tid: u32) -> Result<TypeKind, String> {
        helpers::type_kind(self.m, tid)
    }

    pub(super) fn rk(&self, r: u8) -> Result<TypeKind, String> {
        helpers::rk(self.m, self.reg_types, r)
    }

    pub(super) fn slot_size_bytes(&self, k: TypeKind) -> usize {
        helpers::slot_size_bytes(k)
    }

    pub(super) fn is_i32ish(&self, k: TypeKind) -> bool {
        helpers::is_i32ish(k)
    }

    pub(super) fn is_ptr_kind(&self, k: TypeKind) -> bool {
        helpers::is_ptr_kind(k)
    }

    pub(super) fn unary_elem_tid(
        &self,
        container_tid: u32,
        want: TypeKind,
        why: &'static str,
    ) -> Result<u32, String> {
        helpers::unary_elem_tid(self.m, container_tid, want, why)
    }
}
