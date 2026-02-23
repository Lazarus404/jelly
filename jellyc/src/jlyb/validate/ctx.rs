/*
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

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
