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
