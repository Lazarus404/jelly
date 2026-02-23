/**
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

// High-level IR: AST + semantic info after typechecking.
//
// `HirProgram` wraps the AST; `SemanticInfo` holds type tables and capture info.

use std::collections::HashMap;

use crate::ast::{Program, Span};
use crate::ir::TypeId;
use crate::typectx::TypeCtx;

mod render;
mod type_name;

#[derive(Clone, Debug)]
pub struct HirProgram {
    /// For now, HIR is represented as an AST-shaped tree with semantic-normalized nodes
    /// (eg. `ExprKind::Truthy`), plus side tables in `SemanticInfo`.
    pub program: Program,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub Span);

#[derive(Clone, Debug)]
pub struct SemanticInfo {
    pub expr_types: HashMap<NodeId, TypeId>,
    pub binding_types: HashMap<NodeId, TypeId>,
    pub const_inits: HashMap<NodeId, ConstInit>,
    pub captures: HashMap<NodeId, Vec<Capture>>,
    pub type_ctx: TypeCtx,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstInit {
    /// A fully-evaluated compile-time constant value.
    Value(ConstValue),
    /// An alias to an earlier `const` binding (used to preserve identity for pointer-y constants like `Bytes`).
    Alias(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Bytes(Vec<u8>),
    Atom(String),
    Null,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Capture {
    pub name: String,
    pub tid: TypeId,
}

impl Default for SemanticInfo {
    fn default() -> Self {
        Self {
            expr_types: HashMap::new(),
            binding_types: HashMap::new(),
            const_inits: HashMap::new(),
            captures: HashMap::new(),
            type_ctx: TypeCtx::new_program_base(),
        }
    }
}

pub use render::render_hir;
pub use type_name::type_name;
