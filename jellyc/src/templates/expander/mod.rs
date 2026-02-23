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

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Span, Stmt, Ty};

pub(super) mod infer;
pub(super) mod instantiate;
pub(super) mod meta;
pub(super) mod rewrite;
pub(super) mod spans;

#[derive(Clone)]
pub(super) struct TemplateDef {
    #[allow(dead_code)]
    pub(super) span: Span,
    #[allow(dead_code)]
    pub(super) name: String,
    pub(super) type_params: Vec<String>,
    pub(super) ty: Ty,
    pub(super) expr: Expr,
}

/// Expander for templates.
/// This is used to expand templates into concrete specialized types and expressions.
pub(super) struct Expander {
    pub(super) templates: HashMap<String, TemplateDef>,
    pub(super) emitted: HashSet<String>, // spec name
    pub(super) stack: Vec<String>,       // spec name
    pub(super) out_specs: Vec<Stmt>,
    pub(super) known_vars: HashMap<String, Ty>, // top-level typed lets seen so far (MVP inference aid)
    pub(super) tmp_counter: u32,
}
