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

// Frontend pipeline: templates → normalization → name resolution.
//
// `prepare_program` prepares an AST for semantic analysis. `PreparedProgram` is the proof type.

use crate::ast::Program;
use crate::error::CompileError;

use std::ops::Deref;

mod normalize_truthiness;
mod resolve;

pub(crate) use normalize_truthiness::normalize_truthiness_program;
pub(crate) use resolve::resolve_program;

#[must_use]
#[derive(Debug, Clone, Copy)]
pub struct PreparedProgram<'a>(&'a Program);

impl Deref for PreparedProgram<'_> {
    type Target = Program;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

pub(crate) fn expand_and_normalize(p: &mut Program) -> Result<(), CompileError> {
    crate::templates::expand_templates(p)?;
    normalize_truthiness_program(p);
    Ok(())
}

pub fn prepare_program(p: &mut Program) -> Result<PreparedProgram<'_>, CompileError> {
    expand_and_normalize(p)?;
    resolve_program(p)?;
    Ok(PreparedProgram(p))
}

/// Wrap an AST that is already known to be prepared (templates expanded, truthiness normalized,
/// and resolved). This is mainly for module-graph codepaths that store the prepared `Program`
/// and want to pass the proof downstream.
pub(crate) fn prepared(p: &Program) -> PreparedProgram<'_> {
    PreparedProgram(p)
}
