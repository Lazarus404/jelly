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

use std::collections::HashMap;

use crate::ast::Program;
use crate::error::CompileError;
use crate::hir::SemanticInfo;
use crate::typectx::TypeRepr;

mod call;
mod checker;
mod control;
mod dispatch;
mod expr_let;
mod fn_;
mod index;
mod lit;
mod match_;
mod member;
mod module_init;
mod new_;
mod numeric;
mod op;
mod stmt;

use crate::typectx::T_BYTES;
use checker::{TypeChecker, TypecheckInputs};
pub(in crate::semantic::typecheck) use numeric::{is_integer, is_numeric, join_numeric};

pub fn typecheck_program(p: &Program) -> Result<SemanticInfo, CompileError> {
    let mut tc = TypeChecker::new(TypecheckInputs {
        module_alias_exports: HashMap::new(),
        prelude_env: HashMap::new(),
        expected_program_expr_type: T_BYTES,
    });
    tc.check_program(p)?;
    Ok(tc.finish())
}

#[cfg(test)]
mod tests;

pub fn typecheck_module_init(
    p: &Program,
    import_exports: &HashMap<String, HashMap<String, TypeRepr>>,
    is_repl: bool,
) -> Result<SemanticInfo, CompileError> {
    module_init::typecheck_module_init(p, import_exports, is_repl)
}
