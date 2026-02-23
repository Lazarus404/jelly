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

use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Span};
use crate::error::{CompileError, ErrorKind};

use super::builtins;
use super::Resolver;

impl Resolver {
    pub(super) fn new() -> Self {
        let builtins = builtins::builtin_namespace_spans();
        Self {
            builtins,
            scopes: vec![HashMap::new()],
            loop_depth: 0,
            fn_depth: 0,
            outer_scopes_stack: Vec::new(),
        }
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop();
        if self.scopes.is_empty() {
            self.scopes.push(HashMap::new());
        }
    }

    pub(super) fn with_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        self.push_scope();
        let out = f(self);
        self.pop_scope();
        out
    }

    pub(super) fn is_defined(&self, name: &str) -> bool {
        self.builtins.contains_key(name) || self.scopes.iter().rev().any(|s| s.contains_key(name))
    }

    /// Define a name in the current scope, without shadowing any existing bindings.
    pub(super) fn define_no_shadow(&mut self, name: &str, span: Span) -> Result<(), CompileError> {
        if self.builtins.contains_key(name) || self.scopes.iter().any(|s| s.contains_key(name)) {
            return Err(CompileError::new(
                ErrorKind::Name,
                span,
                format!("duplicate let binding '{}'", name),
            ));
        }
        self.scopes
            .last_mut()
            .expect("scopes")
            .insert(name.to_string(), span);
        Ok(())
    }

    /// Require a name to be defined in the current scope.
    pub(super) fn require_defined(&self, name: &str, span: Span) -> Result<(), CompileError> {
        // If it's defined in the current environment (including builtins), it's fine.
        if self.is_defined(name) {
            return Ok(());
        }
        // Otherwise, if we're inside a fn literal and the name exists in outer scopes, it's a
        // capture (handled by the lowering/backend).
        if let Some(outer) = self.outer_scopes_stack.last() {
            if outer.iter().rev().any(|s| s.contains_key(name)) {
                return Ok(());
            }
        }

        Err(CompileError::new(
            ErrorKind::Name,
            span,
            format!("unknown variable '{}'", name),
        ))
    }

    pub(super) fn validate_builtin_member(
        &self,
        base: &Expr,
        name: &str,
        span: Span,
    ) -> Result<(), CompileError> {
        // If this is a builtin namespace (and not shadowed by import/let), validate the member name.
        if let ExprKind::Var(ns) = &base.node {
            // If ns is in scopes (eg from `import math as Math`), treat as normal property access.
            let shadowed = self.scopes.iter().rev().any(|s| s.contains_key(ns));
            if !shadowed && !builtins::is_valid_builtin_member(ns, name) {
                return Err(CompileError::new(
                    ErrorKind::Name,
                    span,
                    format!("unknown builtin '{}.{}'", ns, name),
                ));
            }
        }
        Ok(())
    }
}
