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

use crate::ast::{Expr, ExprKind, Ty, TyKind};

use super::Expander;

impl Expander {
    /// Infer the type from an expression.
    /// This is used to infer the type of a literal expression.
    pub(super) fn infer_ty_from_expr(&self, e: &Expr) -> Option<Ty> {
        match &e.node {
            ExprKind::I8Lit(_) => Some(Ty::new(TyKind::Named("I8".to_string()), e.span)),
            ExprKind::I16Lit(_) => Some(Ty::new(TyKind::Named("I16".to_string()), e.span)),
            ExprKind::I32Lit(_) => Some(Ty::new(TyKind::Named("I32".to_string()), e.span)),
            ExprKind::I64Lit(_) => Some(Ty::new(TyKind::Named("I64".to_string()), e.span)),
            ExprKind::F16Lit(_) => Some(Ty::new(TyKind::Named("F16".to_string()), e.span)),
            ExprKind::F64Lit(_) => Some(Ty::new(TyKind::Named("F32".to_string()), e.span)),
            ExprKind::BoolLit(_) => Some(Ty::new(TyKind::Named("Bool".to_string()), e.span)),
            ExprKind::BytesLit(_) => Some(Ty::new(TyKind::Named("Bytes".to_string()), e.span)),
            ExprKind::Var(n) => self.known_vars.get(n).cloned(),
            // MVP: no inference for `null` or non-trivial expressions.
            _ => None,
        }
    }
}
