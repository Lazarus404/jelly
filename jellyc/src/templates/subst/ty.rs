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

use std::collections::HashMap;

use crate::ast::{Ty, TyKind};

/// Substitute a type with a substitution.
pub(in crate::templates) fn subst_ty(t: &Ty, subst: &HashMap<String, Ty>) -> Ty {
    match &t.node {
        TyKind::Named(n) => subst
            .get(n)
            .cloned()
            .unwrap_or_else(|| Ty::new(TyKind::Named(n.clone()), t.span)),
        TyKind::Generic { base, args } => Ty::new(
            TyKind::Generic {
                base: base.clone(),
                args: args.iter().map(|a| subst_ty(a, subst)).collect(),
            },
            t.span,
        ),
        TyKind::Fun { args, ret } => Ty::new(
            TyKind::Fun {
                args: args.iter().map(|a| subst_ty(a, subst)).collect(),
                ret: Box::new(subst_ty(ret, subst)),
            },
            t.span,
        ),
        TyKind::Tuple(elems) => Ty::new(
            TyKind::Tuple(elems.iter().map(|a| subst_ty(a, subst)).collect()),
            t.span,
        ),
    }
}
