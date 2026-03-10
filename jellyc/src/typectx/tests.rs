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
use super::*;
use crate::ast::{Span, Spanned};
use crate::ast::{Ty, TyKind};

fn ty_named(name: &str) -> Ty {
    Spanned::new(TyKind::Named(name.to_string()), Span::point(0))
}

#[test]
fn resolve_ty_with_subst_named_typevar() {
    let mut tc = TypeCtx::new_program_base();
    let subst = HashMap::from([("T".to_string(), T_I32)]);
    assert_eq!(
        tc.resolve_ty_with_subst(&ty_named("T"), &subst).unwrap(),
        T_I32
    );
}

#[test]
fn resolve_ty_with_subst_generic_array() {
    let mut tc = TypeCtx::new_program_base();
    let t = Spanned::new(
        TyKind::Generic {
            base: "Array".to_string(),
            args: vec![ty_named("T")],
        },
        Span::point(0),
    );
    let subst = HashMap::from([("T".to_string(), T_I32)]);
    assert_eq!(tc.resolve_ty_with_subst(&t, &subst).unwrap(), T_ARRAY_I32);
}

#[test]
fn resolve_ty_with_subst_unknown_still_errors() {
    let mut tc = TypeCtx::new_program_base();
    let subst: HashMap<String, u32> = HashMap::new();
    let err = tc
        .resolve_ty_with_subst(&ty_named("T"), &subst)
        .unwrap_err();
    assert!(err.message.contains("unknown type"));
}
