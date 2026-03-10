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

use std::convert::Infallible;

use crate::ast::{Expr, ExprKind, Program};
use crate::visit::VisitorMut;

pub(crate) fn normalize_truthiness_program(p: &mut Program) {
    struct Normalizer;

    impl VisitorMut for Normalizer {
        type Err = Infallible;

        fn visit_expr(&mut self, e: &mut Expr) -> Result<(), Self::Err> {
            crate::visit::walk_expr_mut(self, e)?;

            match &e.node {
                ExprKind::Eq(_, _) => normalize_truthy_cmp_in_place(e, true),
                ExprKind::Ne(_, _) => normalize_truthy_cmp_in_place(e, false),
                _ => {}
            }
            Ok(())
        }
    }

    fn normalize_truthy_cmp_in_place(e: &mut Expr, is_eq: bool) {
        let span = e.span;
        let (a, b) = match std::mem::replace(&mut e.node, ExprKind::Null) {
            ExprKind::Eq(a, b) if is_eq => (a, b),
            ExprKind::Ne(a, b) if !is_eq => (a, b),
            other => {
                e.node = other;
                return;
            }
        };

        let mk = |bv: bool, other: Expr| -> ExprKind {
            let truthy = Expr::new(ExprKind::Truthy(Box::new(other)), span);
            if (is_eq && bv) || (!is_eq && !bv) {
                truthy.node
            } else {
                ExprKind::Not(Box::new(truthy))
            }
        };

        if let ExprKind::BoolLit(bv) = a.node {
            e.node = mk(bv, *b);
            return;
        }
        if let ExprKind::BoolLit(bv) = b.node {
            e.node = mk(bv, *a);
            return;
        }

        e.node = if is_eq {
            ExprKind::Eq(a, b)
        } else {
            ExprKind::Ne(a, b)
        };
    }

    let mut n = Normalizer;
    n.visit_program(p).unwrap();
}
