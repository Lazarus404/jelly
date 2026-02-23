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
