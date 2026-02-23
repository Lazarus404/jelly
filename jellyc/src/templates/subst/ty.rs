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
