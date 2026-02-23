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
