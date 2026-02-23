use std::collections::HashMap;

use crate::ast::{MatchArm, Ty};

use super::expr::subst_expr;
use super::stmt::subst_stmt;

/// Substitute a match arm with a substitution.
pub(in crate::templates::subst) fn subst_arm(
    a: &MatchArm,
    subst: &HashMap<String, Ty>,
) -> MatchArm {
    MatchArm {
        pat: a.pat.clone(),
        when: a.when.as_ref().map(|w| subst_expr(w, subst)),
        body: a.body.iter().map(|s| subst_stmt(s, subst)).collect(),
        tail: a.tail.as_ref().map(|t| subst_expr(t, subst)),
    }
}
