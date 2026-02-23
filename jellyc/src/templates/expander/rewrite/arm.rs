use crate::ast::MatchArm;
use crate::error::CompileError;

use super::super::Expander;

impl Expander {
    /// Rewrite a match arm.
    pub(in crate::templates) fn rewrite_arm(
        &mut self,
        a: &MatchArm,
    ) -> Result<MatchArm, CompileError> {
        Ok(MatchArm {
            pat: a.pat.clone(),
            when: match &a.when {
                Some(w) => Some(self.rewrite_expr(w)?),
                None => None,
            },
            body: a
                .body
                .iter()
                .map(|s| self.rewrite_stmt(s))
                .collect::<Result<_, _>>()?,
            tail: match &a.tail {
                Some(t) => Some(self.rewrite_expr(t)?),
                None => None,
            },
        })
    }
}
