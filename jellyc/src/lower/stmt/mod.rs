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
use std::collections::HashSet;
use std::convert::Infallible;

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::visit::Visitor;

mod dispatch;

/// Collect variable names assigned in the given statements (Assign and Let).
fn vars_assigned_in(stmts: &[Stmt]) -> HashSet<String> {
    let mut out = HashSet::new();
    for s in stmts {
        match &s.node {
            StmtKind::Assign { name, .. } => {
                out.insert(name.clone());
            }
            StmtKind::Let { name, .. } => {
                out.insert(name.clone());
            }
            _ => {}
        }
    }
    out
}

/// Collect variable names used (read) in the given expression.
fn vars_used_in_expr(e: &Expr) -> HashSet<String> {
    #[derive(Default)]
    struct Collector {
        out: HashSet<String>,
    }

    impl crate::visit::Visitor for Collector {
        type Err = Infallible;

        fn visit_expr(&mut self, e: &Expr) -> Result<(), Self::Err> {
            match &e.node {
                ExprKind::Var(n) => {
                    self.out.insert(n.clone());
                    Ok(())
                }
                // `TypeApp` is compile-time only; historically we did not treat it as a runtime "use".
                ExprKind::TypeApp { .. } => Ok(()),
                _ => crate::visit::walk_expr(self, e),
            }
        }
    }

    let mut c = Collector::default();
    c.visit_expr(e).unwrap();
    c.out
}

pub(crate) use dispatch::{lower_assign_expr, lower_member_assign_expr};
pub use dispatch::let_::lower_let_expr;
pub use dispatch::lower_stmt;
