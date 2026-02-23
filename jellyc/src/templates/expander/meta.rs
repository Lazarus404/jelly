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

use crate::ast::{Expr, ExprKind, Span, Stmt, StmtKind, Ty};
use crate::error::CompileError;

use super::super::names::type_key;
use super::Expander;

impl Expander {
    pub(super) fn wrap_obj_with_template_meta(
        &mut self,
        obj_expr: Expr,   // Expression for the object literal
        tmpl_name: &str,  // Name of the template
        type_args: &[Ty], // Type arguments
        self_ty: &Ty,     // Self type
        span: Span,       // Span for the statement
    ) -> Result<Expr, CompileError> {
        // Template expansion introduces synthetic AST nodes. Our semantic tables are keyed by `Span`,
        // so we must avoid re-using the same span for multiple different nodes.
        let sp = |off: usize| Span::point(span.start.saturating_add(off));

        let tmp = format!("__tmpl{}", self.tmp_counter);
        self.tmp_counter += 1;

        let mut arg_s = String::new();
        for (i, ta) in type_args.iter().enumerate() {
            if i != 0 {
                arg_s.push(',');
            }
            arg_s.push_str(&type_key(ta)?);
        }

        let s_tmp = Stmt::new(
            StmtKind::Let {
                is_const: false,
                exported: false,
                name: tmp.clone(),
                type_params: Vec::new(),
                ty: Some(self_ty.clone()),
                expr: obj_expr,
            },
            sp(0),
        );
        let set_name = Stmt::new(
            StmtKind::MemberAssign {
                base: Expr::new(ExprKind::Var(tmp.clone()), sp(1)),
                name: "__template__".to_string(),
                expr: Expr::new(ExprKind::BytesLit(tmpl_name.as_bytes().to_vec()), sp(4)),
            },
            sp(1),
        );
        let set_args = Stmt::new(
            StmtKind::MemberAssign {
                base: Expr::new(ExprKind::Var(tmp.clone()), sp(2)),
                name: "__type_args__".to_string(),
                expr: Expr::new(ExprKind::BytesLit(arg_s.into_bytes()), sp(5)),
            },
            sp(2),
        );
        Ok(Expr::new(
            ExprKind::Block {
                stmts: vec![s_tmp, set_name, set_args],
                expr: Box::new(Expr::new(ExprKind::Var(tmp), sp(3))),
            },
            span,
        ))
    }
}
