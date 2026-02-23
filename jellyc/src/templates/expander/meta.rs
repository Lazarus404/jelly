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
