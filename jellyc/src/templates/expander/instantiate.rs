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

use std::collections::HashMap;

use crate::ast::{ExprKind, Span, Stmt, StmtKind, Ty};
use crate::error::{CompileError, ErrorKind};

use super::super::names::spec_name;
use super::super::subst::{subst_expr, subst_ty};
use super::Expander;

impl Expander {
    /// Instantiate a template.
    /// This is used to instantiate a template with a given set of type arguments.
    pub(super) fn instantiate(
        &mut self,
        base: &str,
        type_args: &[Ty],
        use_span: Span,
    ) -> Result<String, CompileError> {
        let def = self.templates.get(base).cloned().ok_or_else(|| {
            CompileError::new(
                ErrorKind::Type,
                use_span,
                format!("'{}' is not a template", base),
            )
        })?;
        if type_args.len() != def.type_params.len() {
            return Err(CompileError::new(
                ErrorKind::Type,
                use_span,
                format!(
                    "template '{}' expects {} type args, got {}",
                    base,
                    def.type_params.len(),
                    type_args.len()
                ),
            ));
        }

        let nm = spec_name(base, type_args)?;
        if self.emitted.contains(&nm) {
            return Ok(nm);
        }
        if self.stack.contains(&nm) {
            return Err(CompileError::new(
                ErrorKind::Type,
                use_span,
                format!("template specialization cycle involving '{}'", nm),
            ));
        }

        self.stack.push(nm.clone());

        let mut subst: HashMap<String, Ty> = HashMap::new();
        for (tp, ta) in def.type_params.iter().zip(type_args.iter()) {
            subst.insert(tp.clone(), ta.clone());
        }

        let spec_ty = subst_ty(&def.ty, &subst);
        let spec_expr_raw = subst_expr(&def.expr, &subst);
        let mut spec_expr = self.rewrite_expr(&spec_expr_raw)?;

        // Specializations clone AST nodes from the template definition; ensure spans are
        // distinct across different specializations so semantic side tables (keyed by span)
        // don't collide.
        let spec_idx = self.emitted.len() as i64;
        self.shift_expr_spans(&mut spec_expr, -spec_idx);

        // Runtime template metadata (MVP):
        // If the specialization produces an object literal (commonly prototypes), attach
        // `.__template__` and `.__type_args__` fields on the object.
        if matches!(spec_expr.node, ExprKind::ObjLit(_)) {
            // Use the specialization expression span (not the instantiation site span) to avoid
            // span collisions with unrelated expressions in the use site.
            let meta_span = spec_expr.span;
            spec_expr =
                self.wrap_obj_with_template_meta(spec_expr, base, type_args, &spec_ty, meta_span)?;
        }

        self.out_specs.push(Stmt::new(
            StmtKind::Let {
                is_const: false,
                exported: false,
                name: nm.clone(),
                type_params: Vec::new(),
                ty: Some(spec_ty),
                expr: spec_expr,
            },
            // Use the instantiation site span so different specializations don't collide in
            // semantic side tables keyed by `Span`.
            use_span,
        ));

        self.emitted.insert(nm.clone());
        let _ = self.stack.pop();
        Ok(nm)
    }
}
