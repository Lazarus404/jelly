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

use crate::ast::{Expr, ExprKind, Ty};
use crate::error::{CompileError, ErrorKind};

use super::TypeChecker;

pub(super) fn type_let_expr(
    tc: &mut TypeChecker,
    e: &Expr,
    is_const: bool,
    name: &str,
    type_params: &[String],
    ty: Option<&Ty>,
    init: &Expr,
    expect: Option<crate::ir::TypeId>,
) -> Result<crate::ir::TypeId, CompileError> {
    if !type_params.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "template lets must be expanded before semantic analysis",
        ));
    }
    let is_discard = name == "_";
    if is_discard {
        let tid = tc.check_expr(init, expect)?;
        if is_const {
            let _ = tc.eval_const_expr(init)?;
        }
        return Ok(tid);
    }

    let bind_tid = if let Some(ann) = ty {
        tc.resolve_ann_tid(ann)?
    } else if let ExprKind::Fn { params, body, tail } = &init.node {
        tc.infer_fn_tid_for_let(name, params, body, tail)?
    } else {
        tc.check_expr(init, None)?
    };

    if matches!(&init.node, ExprKind::Fn { .. }) {
        tc.bind_local(name, bind_tid);
    }

    let init_tid = tc.check_expr(init, Some(bind_tid))?;
    if init_tid != bind_tid {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "let initializer type mismatch",
        ));
    }
    if is_const {
        let init_val = tc.eval_const_expr(init)?;
        tc.record_const_init(e.span, init_val.clone());
        tc.bind_const(name, init_val);
    }
    tc.record_binding(e.span, bind_tid);
    if !matches!(&init.node, ExprKind::Fn { .. }) {
        tc.bind_local(name, bind_tid);
    }
    Ok(bind_tid)
}
