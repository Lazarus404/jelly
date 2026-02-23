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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
use crate::ast::{Expr, ExprKind, Span, Ty};
use crate::error::CompileError;
use crate::ir::TypeId;
use crate::typectx::TypeCtx;

mod array;
mod list;
mod misc;
mod object;

pub use array::array_constraints_from_arr_tid;
pub use list::list_constraints_from_list_tid;

#[derive(Clone, Copy, Debug)]
pub enum ArgConstraint {
    Exact(TypeId),
    /// Any nominal object kind (including plain `object`).
    ///
    /// Note: `dynamic` is allowed by semantic analysis (it can be coerced to `object`).
    ObjectKind,
    Numeric,
    Any,
}

#[derive(Clone, Debug)]
pub struct BuiltinConstraints {
    pub args: Vec<ArgConstraint>,
    pub ret: TypeId,
}

fn builtin_name(callee: &Expr) -> Option<(&str, &str)> {
    match &callee.node {
        ExprKind::Var(n) => Some(("", n.as_str())),
        ExprKind::Member { base, name } => match &base.node {
            ExprKind::Var(ns) => Some((ns.as_str(), name.as_str())),
            _ => None,
        },
        _ => None,
    }
}

pub fn builtin_constraints(
    callee: &Expr,
    type_args: &[Ty],
    args_len: usize,
    expect: Option<TypeId>,
    tc: &mut TypeCtx,
    allow_ambiguous_generics: bool,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    let Some((ns, name)) = builtin_name(callee) else {
        return Ok(None);
    };

    if ns == "Object" {
        return object::namespace_constraints(name, type_args, args_len, expect, tc, span);
    }
    if ns == "Array" {
        return array::namespace_constraints(
            name,
            type_args,
            args_len,
            expect,
            tc,
            allow_ambiguous_generics,
            span,
        );
    }
    if ns == "List" {
        return list::namespace_constraints(
            name,
            type_args,
            args_len,
            expect,
            tc,
            allow_ambiguous_generics,
            span,
        );
    }

    misc::namespace_constraints(ns, name, type_args, args_len, expect, span)
}
