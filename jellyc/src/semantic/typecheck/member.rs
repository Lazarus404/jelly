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

use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

use super::TypeChecker;

pub(super) fn type_member(
    tc: &mut TypeChecker,
    e: &Expr,
    base: &Expr,
    name: &str,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    // Builtin namespaces must be called, not extracted as values.
    // But allow shadowing (e.g. `import foo as Bytes` or `let Bytes = {...}`).
    if let ExprKind::Var(ns) = &base.node {
        let shadowed = tc.lookup(ns).is_some();
        if !shadowed
            && matches!(
                ns.as_str(),
                "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
            )
        {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "namespace members must be called (e.g. Bytes.len(x))",
            ));
        }
    }

    // Module namespace object: use declared export type if available.
    if let ExprKind::Var(alias) = &base.node {
        if let Some(exp_tid) = tc.module_export_tid(alias, name) {
            // Ensure the module alias itself is typed/recorded (lowering requires it).
            let _ = tc.check_expr(base, Some(T_OBJECT))?;
            let out_tid = Some(exp_tid).or(expect).unwrap_or(T_DYNAMIC);
            if let Some(et) = expect {
                return tc.coerce_type(out_tid, et, e.span);
            }
            return Ok(out_tid);
        }
    }

    // Regular object / nominal object kinds.
    // Preserve nominal object types, but allow `dynamic` to be treated as `object` here.
    let t_obj0 = tc.check_expr(base, None)?;
    let t_obj = if t_obj0 == T_DYNAMIC {
        tc.check_expr(base, Some(T_OBJECT))?
    } else {
        t_obj0
    };
    if !tc.is_object_kind(t_obj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "member access currently only supported for Object (obj.field)",
        ));
    }

    // Tuple element access: `t.0`.
    if tc.is_tuple_type(t_obj) {
        let idx: usize = name.parse().map_err(|_| {
            CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple element access must be .<index>",
            )
        })?;
        let elems = tc.tuple_elems(t_obj, e.span)?;
        if idx >= elems.len() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple index out of range",
            ));
        }
        let elem_tid = elems[idx];
        if let Some(et) = expect {
            return tc.coerce_type(elem_tid, et, e.span);
        }
        return Ok(elem_tid);
    }

    // Object property access yields Dynamic unless context requires typed unboxing.
    if let Some(et) = expect {
        if et != T_DYNAMIC {
            return Ok(et);
        }
    }
    Ok(T_DYNAMIC)
}
