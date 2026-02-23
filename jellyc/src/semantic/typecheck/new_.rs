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

use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

use super::TypeChecker;

pub(super) fn type_new(
    tc: &mut TypeChecker,
    e: &Expr,
    proto: &Expr,
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let t_proto0 = tc.check_expr(proto, None)?;
    let t_proto = if t_proto0 == T_DYNAMIC {
        tc.check_expr(proto, Some(T_OBJECT))?
    } else {
        t_proto0
    };
    if !tc.is_object_kind(t_proto) {
        return Err(CompileError::new(
            ErrorKind::Type,
            proto.span,
            "new expects an Object prototype",
        ));
    }
    for a in args {
        let _ = tc.check_expr(a, None)?;
    }
    // Type of the allocated instance:
    // - default: use the prototype's (possibly nominal) type
    // - allow erasure to plain Object if context expects Object
    // - otherwise, require the expected nominal type to match the prototype's nominal type
    let self_tid = match expect {
        Some(et) if et == T_OBJECT => T_OBJECT,
        Some(et) if tc.is_object_kind(et) => {
            if et != t_proto {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "new: expected object type does not match prototype type",
                ));
            }
            et
        }
        _ => t_proto,
    };
    Ok(self_tid)
}
