/*
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

use crate::ast::ExprKind;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{intern_atom, is_object_kind, lookup_var, lower_expr, LowerCtx, T_OBJECT};

pub(super) fn lower_obj_lit(
    e: &crate::ast::Expr,
    fields: &[(String, crate::ast::Expr)],
    out_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    if out_tid != T_OBJECT && !is_object_kind(&ctx.type_ctx, out_tid) {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "object literal has non-object semantic type",
        ));
    }
    let out = b.new_vreg(out_tid);
    b.emit(e.span, IrOp::ObjNew { dst: out });
    let proto_tid = ctx.proto_for_nominal;
    for (k, ve) in fields {
        let atom_id = intern_atom(k.as_str(), ctx);
        if let Some(nom_tid) = proto_tid {
            // Direct fn literal: recording_method will be consumed by fn_ lowering.
            ctx.recording_method = Some((nom_tid, atom_id));
            // Var ref: if binding has func_index, record now (before lower_expr overwrites env).
            if let ExprKind::Var(var_name) = &ve.node {
                if let Ok(bd) = lookup_var(ctx, var_name, e.span) {
                    if let Some(fi) = bd.func_index {
                        ctx.method_table.insert((nom_tid, atom_id), fi);
                    }
                }
            }
        }
        let (v_val, _t_val) = lower_expr(ve, ctx, b)?;
        ctx.recording_method = None;
        b.emit(
            e.span,
            IrOp::ObjSetAtom {
                obj: out,
                atom_id,
                value: v_val,
            },
        );
    }
    Ok((out, out_tid))
}
