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
