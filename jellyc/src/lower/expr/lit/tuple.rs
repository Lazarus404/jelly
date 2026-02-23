use crate::error::CompileError;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{intern_atom, lower_expr, LowerCtx};

pub(super) fn lower_tuple_lit(
    e: &crate::ast::Expr,
    elems: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let mut vs: Vec<(VRegId, TypeId)> = Vec::with_capacity(elems.len());
    let mut elem_tids: Vec<TypeId> = Vec::with_capacity(elems.len());
    for el in elems {
        let (v, t) = lower_expr(el, ctx, b)?;
        vs.push((v, t));
        elem_tids.push(t);
    }

    let tup_tid = ctx.type_ctx.intern_tuple_type(&elem_tids);
    let out = b.new_vreg(tup_tid);
    b.emit(e.span, IrOp::ObjNew { dst: out });

    for (i, (v, _t)) in vs.iter().enumerate() {
        let key = i.to_string();
        let atom_id = intern_atom(&key, ctx);
        b.emit(
            e.span,
            IrOp::ObjSetAtom {
                obj: out,
                atom_id,
                value: *v,
            },
        );
    }
    Ok((out, tup_tid))
}
