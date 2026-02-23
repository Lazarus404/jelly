use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{lower_expr, LowerCtx};
use super::super::{T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_I32};

pub(super) fn lower_array_lit(
    e: &crate::ast::Expr,
    elems: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    if elems.is_empty() {
        let et = ctx
            .sem_expr_types
            .get(&NodeId(e.span))
            .copied()
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    e.span,
                    "missing semantic type for array literal",
                )
            })?;
        if et != T_ARRAY_I32 && et != T_ARRAY_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "array literal has non-array semantic type",
            ));
        }
        let v0 = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ConstI32 { dst: v0, imm: 0 });
        let out = b.new_vreg(et);
        b.emit(e.span, IrOp::ArrayNew { dst: out, len: v0 });
        return Ok((out, et));
    }

    let mut vs: Vec<(VRegId, TypeId)> = Vec::with_capacity(elems.len());
    for el in elems {
        vs.push(lower_expr(el, ctx, b)?);
    }
    let t0 = vs[0].1;
    for &(_, tt) in &vs[1..] {
        if tt != t0 {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "array literal elements must have same type",
            ));
        }
    }
    let arr_tid = match t0 {
        T_I32 => T_ARRAY_I32,
        T_BYTES => T_ARRAY_BYTES,
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "array literal only supports i32/bytes elements for now",
            ))
        }
    };

    let v_n = b.new_vreg(T_I32);
    b.emit(
        e.span,
        IrOp::ConstI32 {
            dst: v_n,
            imm: vs.len() as i32,
        },
    );
    let v_arr = b.new_vreg(arr_tid);
    b.emit(
        e.span,
        IrOp::ArrayNew {
            dst: v_arr,
            len: v_n,
        },
    );
    for (i, (vp, _)) in vs.iter().enumerate() {
        let v_i = b.new_vreg(T_I32);
        b.emit(
            e.span,
            IrOp::ConstI32 {
                dst: v_i,
                imm: i as i32,
            },
        );
        b.emit(
            e.span,
            IrOp::ArraySet {
                arr: v_arr,
                index: v_i,
                value: *vp,
            },
        );
    }
    Ok((v_arr, arr_tid))
}
