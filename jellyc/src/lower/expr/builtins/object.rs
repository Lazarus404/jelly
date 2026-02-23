use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use crate::lower::is_object_kind;

use super::super::{lower_expr_expect, LowerCtx, T_ATOM, T_OBJECT};

pub(super) fn try_lower_object_builtin(
    e: &Expr,
    ns: &str,
    name: &str,
    out_tid: TypeId,
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    if ns != "Object" {
        return Ok(None);
    }

    if name == "get" {
        let (v_obj, t_obj) = lower_expr_expect(&args[0], ctx, b)?;
        if !is_object_kind(&ctx.type_ctx, t_obj) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Object.get receiver",
            ));
        }
        let (v_key, t_key) = lower_expr_expect(&args[1], ctx, b)?;
        if t_key != T_ATOM {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[1].span,
                "semantic type mismatch for Object.get key",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(
            e.span,
            IrOp::ObjGet {
                dst: out,
                obj: v_obj,
                atom: v_key,
            },
        );
        return Ok(Some((out, out_tid)));
    }

    if name == "set" {
        let (v_obj, t_obj) = lower_expr_expect(&args[0], ctx, b)?;
        if !is_object_kind(&ctx.type_ctx, t_obj) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Object.set receiver",
            ));
        }
        let (v_key, t_key) = lower_expr_expect(&args[1], ctx, b)?;
        if t_key != T_ATOM {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[1].span,
                "semantic type mismatch for Object.set key",
            ));
        }
        let (v_val, _t_val) = lower_expr_expect(&args[2], ctx, b)?;
        b.emit(
            e.span,
            IrOp::ObjSet {
                obj: v_obj,
                atom: v_key,
                value: v_val,
            },
        );
        if out_tid != t_obj && out_tid != T_OBJECT {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Object.set",
            ));
        }
        return Ok(Some((v_obj, out_tid)));
    }

    Ok(None)
}
