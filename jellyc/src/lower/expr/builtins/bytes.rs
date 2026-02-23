use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};
use crate::typectx::{T_BYTES, T_I32};

use super::super::{lower_expr_expect, LowerCtx, T_BOOL};

pub(super) fn try_lower_bytes_builtin(
    e: &Expr,
    ns: &str,
    name: &str,
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    if ns != "Bytes" {
        return Ok(None);
    }

    if name == "new" {
        let (vlen, tlen) = lower_expr_expect(&args[0], ctx, b)?;
        if tlen != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Bytes.new",
            ));
        }
        let out = b.new_vreg(T_BYTES);
        b.emit(
            e.span,
            IrOp::BytesNew {
                dst: out,
                len: vlen,
            },
        );
        return Ok(Some((out, T_BYTES)));
    }
    if name == "len" {
        let (vb, tb) = lower_expr_expect(&args[0], ctx, b)?;
        if tb != T_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Bytes.len",
            ));
        }
        let out = b.new_vreg(T_I32);
        b.emit(
            e.span,
            IrOp::BytesLen {
                dst: out,
                bytes: vb,
            },
        );
        return Ok(Some((out, T_I32)));
    }
    if name == "get_u8" {
        let (vb, tb) = lower_expr_expect(&args[0], ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], ctx, b)?;
        if tb != T_BYTES || ti != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Bytes.get_u8",
            ));
        }
        let out = b.new_vreg(T_I32);
        b.emit(
            e.span,
            IrOp::BytesGetU8 {
                dst: out,
                bytes: vb,
                index: vi,
            },
        );
        return Ok(Some((out, T_I32)));
    }
    if name == "set_u8" {
        let (vb, tb) = lower_expr_expect(&args[0], ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], ctx, b)?;
        let (vv, tv) = lower_expr_expect(&args[2], ctx, b)?;
        if tb != T_BYTES || ti != T_I32 || tv != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Bytes.set_u8",
            ));
        }
        b.emit(
            e.span,
            IrOp::BytesSetU8 {
                bytes: vb,
                index: vi,
                value: vv,
            },
        );
        return Ok(Some((vb, T_BYTES)));
    }
    if name == "slice" {
        let (vb, tb) = lower_expr_expect(&args[0], ctx, b)?;
        let (v_start, t_start) = lower_expr_expect(&args[1], ctx, b)?;
        let (v_len, t_len) = lower_expr_expect(&args[2], ctx, b)?;
        if tb != T_BYTES || t_start != T_I32 || t_len != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Bytes.slice",
            ));
        }

        let sig_args = [T_BYTES, T_I32, T_I32];
        let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
        let fun_tid = ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args);

        let vcallee = b.new_vreg(fun_tid);
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::PRELUDE_BYTES_SLICE,
            },
        );

        // Marshal args into a contiguous vreg window.
        let arg0 = b.new_vreg(T_BYTES);
        b.emit(args[0].span, IrOp::Mov { dst: arg0, src: vb });
        let arg1 = b.new_vreg(T_I32);
        b.emit(
            args[1].span,
            IrOp::Mov {
                dst: arg1,
                src: v_start,
            },
        );
        let arg2 = b.new_vreg(T_I32);
        b.emit(
            args[2].span,
            IrOp::Mov {
                dst: arg2,
                src: v_len,
            },
        );

        let out = b.new_vreg(T_BYTES);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base: arg0,
                nargs: 3,
            },
        );
        return Ok(Some((out, T_BYTES)));
    }
    if name == "eq" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        let (vb, tb) = lower_expr_expect(&args[1], ctx, b)?;
        if ta != T_BYTES || tb != T_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Bytes.eq",
            ));
        }

        let sig_args = [T_BYTES, T_BYTES];
        let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);
        let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args);

        let vcallee = b.new_vreg(fun_tid);
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::PRELUDE_BYTES_EQ,
            },
        );

        // Marshal args into a contiguous vreg window.
        let arg0 = b.new_vreg(T_BYTES);
        b.emit(args[0].span, IrOp::Mov { dst: arg0, src: va });
        let arg1 = b.new_vreg(T_BYTES);
        b.emit(args[1].span, IrOp::Mov { dst: arg1, src: vb });

        let out = b.new_vreg(T_BOOL);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base: arg0,
                nargs: 2,
            },
        );
        return Ok(Some((out, T_BOOL)));
    }

    Ok(None)
}
