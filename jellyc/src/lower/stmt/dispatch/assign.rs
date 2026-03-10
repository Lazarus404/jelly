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

use crate::ast::{Expr, Stmt};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_BYTES, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8,
};

use crate::lower::expr;
use crate::lower::expr::{coerce_numeric, is_narrowing_numeric, lower_expr, lower_expr_expect};
use crate::lower::{ensure_open_block, intern_atom, LowerCtx, T_DYNAMIC};

/// Lower assignment as expression: returns the assigned value (same as RHS after Mov).
pub(crate) fn lower_assign_expr(
    e: &Expr,
    name: &str,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    ensure_open_block(b);
    let dst = *ctx
        .env_stack
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Name,
                e.span,
                format!("unknown variable '{}'", name),
            )
        })?;
    let (rhs, rt) = lower_expr(expr0, ctx, b)?;
    let rhs = if rt != dst.tid {
        if dst.tid == T_DYNAMIC {
            let out = b.new_vreg(T_DYNAMIC);
            b.emit(e.span, IrOp::ToDyn { dst: out, src: rhs });
            out
        } else {
            let coerced = coerce_numeric(e.span, rhs, rt, dst.tid, b).map_err(|_| {
                CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    format!("assignment to '{}' changes type", name),
                )
            })?;
            if is_narrowing_numeric(rt, dst.tid) {
                ctx.warnings.push(crate::error::CompileWarning::new(
                    e.span,
                    format!(
                        "implicit narrowing conversion in assignment from {} to {}",
                        expr::type_name(rt),
                        expr::type_name(dst.tid)
                    ),
                ));
            }
            coerced
        }
    } else {
        rhs
    };
    b.emit(
        e.span,
        IrOp::Mov {
            dst: dst.v,
            src: rhs,
        },
    );
    if ctx.exported_bindings.contains(name) {
        if let Some(exports_obj) = ctx.exports_obj {
            let atom_id = intern_atom(name, ctx);
            b.emit(
                e.span,
                IrOp::ObjSetAtom {
                    obj: exports_obj,
                    atom_id,
                    value: dst.v,
                },
            );
        }
    }
    if ctx.session_imported_names.contains(name) {
        if let Some(session_obj) = ctx.session_obj {
            let atom_id = intern_atom(name, ctx);
            b.emit(
                e.span,
                IrOp::ObjSetAtom {
                    obj: session_obj,
                    atom_id,
                    value: dst.v,
                },
            );
        }
    }
    Ok((dst.v, dst.tid))
}

pub(super) fn lower_assign_stmt(
    s: &Stmt,
    name: &str,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let dst = *ctx
        .env_stack
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Name,
                s.span,
                format!("unknown variable '{}'", name),
            )
        })?;
    let (rhs, rt) = lower_expr(expr0, ctx, b)?;
    let rhs = if rt != dst.tid {
        if dst.tid == T_DYNAMIC {
            let out = b.new_vreg(T_DYNAMIC);
            b.emit(s.span, IrOp::ToDyn { dst: out, src: rhs });
            out
        } else {
            let coerced = coerce_numeric(s.span, rhs, rt, dst.tid, b).map_err(|_| {
                CompileError::new(
                    ErrorKind::Type,
                    s.span,
                    format!("assignment to '{}' changes type", name),
                )
            })?;
            if is_narrowing_numeric(rt, dst.tid) {
                ctx.warnings.push(crate::error::CompileWarning::new(
                    s.span,
                    format!(
                        "implicit narrowing conversion in assignment from {} to {}",
                        expr::type_name(rt),
                        expr::type_name(dst.tid)
                    ),
                ));
            }
            coerced
        }
    } else {
        rhs
    };
    b.emit(
        s.span,
        IrOp::Mov {
            dst: dst.v,
            src: rhs,
        },
    );
    if ctx.exported_bindings.contains(name) {
        if let Some(exports_obj) = ctx.exports_obj {
            let atom_id = intern_atom(name, ctx);
            b.emit(
                s.span,
                IrOp::ObjSetAtom {
                    obj: exports_obj,
                    atom_id,
                    value: dst.v,
                },
            );
        }
    }
    if ctx.session_imported_names.contains(name) {
        if let Some(session_obj) = ctx.session_obj {
            let atom_id = intern_atom(name, ctx);
            b.emit(
                s.span,
                IrOp::ObjSetAtom {
                    obj: session_obj,
                    atom_id,
                    value: dst.v,
                },
            );
        }
    }
    Ok(())
}

/// Lower member assignment as expression: returns the assigned value.
pub(crate) fn lower_member_assign_expr(
    e: &Expr,
    base: &Expr,
    name: &str,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    ensure_open_block(b);
    let (v_obj, t_obj) = lower_expr(base, ctx, b)?;
    let te = ctx.type_ctx.types.get(t_obj as usize).ok_or_else(|| {
        CompileError::new(
            ErrorKind::Internal,
            base.span,
            "bad member assignment base type id",
        )
    })?;
    if te.kind != crate::jlyb::TypeKind::Object {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "member assignment target must be Object",
        ));
    }
    if ctx.type_ctx.is_tuple_type(t_obj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "tuples are immutable",
        ));
    }
    let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
    let atom_id = intern_atom(name, ctx);
    b.emit(
        e.span,
        IrOp::ObjSetAtom {
            obj: v_obj,
            atom_id,
            value: v_val,
        },
    );
    Ok((v_val, t_val))
}

pub(super) fn lower_member_assign_stmt(
    s: &Stmt,
    base: &Expr,
    name: &str,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let (v_obj, t_obj) = lower_expr(base, ctx, b)?;
    let te = ctx.type_ctx.types.get(t_obj as usize).ok_or_else(|| {
        CompileError::new(
            ErrorKind::Internal,
            base.span,
            "bad member assignment base type id",
        )
    })?;
    if te.kind != crate::jlyb::TypeKind::Object {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "member assignment target must be Object",
        ));
    }
    if ctx.type_ctx.is_tuple_type(t_obj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "tuples are immutable",
        ));
    }
    let (v_val, _t_val) = lower_expr(expr0, ctx, b)?;
    let atom_id = intern_atom(name, ctx);
    b.emit(
        s.span,
        IrOp::ObjSetAtom {
            obj: v_obj,
            atom_id,
            value: v_val,
        },
    );
    Ok(())
}

pub(super) fn lower_index_assign_stmt(
    s: &Stmt,
    base: &Expr,
    index: &Expr,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let (v_base, t_base) = lower_expr(base, ctx, b)?;
    let (v_idx_raw, t_idx_raw) = lower_expr(index, ctx, b)?;
    let v_idx = match t_idx_raw {
        T_I32 => v_idx_raw,
        T_I8 | T_I16 | T_I64 => coerce_numeric(index.span, v_idx_raw, t_idx_raw, T_I32, b)?,
        T_DYNAMIC => lower_expr_expect(index, ctx, b)?.0,
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                index.span,
                "index must be an integer",
            ))
        }
    };
    match t_base {
        T_ARRAY_I32 => {
            let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
            let v_val = if t_val == T_I32 {
                v_val
            } else if expr::is_numeric(t_val) {
                let coerced = coerce_numeric(expr0.span, v_val, t_val, T_I32, b).map_err(|_| {
                    CompileError::new(
                        ErrorKind::Type,
                        expr0.span,
                        "Array<I32> index assignment requires numeric value",
                    )
                })?;
                if is_narrowing_numeric(t_val, T_I32) {
                    ctx.warnings.push(crate::error::CompileWarning::new(
                        expr0.span,
                        format!(
                            "implicit narrowing conversion in array assignment from {} to {}",
                            expr::type_name(t_val),
                            expr::type_name(T_I32)
                        ),
                    ));
                }
                coerced
            } else {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    expr0.span,
                    "Array<I32> index assignment requires numeric value",
                ));
            };
            b.emit(
                s.span,
                IrOp::ArraySet {
                    arr: v_base,
                    index: v_idx,
                    value: v_val,
                },
            );
            Ok(())
        }
        T_ARRAY_BYTES => {
            let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
            if t_val != T_BYTES {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    expr0.span,
                    "Array<Bytes> index assignment requires Bytes value",
                ));
            }
            b.emit(
                s.span,
                IrOp::ArraySet {
                    arr: v_base,
                    index: v_idx,
                    value: v_val,
                },
            );
            Ok(())
        }
        T_BYTES => {
            let (v_val, t_val) = lower_expr(expr0, ctx, b)?;
            let v_val = match t_val {
                T_I32 => v_val,
                T_I8 | T_I16 | T_I64 | T_F16 | T_F32 | T_F64 => {
                    let coerced =
                        coerce_numeric(expr0.span, v_val, t_val, T_I32, b).map_err(|_| {
                            CompileError::new(
                                ErrorKind::Type,
                                expr0.span,
                                "bytes index assignment requires numeric value",
                            )
                        })?;
                    if is_narrowing_numeric(t_val, T_I32) {
                        ctx.warnings.push(crate::error::CompileWarning::new(
                            expr0.span,
                            format!(
                                "implicit narrowing conversion in bytes assignment from {} to {}",
                                expr::type_name(t_val),
                                expr::type_name(T_I32)
                            ),
                        ));
                    }
                    coerced
                }
                T_DYNAMIC => lower_expr_expect(expr0, ctx, b)?.0,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        expr0.span,
                        "bytes index assignment requires numeric value",
                    ))
                }
            };
            b.emit(
                s.span,
                IrOp::BytesSetU8 {
                    bytes: v_base,
                    index: v_idx,
                    value: v_val,
                },
            );
            Ok(())
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "index assignment only supported for Array<I32>, Array<Bytes>, and bytes",
        )),
    }
}
