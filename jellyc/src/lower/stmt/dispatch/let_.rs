use crate::ast::{Expr, ExprKind, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, ConstValue, NodeId};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use crate::lower::expr;
use crate::lower::expr::{coerce_numeric, is_narrowing_numeric, lower_expr, lower_expr_expect};
use crate::lower::{
    bind_local_with_func_index, ensure_open_block, intern_atom, lookup_var, resolve_opt_ty,
    LowerCtx, T_DYNAMIC,
};

mod const_init;

/// Lower `let x = e` as an expression. Evaluates to the value of `e` and binds `x` in scope.
pub fn lower_let_expr(
    e: &Expr,
    is_const: bool,
    name: &str,
    type_params: &[String],
    ty: &Option<Ty>,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    ensure_open_block(b);
    if !type_params.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "template lets must be expanded before lowering",
        ));
    }
    let is_discard = name == "_";
    if is_discard {
        let (v, tid) = lower_expr(expr0, ctx, b)?;
        if is_const {
            let _ = ctx.sem_const_inits.get(&NodeId(e.span));
        }
        return Ok((v, tid));
    }
    if ctx.env_stack.iter().any(|m| m.contains_key(name)) {
        return Err(CompileError::new(
            ErrorKind::Name,
            e.span,
            format!("duplicate let binding '{}'", name),
        ));
    }
    if is_const {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "const in let expression not yet supported",
        ));
    }

    let expect =
        resolve_opt_ty(ty, ctx)?.or_else(|| ctx.sem_binding_types.get(&NodeId(e.span)).copied());

    if let (Some(et), true) = (expect, matches!(&expr0.node, ExprKind::Fn { .. })) {
        ctx.pending_fn_self = Some((name.to_string(), et));
        let dst = b.new_vreg(et);
        bind_local_with_func_index(ctx, name, dst, et, None);
        let (v, tid) = lower_expr_expect(expr0, ctx, b)?;
        ctx.pending_fn_self = None;
        if tid != et {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "let initializer type mismatch",
            ));
        }
        if v != dst {
            b.emit(e.span, IrOp::Mov { dst, src: v });
        }
        let fi = ctx.last_const_fun_index.take();
        bind_local_with_func_index(ctx, name, dst, et, fi);
        return Ok((dst, et));
    }

    let proto_tid = expect.and_then(|et| {
        if ctx.type_ctx.is_nominal_object_type(et) && matches!(expr0.node, ExprKind::ObjLit(_)) {
            Some(et)
        } else {
            None
        }
    });
    if let Some(tid) = proto_tid {
        ctx.proto_for_nominal = Some(tid);
    }
    let (v0, tid0) = lower_expr_expect(expr0, ctx, b)?;
    ctx.proto_for_nominal = None;
    ctx.pending_fn_self = None;
    let (v, tid) = if let Some(et) = expect {
        if tid0 != et {
            if et == T_DYNAMIC {
                let out = b.new_vreg(T_DYNAMIC);
                b.emit(e.span, IrOp::ToDyn { dst: out, src: v0 });
                (out, T_DYNAMIC)
            } else {
                match coerce_numeric(e.span, v0, tid0, et, b) {
                    Ok(coerced) => {
                        if is_narrowing_numeric(tid0, et) {
                            ctx.warnings.push(crate::error::CompileWarning::new(
                                e.span,
                                format!(
                                    "implicit narrowing conversion in let initializer from {} to {}",
                                    expr::type_name(tid0),
                                    expr::type_name(et)
                                ),
                            ));
                        }
                        (coerced, et)
                    }
                    Err(_) => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "let initializer type mismatch",
                        ));
                    }
                }
            }
        } else {
            (v0, tid0)
        }
    } else {
        (v0, tid0)
    };

    let init_is_aliased = ctx.env_stack.iter().any(|m| m.values().any(|bd| bd.v == v));
    let init_is_member = matches!(expr0.node, ExprKind::Member { .. });
    let dst = if init_is_aliased || init_is_member {
        let dst = b.new_vreg(tid);
        if v != dst {
            b.emit(e.span, IrOp::Mov { dst, src: v });
        }
        dst
    } else {
        v
    };

    let fi = ctx.last_const_fun_index.take();
    bind_local_with_func_index(ctx, name, dst, tid, fi);
    Ok((dst, tid))
}

pub(super) fn lower_let_stmt(
    s: &Stmt,
    is_const: bool,
    exported: bool,
    name: &str,
    type_params: &[String],
    ty: &Option<Ty>,
    expr0: &Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    if !type_params.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "template lets must be expanded before lowering",
        ));
    }
    let is_discard = name == "_";
    if !is_discard && ctx.env_stack.iter().any(|m| m.contains_key(name)) {
        return Err(CompileError::new(
            ErrorKind::Name,
            s.span,
            format!("duplicate let binding '{}'", name),
        ));
    }
    if is_discard {
        if !is_const {
            let _ = lower_expr(expr0, ctx, b)?;
        }
        return Ok(());
    }

    if is_const {
        // Const bindings lower directly from the semantic const initializer plan,
        // which avoids runtime computation (and preserves Bytes identity on alias).
        let bind_tid = resolve_opt_ty(ty, ctx)?
            .or_else(|| ctx.sem_binding_types.get(&NodeId(s.span)).copied())
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    s.span,
                    "missing semantic type for const binding",
                )
            })?;

        let init = ctx
            .sem_const_inits
            .get(&NodeId(s.span))
            .cloned()
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    s.span,
                    "missing semantic const initializer (const evaluator did not run?)",
                )
            })?;

        let v = match init {
            ConstInit::Alias(src) => {
                let bd = lookup_var(ctx, src.as_str(), s.span)?;
                if bd.tid != bind_tid {
                    let out = b.new_vreg(bind_tid);
                    b.emit(
                        s.span,
                        IrOp::Mov {
                            dst: out,
                            src: bd.v,
                        },
                    );
                    out
                } else {
                    bd.v
                }
            }
            ConstInit::Value(cv) => {
                if bind_tid == T_DYNAMIC {
                    match cv {
                        ConstValue::Null => const_init::emit_const_typed(
                            s.span,
                            T_DYNAMIC,
                            ConstValue::Null,
                            ctx,
                            b,
                        )?,
                        other => {
                            let src_tid = ctx
                                .sem_expr_types
                                .get(&NodeId(expr0.span))
                                .copied()
                                .unwrap_or(T_DYNAMIC);
                            let src = const_init::emit_const_typed(s.span, src_tid, other, ctx, b)?;
                            let out = b.new_vreg(T_DYNAMIC);
                            b.emit(s.span, IrOp::ToDyn { dst: out, src });
                            out
                        }
                    }
                } else {
                    const_init::emit_const_typed(s.span, bind_tid, cv, ctx, b)?
                }
            }
        };

        bind_local_with_func_index(ctx, name, v, bind_tid, None);

        if exported || ctx.export_all_lets {
            let exports_obj = ctx.exports_obj.ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Name,
                    s.span,
                    "export requires module compilation (use --backend ir with imports/exports)",
                )
            })?;
            let atom_id = intern_atom(name, ctx);
            b.emit(
                s.span,
                IrOp::ObjSetAtom {
                    obj: exports_obj,
                    atom_id,
                    value: v,
                },
            );
        }
        return Ok(());
    }

    let expect =
        resolve_opt_ty(ty, ctx)?.or_else(|| ctx.sem_binding_types.get(&NodeId(s.span)).copied());

    // Preserve self-recursive fn literal lowering behavior.
    if let (Some(et), true) = (expect, matches!(&expr0.node, ExprKind::Fn { .. })) {
        ctx.pending_fn_self = Some((name.to_string(), et));
        let dst = b.new_vreg(et);
        bind_local_with_func_index(ctx, name, dst, et, None);
        let (v, tid) = lower_expr_expect(expr0, ctx, b)?;
        ctx.pending_fn_self = None;
        if tid != et {
            return Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "let initializer type mismatch",
            ));
        }
        if v != dst {
            b.emit(s.span, IrOp::Mov { dst, src: v });
        }
        // Update binding with func_index so method table can record (nom_tid, atom) -> func_index.
        let fi = ctx.last_const_fun_index.take();
        bind_local_with_func_index(ctx, name, dst, et, fi);
        if exported || ctx.export_all_lets {
            let exports_obj = ctx.exports_obj.ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Name,
                    s.span,
                    "export requires module compilation (use --backend ir with imports/exports)",
                )
            })?;
            let atom_id = intern_atom(name, ctx);
            b.emit(
                s.span,
                IrOp::ObjSetAtom {
                    obj: exports_obj,
                    atom_id,
                    value: dst,
                },
            );
        }
        Ok(())
    } else {
        // When let Name: Name = { ... } (prototype init), record method table from object literal.
        let proto_tid = expect.and_then(|et| {
            if ctx.type_ctx.is_nominal_object_type(et) && matches!(expr0.node, ExprKind::ObjLit(_)) {
                Some(et)
            } else {
                None
            }
        });
        if let Some(tid) = proto_tid {
            ctx.proto_for_nominal = Some(tid);
        }
        let (v0, tid0) = lower_expr_expect(expr0, ctx, b)?;
        ctx.proto_for_nominal = None;
        ctx.pending_fn_self = None;
        let (v, tid) = if let Some(et) = expect {
            if tid0 != et {
                if et == T_DYNAMIC {
                    let out = b.new_vreg(T_DYNAMIC);
                    b.emit(s.span, IrOp::ToDyn { dst: out, src: v0 });
                    (out, T_DYNAMIC)
                } else {
                    match coerce_numeric(s.span, v0, tid0, et, b) {
                        Ok(coerced) => {
                            if is_narrowing_numeric(tid0, et) {
                                ctx.warnings.push(crate::error::CompileWarning::new(
                                    s.span,
                                    format!(
                                        "implicit narrowing conversion in let initializer from {} to {}",
                                        expr::type_name(tid0),
                                        expr::type_name(et)
                                    ),
                                ));
                            }
                            (coerced, et)
                        }
                        Err(_) => {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                s.span,
                                "let initializer type mismatch",
                            ));
                        }
                    }
                }
            } else {
                (v0, tid0)
            }
        } else {
            (v0, tid0)
        };

        // Avoid aliasing: if the initializer vreg is already bound to some name,
        // `let y = x; y = ...` must not mutate `x`.
        // Also always use a fresh slot for Member (ObjGetAtom) results to avoid reg-alloc overwrites.
        let init_is_aliased = ctx.env_stack.iter().any(|m| m.values().any(|bd| bd.v == v));
        let init_is_member = matches!(expr0.node, ExprKind::Member { .. });
        let dst = if init_is_aliased || init_is_member {
            let dst = b.new_vreg(tid);
            if v != dst {
                b.emit(s.span, IrOp::Mov { dst, src: v });
            }
            dst
        } else {
            v
        };

        let fi = ctx.last_const_fun_index.take();
        bind_local_with_func_index(ctx, name, dst, tid, fi);

        if exported || ctx.export_all_lets {
            let exports_obj = ctx.exports_obj.ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Name,
                    s.span,
                    "export requires module compilation (use --backend ir with imports/exports)",
                )
            })?;
            let atom_id = intern_atom(name, ctx);
            b.emit(
                s.span,
                IrOp::ObjSetAtom {
                    obj: exports_obj,
                    atom_id,
                    value: dst,
                },
            );
        }
        Ok(())
    }
}
