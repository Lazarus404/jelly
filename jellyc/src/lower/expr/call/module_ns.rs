use crate::ast::ExprKind;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use crate::lower::intern_atom;

use super::super::{lower_expr_expect, LowerCtx, T_OBJECT};

pub(super) fn try_lower_module_namespace_call(
    e: &crate::ast::Expr,
    callee: &crate::ast::Expr,
    args: &[crate::ast::Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    let ExprKind::Member { base, name } = &callee.node else {
        return Ok(None);
    };
    let ExprKind::Var(alias) = &base.node else {
        return Ok(None);
    };
    if !ctx.module_alias_exports.contains_key(alias) {
        return Ok(None);
    }

    let (v_obj, t_obj) = lower_expr_expect(base, ctx, b)?;
    if t_obj != T_OBJECT {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "module namespace must be Object",
        ));
    }

    // If we know this export's declared function type, enforce it.
    let maybe_tid = ctx
        .module_alias_exports
        .get(alias)
        .and_then(|m| m.get(name))
        .copied();

    let (fun_tid, sig_id, sig_args, sig_ret) = if let Some(exp_tid) = maybe_tid {
        let te =
            ctx.type_ctx.types.get(exp_tid as usize).ok_or_else(|| {
                CompileError::new(ErrorKind::Internal, e.span, "bad export type id")
            })?;
        if te.kind != crate::jlyb::TypeKind::Function {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "module member is not callable",
            ));
        }
        let sig_id = te.p0;
        let sig = ctx
            .type_ctx
            .sigs
            .get(sig_id as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?
            .clone();
        (exp_tid, sig_id, sig.args, sig.ret_type)
    } else {
        return Err(CompileError::new(
            ErrorKind::Name,
            e.span,
            format!("unknown export '{}.{}'", alias, name),
        ));
    };

    if sig_args.len() != args.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "call arity mismatch",
        ));
    }

    // Evaluate args left-to-right with expected types.
    let mut arg_regs: Vec<VRegId> = Vec::with_capacity(args.len());
    for (i, a) in args.iter().enumerate() {
        let et = sig_args[i];
        let (v, t) = lower_expr_expect(a, ctx, b)?;
        if et == crate::typectx::T_DYNAMIC && t != crate::typectx::T_DYNAMIC {
            let out = b.new_vreg(crate::typectx::T_DYNAMIC);
            b.emit(a.span, IrOp::ToDyn { dst: out, src: v });
            arg_regs.push(out);
            continue;
        }
        if t != et {
            return Err(CompileError::new(
                ErrorKind::Internal,
                a.span,
                "semantic type mismatch for module export call arg",
            ));
        }
        arg_regs.push(v);
    }

    let atom_id = intern_atom(name, ctx);
    let v_f = b.new_vreg(fun_tid);
    b.emit(
        e.span,
        IrOp::ObjGetAtom {
            dst: v_f,
            obj: v_obj,
            atom_id,
        },
    );

    let (arg_base, nargs) =
        super::marshal::marshal_args_window(e.span, &sig_args, &arg_regs, b, false);
    let out = b.new_vreg(sig_ret);
    b.emit(
        e.span,
        IrOp::Call {
            dst: out,
            callee: v_f,
            sig_id,
            arg_base,
            nargs,
        },
    );

    Ok(Some((out, sig_ret)))
}
