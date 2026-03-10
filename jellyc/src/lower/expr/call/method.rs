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

use crate::lower::intern_atom;
use crate::lower::is_object_kind;

use super::super::{lower_expr_expect, LowerCtx, T_OBJECT};

pub(super) fn try_lower_method_sugar_call(
    e: &crate::ast::Expr,
    callee: &crate::ast::Expr,
    args: &[crate::ast::Expr],
    ret_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    let ExprKind::Member { base, name } = &callee.node else {
        return Ok(None);
    };

    // Module namespace call: `Mod.f(args...)` does NOT bind `this` (handled elsewhere).
    if let ExprKind::Var(alias) = &base.node {
        if ctx.module_alias_exports.contains_key(alias) {
            return Ok(None);
        }
    }

    let (v_obj, t_obj) = lower_expr_expect(base, ctx, b)?;
    if !is_object_kind(&ctx.type_ctx, t_obj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            base.span,
            "method receiver must be Object",
        ));
    }

    // Evaluate args left-to-right.
    let mut arg_vals: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
    for a in args {
        arg_vals.push(lower_expr_expect(a, ctx, b)?);
    }

    // Unbound method type: (Object, A...) -> R
    let mut unbound_args: Vec<TypeId> = Vec::with_capacity(1 + arg_vals.len());
    unbound_args.push(T_OBJECT);
    unbound_args.extend(arg_vals.iter().map(|(_, t)| *t));
    let unbound_fun_tid = ctx.type_ctx.intern_fun_type(ret_tid, &unbound_args);

    // Bound method type (after BIND_THIS): (A...) -> R
    let bound_args: Vec<TypeId> = arg_vals.iter().map(|(_, t)| *t).collect();
    let bound_sig_id = ctx.type_ctx.intern_sig(ret_tid, &bound_args);
    let bound_fun_tid = ctx.type_ctx.intern_fun_type(ret_tid, &bound_args);

    let atom_id = intern_atom(name, ctx);

    // ConstFun for method refs: when obj has nominal type and method is in table, emit ConstFun.
    // When we use ConstFun, skip BindThis and call directly with (obj, ...args) so codegen
    // can emit direct CALL instead of CALLR.
    let use_const_fun = ctx.type_ctx.is_nominal_object_type(t_obj)
        && ctx.method_table.get(&(t_obj, atom_id)).is_some();

    let v_unbound = b.new_vreg(unbound_fun_tid);
    if use_const_fun {
        let func_index = ctx.method_table.get(&(t_obj, atom_id)).copied().unwrap();
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: v_unbound,
                func_index,
            },
        );
    } else {
        b.emit(
            e.span,
            IrOp::ObjGetAtom {
                dst: v_unbound,
                obj: v_obj,
                atom_id,
            },
        );
    }

    let (callee, arg_tids, sig_id) = if use_const_fun {
        // Direct call: pass (obj, ...args), callee is v_unbound (ConstFun).
        let unbound_sig_id = ctx.type_ctx.intern_sig(ret_tid, &unbound_args);
        let mut all_arg_regs: Vec<VRegId> = Vec::with_capacity(1 + arg_vals.len());
        all_arg_regs.push(v_obj);
        all_arg_regs.extend(arg_vals.iter().map(|(v, _)| *v));
        let (arg_base, nargs) =
            super::marshal::marshal_args_window(e.span, &unbound_args, &all_arg_regs, b, false);
        let out = b.new_vreg(ret_tid);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: v_unbound,
                sig_id: unbound_sig_id,
                arg_base,
                nargs,
            },
        );
        return Ok(Some((out, ret_tid)));
    } else {
        // BindThis path: callee is bound method, args are just method args.
        let v_bound = b.new_vreg(bound_fun_tid);
        b.emit(
            e.span,
            IrOp::BindThis {
                dst: v_bound,
                func: v_unbound,
                this: v_obj,
            },
        );
        (v_bound, bound_args, bound_sig_id)
    };

    // Build a contiguous vreg window holding the arguments (BindThis path).
    let arg_regs: Vec<VRegId> = arg_vals.iter().map(|(v, _)| *v).collect();
    let (arg_base, nargs) =
        super::marshal::marshal_args_window(e.span, &arg_tids, &arg_regs, b, false);

    let out = b.new_vreg(ret_tid);
    b.emit(
        e.span,
        IrOp::Call {
            dst: out,
            callee,
            sig_id,
            arg_base,
            nargs,
        },
    );

    Ok(Some((out, ret_tid)))
}
