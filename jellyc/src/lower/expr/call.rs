/**
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

// Call expression lowering (builtins + general calls).

use crate::ast::ExprKind;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use crate::lower::intern_atom;
use super::{coerce_numeric, is_narrowing_numeric, lower_expr_expect, LowerCtx};
use super::{
    T_DYNAMIC, T_OBJECT,
};

pub fn lower_call_expr(
    e: &crate::ast::Expr,
    callee: &crate::ast::Expr,
    type_args: &[crate::ast::Ty],
    args: &[crate::ast::Expr],
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
        // Builtins are handled in a shared module so inference + lowering stay consistent.
        if let Some(out) = super::builtins::try_lower_builtin_call(e, callee, type_args, args, expect, ctx, b)? {
            return Ok(out);
        }

        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "generic type arguments are only supported for builtins (for now)",
            ));
        }

        // General function call.
        // Method call sugar: `obj.m(args...)` binds `this=obj` using `BIND_THIS`.
        if let ExprKind::Member { base, name } = &callee.node {
            // Module namespace call: `Mod.f(args...)` does NOT bind `this`.
            if let ExprKind::Var(alias) = &base.node {
                if ctx.module_alias_exports.contains_key(alias) {
                    let (v_obj, t_obj) = lower_expr_expect(base, Some(T_OBJECT), ctx, b)?;
                    if t_obj != T_OBJECT {
                        return Err(CompileError::new(ErrorKind::Type, base.span, "module namespace must be Object"));
                    }

                    // If we know this export's declared function type, enforce it.
                    let maybe_tid = ctx
                        .module_alias_exports
                        .get(alias)
                        .and_then(|m| m.get(name))
                        .copied();

                    let (fun_tid, sig_id, sig_args, sig_ret) = if let Some(exp_tid) = maybe_tid {
                        let te = ctx
                            .type_ctx
                            .types
                            .get(exp_tid as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad export type id"))?;
                        if te.kind != crate::jlyb::TypeKind::Function {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "module member is not callable"));
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

                    // Evaluate args left-to-right with expected types when available.
                    let mut arg_vals: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
                    for (i, a) in args.iter().enumerate() {
                        let et = sig_args.get(i).copied();
                        arg_vals.push(lower_expr_expect(a, et, ctx, b)?);
                    }

                    let atom_id = intern_atom(name, ctx);
                    let v_f = b.new_vreg(fun_tid);
                    b.emit(e.span, IrOp::ObjGetAtom { dst: v_f, obj: v_obj, atom_id });

                    // Marshal args into a contiguous vreg window.
                    let nargs = arg_vals.len() as u8;
                    let arg_base = if nargs == 0 {
                        VRegId(0)
                    } else {
                        let base = b.new_vreg(sig_args[0]);
                        b.emit(e.span, IrOp::Mov { dst: base, src: arg_vals[0].0 });
                        let mut prev = base;
                        for i in 1..(nargs as usize) {
                            let v = b.new_vreg(sig_args[i]);
                            debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
                            b.emit(e.span, IrOp::Mov { dst: v, src: arg_vals[i].0 });
                            prev = v;
                        }
                        base
                    };

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
                    return Ok((out, sig_ret));
                }
            }

            let ret_tid = expect.unwrap_or(T_DYNAMIC);
            let (v_obj, t_obj) = lower_expr_expect(base, Some(T_OBJECT), ctx, b)?;
            if t_obj != T_OBJECT {
                return Err(CompileError::new(ErrorKind::Type, base.span, "method receiver must be Object"));
            }

            // Evaluate args left-to-right.
            let mut arg_vals: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
            for a in args {
                arg_vals.push(lower_expr_expect(a, None, ctx, b)?);
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
            let v_unbound = b.new_vreg(unbound_fun_tid);
            b.emit(
                e.span,
                IrOp::ObjGetAtom {
                    dst: v_unbound,
                    obj: v_obj,
                    atom_id,
                },
            );

            let v_bound = b.new_vreg(bound_fun_tid);
            b.emit(e.span, IrOp::BindThis { dst: v_bound, func: v_unbound, this: v_obj });

            // Build a contiguous vreg window holding the arguments.
            let nargs = arg_vals.len() as u8;
            let arg_base = if nargs == 0 {
                VRegId(0)
            } else {
                let base = b.new_vreg(bound_args[0]);
                b.emit(e.span, IrOp::Mov { dst: base, src: arg_vals[0].0 });
                let mut prev = base;
                for i in 1..(nargs as usize) {
                    let v = b.new_vreg(bound_args[i]);
                    debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
                    b.emit(e.span, IrOp::Mov { dst: v, src: arg_vals[i].0 });
                    prev = v;
                }
                base
            };

            let out = b.new_vreg(ret_tid);
            b.emit(
                e.span,
                IrOp::Call {
                    dst: out,
                    callee: v_bound,
                    sig_id: bound_sig_id,
                    arg_base,
                    nargs,
                },
            );
            return Ok((out, ret_tid));
        }

        let (vcallee, callee_tid) = lower_expr_expect(callee, None, ctx, b)?;
        let te = ctx
            .type_ctx
            .types
            .get(callee_tid as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad callee type id"))?;
        if te.kind != crate::jlyb::TypeKind::Function {
            return Err(CompileError::new(ErrorKind::Type, callee.span, "call target is not a function"));
        }
        let sig_id = te.p0;
        let (sig_args, sig_ret) = {
            let sig = ctx
                .type_ctx
                .sigs
                .get(sig_id as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?;
            (sig.args.clone(), sig.ret_type)
        };
        if sig_args.len() != args.len() {
            return Err(CompileError::new(ErrorKind::Type, e.span, "call arity mismatch"));
        }
        // Build a contiguous vreg window holding the arguments (implicit widening/narrowing with warning).
        let mut arg_vals: Vec<VRegId> = Vec::with_capacity(args.len());
        for (i, a) in args.iter().enumerate() {
            let (va, ta) = lower_expr_expect(a, Some(sig_args[i]), ctx, b)?;
            let va = if ta != sig_args[i] {
                if super::is_numeric(ta) && super::is_numeric(sig_args[i]) {
                    let coerced = coerce_numeric(a.span, va, ta, sig_args[i], b)
                        .map_err(|_| CompileError::new(ErrorKind::Type, a.span, "call argument type mismatch"))?;
                    if is_narrowing_numeric(ta, sig_args[i]) {
                        ctx.warnings.push(crate::error::CompileWarning::new(
                            a.span,
                            format!(
                                "implicit narrowing conversion from {} to {}",
                                super::type_name(ta),
                                super::type_name(sig_args[i])
                            ),
                        ));
                    }
                    coerced
                } else {
                    return Err(CompileError::new(ErrorKind::Type, a.span, "call argument type mismatch"));
                }
            } else {
                va
            };
            arg_vals.push(va);
        }

        let nargs = arg_vals.len() as u8;
        let arg_base = if nargs == 0 {
            VRegId(0)
        } else {
            let base = b.new_vreg(sig_args[0]);
            b.emit(e.span, IrOp::Mov { dst: base, src: arg_vals[0] });
            let mut prev = base;
            for i in 1..(nargs as usize) {
                let v = b.new_vreg(sig_args[i]);
                debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
                b.emit(e.span, IrOp::Mov { dst: v, src: arg_vals[i] });
                prev = v;
            }
            base
        };
        let out_tid = sig_ret;
        let out = b.new_vreg(out_tid);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base,
                nargs,
            },
        );
        Ok((out, out_tid))
}
