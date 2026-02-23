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
// Builtin call lowering.
use crate::ast::{Expr, ExprKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};
use crate::lower::intern_atom;

use crate::builtin_constraints;

use crate::typectx::{T_BYTES, T_F64, T_I32};
use super::{lower_expr_expect, LowerCtx, T_ATOM, T_BOOL};

mod array;
mod bytes;
mod list;
mod numeric;
mod object;

fn builtin_name(callee: &Expr) -> Option<(&str, &str)> {
    match &callee.node {
        ExprKind::Var(n) => Some(("", n.as_str())),
        ExprKind::Member { base, name } => match &base.node {
            ExprKind::Var(ns) => Some((ns.as_str(), name.as_str())),
            _ => None,
        },
        _ => None,
    }
}

pub(super) fn try_lower_builtin_call(
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    let Some((ns, name)) = builtin_name(callee) else {
        return Ok(None);
    };

    // Builtin namespaces can be shadowed by local bindings or module imports (`import foo as Bytes`).
    // In that case, treat `Bytes.len(...)` etc as a normal call, not a builtin.
    if !ns.is_empty() {
        let shadowed_by_binding = ctx.env_stack.iter().rev().any(|m| m.contains_key(ns));
        let shadowed_by_module = ctx.module_alias_exports.contains_key(ns);
        if shadowed_by_binding || shadowed_by_module {
            return Ok(None);
        }
    }

    // Validate arity/type-args using the centralized builtin constraints,
    // and use semantic analysis as the source of truth for the result type.
    let out_tid = ctx
        .sem_expr_types
        .get(&NodeId(e.span))
        .copied()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "missing semantic type for builtin call",
            )
        })?;
    // Some builtins have return types depending on arg0's concrete container type (Array/List);
    // use the centralized helpers that take the arg0 type. Others can use `builtin_constraints`.
    let cs = if ns == "Array" && matches!(name, "len" | "get" | "set") {
        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                format!("Array.{} does not take type arguments", name),
            ));
        }
        let a0 = args.get(0).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Type,
                e.span,
                format!("Array.{} expects at least 1 arg", name),
            )
        })?;
        let t_arr = ctx
            .sem_expr_types
            .get(&NodeId(a0.span))
            .copied()
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    a0.span,
                    "missing semantic type for Array.* receiver",
                )
            })?;
        builtin_constraints::array_constraints_from_arr_tid(name, args.len(), t_arr, e.span)?
    } else if ns == "List" && matches!(name, "head" | "tail" | "is_nil") {
        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                format!("List.{} does not take type arguments", name),
            ));
        }
        let a0 = args.get(0).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Type,
                e.span,
                format!("List.{} expects at least 1 arg", name),
            )
        })?;
        let t_list = ctx
            .sem_expr_types
            .get(&NodeId(a0.span))
            .copied()
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    a0.span,
                    "missing semantic type for List.* receiver",
                )
            })?;
        builtin_constraints::list_constraints_from_list_tid(name, args.len(), t_list, e.span)?
    } else {
        builtin_constraints::builtin_constraints(
            callee,
            type_args,
            args.len(),
            Some(out_tid),
            &mut ctx.type_ctx,
            true,
            e.span,
        )?
    };
    let Some(cs) = cs else { return Ok(None) };
    if cs.ret != out_tid {
        return Err(CompileError::new(
            ErrorKind::Internal,
            e.span,
            "builtin constraint return type does not match semantic type",
        ));
    }

    // System.assert(cond: Bool) -> Bool (throws if cond is false)
    if ns == "System" && name == "assert" {
        let (vcond, tcond) = lower_expr_expect(&args[0], ctx, b)?;
        if tcond != T_BOOL {
            return Err(CompileError::new(
                ErrorKind::Type,
                args[0].span,
                "assert expects Bool",
            ));
        }
        b.emit(e.span, IrOp::Assert { cond: vcond });
        return Ok(Some((vcond, out_tid)));
    }

    // System.exit() -> never returns (native builtin; exits process with code 123)
    if ns == "System" && name == "exit" {
        let sig_args: [u32; 0] = [];
        let sig_id = ctx.type_ctx.intern_sig(out_tid, &sig_args);
        let vcallee = b.new_vreg(ctx.type_ctx.intern_fun_type(out_tid, &sig_args));
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::NATIVE_BUILTIN_SYSTEM_EXIT,
            },
        );
        let arg_base = b.new_vreg(out_tid); // dummy; nargs=0
        let out = b.new_vreg(out_tid);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base,
                nargs: 0,
            },
        );
        return Ok(Some((out, out_tid)));
    }

    // I32.to_bytes(x) -> Bytes (native builtin; converts I32 to decimal string)
    if ns == "I32" && name == "to_bytes" {
        let arg0 = args.first().ok_or_else(|| {
            CompileError::new(ErrorKind::Type, e.span, "I32.to_bytes expects 1 arg")
        })?;
        let (v_arg, t_arg) = lower_expr_expect(arg0, ctx, b)?;
        if t_arg != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Type,
                arg0.span,
                "I32.to_bytes expects I32",
            ));
        }
        let sig_args = [T_I32];
        let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
        let vcallee = b.new_vreg(ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args));
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::NATIVE_BUILTIN_I32_TO_BYTES,
            },
        );
        let out = b.new_vreg(T_BYTES);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base: v_arg,
                nargs: 1,
            },
        );
        return Ok(Some((out, T_BYTES)));
    }

    // F64.to_bytes(x) -> Bytes (native builtin; converts F64 to string)
    if ns == "F64" && name == "to_bytes" {
        let arg0 = args.first().ok_or_else(|| {
            CompileError::new(ErrorKind::Type, e.span, "F64.to_bytes expects 1 arg")
        })?;
        let (v_arg, t_arg) = lower_expr_expect(arg0, ctx, b)?;
        if t_arg != T_F64 {
            return Err(CompileError::new(
                ErrorKind::Type,
                arg0.span,
                "F64.to_bytes expects F64",
            ));
        }
        let sig_args = [T_F64];
        let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
        let vcallee = b.new_vreg(ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args));
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::NATIVE_BUILTIN_F64_TO_BYTES,
            },
        );
        let out = b.new_vreg(T_BYTES);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base: v_arg,
                nargs: 1,
            },
        );
        return Ok(Some((out, T_BYTES)));
    }

    if let Some(res) = numeric::try_lower_numeric_builtin(e, ns, name, args, ctx, b)? {
        return Ok(Some(res));
    }

    if let Some(res) = bytes::try_lower_bytes_builtin(e, ns, name, args, ctx, b)? {
        return Ok(Some(res));
    }

    if let Some(res) = object::try_lower_object_builtin(e, ns, name, out_tid, args, ctx, b)? {
        return Ok(Some(res));
    }

    if let Some(res) = array::try_lower_array_builtin(e, ns, name, out_tid, args, ctx, b)? {
        return Ok(Some(res));
    }

    if let Some(res) = list::try_lower_list_builtin(e, ns, name, out_tid, args, ctx, b)? {
        return Ok(Some(res));
    }

    // Bytes.*
    // (handled above)

    // Atom.*
    if ns == "Atom" && name == "intern" {
        let s = match &args[0].node {
            ExprKind::BytesLit(b) => b,
            _ => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    args[0].span,
                    "Atom.intern expects a bytes literal",
                ))
            }
        };
        let name = std::str::from_utf8(s).map_err(|_| {
            CompileError::new(
                ErrorKind::Type,
                args[0].span,
                "Atom.intern expects UTF-8 bytes",
            )
        })?;
        let atom_id = intern_atom(name, ctx);
        let out = b.new_vreg(T_ATOM);
        b.emit(e.span, IrOp::ConstAtom { dst: out, atom_id });
        return Ok(Some((out, T_ATOM)));
    }

    // Object.*
    // (handled above)

    // Array.*
    // (handled above)

    // List.*
    // (handled above)

    Ok(None)
}
