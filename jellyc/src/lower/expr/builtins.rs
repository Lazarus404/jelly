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
use crate::typectx::{T_F16, T_F32, T_F64, T_I8, T_I16, T_I32, T_I64};

use crate::lower::intern_atom;
use crate::lower::is_object_kind;

use crate::builtin_constraints as builtin_constraints;

use super::{
    coerce_numeric, lower_expr_expect, LowerCtx, T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM,
    T_BOOL, T_BYTES, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

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
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing semantic type for builtin call"))?;
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
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "assert expects Bool"));
        }
        b.emit(e.span, IrOp::Assert { cond: vcond });
        return Ok(Some((vcond, out_tid)));
    }

    // Numeric conversion: Integer.to_i8, Integer.to_i16, Integer.to_i32, Integer.to_i64,
    // Float.to_f16, Float.to_f32, Float.to_f64.
    if args.len() == 1 {
        let target_tid = match (ns, name) {
            ("Integer", "to_i8") => Some(T_I8),
            ("Integer", "to_i16") => Some(T_I16),
            ("Integer", "to_i32") => Some(T_I32),
            ("Integer", "to_i64") => Some(T_I64),
            ("Float", "to_f16") => Some(T_F16),
            ("Float", "to_f32") => Some(T_F32),
            ("Float", "to_f64") => Some(T_F64),
            _ => None,
        };
        if let Some(tid) = target_tid {
            let (v, t) = lower_expr_expect(&args[0], ctx, b)?;
            if !super::is_numeric(t) {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    args[0].span,
                    "semantic type mismatch for numeric conversion builtin",
                ));
            }
            let out = coerce_numeric(e.span, v, t, tid, b).map_err(|_| {
                CompileError::new(
                    ErrorKind::Internal,
                    args[0].span,
                    "unsupported numeric conversion in lowering",
                )
            })?;
            return Ok(Some((out, tid)));
        }
    }

    // Math.sqrt(x): any numeric -> F64 (native builtin)
    if ns == "Math" && name == "sqrt" {
        let (v, t) = lower_expr_expect(&args[0], ctx, b)?;
        if !super::is_numeric(t) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Math.sqrt",
            ));
        }
        let v_f64 = coerce_numeric(e.span, v, t, T_F64, b).map_err(|_| {
            CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "unsupported numeric conversion for Math.sqrt in lowering",
            )
        })?;

        let sig_args = [T_F64];
        let sig_id = ctx.type_ctx.intern_sig(T_F64, &sig_args);
        let fun_tid = ctx.type_ctx.intern_fun_type(T_F64, &sig_args);

        let vcallee = b.new_vreg(fun_tid);
        b.emit(
            e.span,
            IrOp::ConstFun {
                dst: vcallee,
                func_index: crate::jlyb::NATIVE_BUILTIN_MATH_SQRT,
            },
        );

        let arg0 = b.new_vreg(T_F64);
        b.emit(args[0].span, IrOp::Mov { dst: arg0, src: v_f64 });

        let out = b.new_vreg(T_F64);
        b.emit(
            e.span,
            IrOp::Call {
                dst: out,
                callee: vcallee,
                sig_id,
                arg_base: arg0,
                nargs: 1,
            },
        );
        return Ok(Some((out, T_F64)));
    }

    // Bytes.*
    if ns == "Bytes" && name == "new" {
        let (vlen, tlen) = lower_expr_expect(&args[0], ctx, b)?;
        if tlen != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Bytes.new",
            ));
        }
        let out = b.new_vreg(T_BYTES);
        b.emit(e.span, IrOp::BytesNew { dst: out, len: vlen });
        return Ok(Some((out, T_BYTES)));
    }
    if ns == "Bytes" && name == "len" {
        let (vb, tb) = lower_expr_expect(&args[0], ctx, b)?;
        if tb != T_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Bytes.len",
            ));
        }
        let out = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::BytesLen { dst: out, bytes: vb });
        return Ok(Some((out, T_I32)));
    }
    if ns == "Bytes" && name == "get_u8" {
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
        b.emit(e.span, IrOp::BytesGetU8 { dst: out, bytes: vb, index: vi });
        return Ok(Some((out, T_I32)));
    }
    if ns == "Bytes" && name == "set_u8" {
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
        b.emit(e.span, IrOp::BytesSetU8 { bytes: vb, index: vi, value: vv });
        return Ok(Some((vb, T_BYTES)));
    }
    if ns == "Bytes" && name == "slice" {
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
        b.emit(args[1].span, IrOp::Mov { dst: arg1, src: v_start });
        let arg2 = b.new_vreg(T_I32);
        b.emit(args[2].span, IrOp::Mov { dst: arg2, src: v_len });

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
    if ns == "Bytes" && name == "eq" {
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
            CompileError::new(ErrorKind::Type, args[0].span, "Atom.intern expects UTF-8 bytes")
        })?;
        let atom_id = intern_atom(name, ctx);
        let out = b.new_vreg(T_ATOM);
        b.emit(e.span, IrOp::ConstAtom { dst: out, atom_id });
        return Ok(Some((out, T_ATOM)));
    }

    // Object.*
    if ns == "Object" && name == "get" {
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
        b.emit(e.span, IrOp::ObjGet { dst: out, obj: v_obj, atom: v_key });
        return Ok(Some((out, out_tid)));
    }
    if ns == "Object" && name == "set" {
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
        b.emit(e.span, IrOp::ObjSet { obj: v_obj, atom: v_key, value: v_val });
        if out_tid != t_obj && out_tid != T_OBJECT {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Object.set",
            ));
        }
        return Ok(Some((v_obj, out_tid)));
    }

    // Array.*
    if ns == "Array" && name == "new" {
        let (vlen, tlen) = lower_expr_expect(&args[0], ctx, b)?;
        if tlen != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Array.new",
            ));
        }
        let arr_tid = out_tid;
        let out = b.new_vreg(arr_tid);
        b.emit(e.span, IrOp::ArrayNew { dst: out, len: vlen });
        return Ok(Some((out, arr_tid)));
    }

    if ns == "Array" && name == "len" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        if ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for Array.len receiver",
            ));
        }
        let out = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ArrayLen { dst: out, arr: va });
        return Ok(Some((out, T_I32)));
    }
    if ns == "Array" && name == "get" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], ctx, b)?;
        if (ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES) || ti != T_I32 {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.get",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ArrayGet { dst: out, arr: va, index: vi });
        return Ok(Some((out, out_tid)));
    }
    if ns == "Array" && name == "set" {
        let (va, ta) = lower_expr_expect(&args[0], ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], ctx, b)?;
        let want = super::elem_tid_for_array(ta).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.set receiver",
            )
        })?;
        let (vv, tv) = lower_expr_expect(&args[2], ctx, b)?;
        if ti != T_I32 || tv != want {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.set",
            ));
        }
        b.emit(e.span, IrOp::ArraySet { arr: va, index: vi, value: vv });
        if out_tid != ta {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for Array.set return type",
            ));
        }
        return Ok(Some((va, out_tid)));
    }

    // List.*
    if ns == "List" && name == "nil" {
        let list_tid = out_tid;
        let out = b.new_vreg(list_tid);
        b.emit(e.span, IrOp::ListNil { dst: out });
        return Ok(Some((out, list_tid)));
    }
    if ns == "List" && name == "cons" {
        let list_tid = out_tid;
        let elem_tid = super::elem_tid_for_list(list_tid).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for List.cons return type",
            )
        })?;
        let (vhead, thead) = lower_expr_expect(&args[0], ctx, b)?;
        let (vtail, ttail) = lower_expr_expect(&args[1], ctx, b)?;
        if thead != elem_tid || ttail != list_tid {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for List.cons",
            ));
        }
        let out = b.new_vreg(list_tid);
        b.emit(e.span, IrOp::ListCons { dst: out, head: vhead, tail: vtail });
        return Ok(Some((out, list_tid)));
    }
    if ns == "List" && name == "head" {
        let (vl, tl) = lower_expr_expect(&args[0], ctx, b)?;
        if super::elem_tid_for_list(tl).is_none() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for List.head receiver",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ListHead { dst: out, list: vl });
        return Ok(Some((out, out_tid)));
    }
    if ns == "List" && name == "tail" {
        let (vl, tl) = lower_expr_expect(&args[0], ctx, b)?;
        if tl != T_LIST_I32 && tl != T_LIST_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for List.tail receiver",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ListTail { dst: out, list: vl });
        return Ok(Some((out, out_tid)));
    }
    if ns == "List" && name == "is_nil" {
        let (vl, tl) = lower_expr_expect(&args[0], ctx, b)?;
        if tl != T_LIST_I32 && tl != T_LIST_BYTES {
            return Err(CompileError::new(
                ErrorKind::Internal,
                args[0].span,
                "semantic type mismatch for List.is_nil receiver",
            ));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ListIsNil { dst: out, list: vl });
        return Ok(Some((out, out_tid)));
    }

    Ok(None)
}

