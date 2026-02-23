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
// Builtin call handling (shared by call lowering + fn type inference).

use crate::ast::{Expr, ExprKind, Span, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};
use crate::typectx::TypeCtx;
use crate::typectx::{T_F16, T_F32, T_F64, T_I8, T_I16, T_I32, T_I64};

use crate::lower::intern_atom;

use super::{
    coerce_numeric, lower_expr_expect, LowerCtx, T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM,
    T_BOOL, T_BYTES, T_DYNAMIC, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

#[derive(Clone, Copy, Debug)]
pub(super) enum ArgConstraint {
    Exact(TypeId),
    Numeric,
    Any,
}

#[derive(Clone, Debug)]
pub(super) struct BuiltinConstraints {
    pub args: Vec<ArgConstraint>,
    pub ret: TypeId,
}

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

pub(super) fn builtin_constraints(
    callee: &Expr,
    type_args: &[Ty],
    args_len: usize,
    expect: Option<TypeId>,
    tc: &mut TypeCtx,
    allow_ambiguous_generics: bool,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    let Some((ns, name)) = builtin_name(callee) else {
        return Ok(None);
    };

    fn err(span: Span, msg: impl Into<String>) -> CompileError {
        CompileError::new(ErrorKind::Type, span, msg)
    }

    let no_targs = |what: &str| {
        if !type_args.is_empty() {
            return Err(err(span, format!("{what} does not take type arguments")));
        }
        Ok(())
    };

    match (ns, name) {
        // System.assert(cond: Bool) -> Bool (throws if cond is false)
        ("System", "assert") => {
            no_targs("assert")?;
            if args_len != 1 {
                return Err(err(span, "assert expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BOOL)],
                ret: T_BOOL,
            }))
        }
        // Integer.to_i8(x: Ix) -> I8
        ("Integer", "to_i8") => {
            no_targs("Integer.to_i8")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I8 }))
        }
        // Integer.to_i16(x: Ix) -> I16
        ("Integer", "to_i16") => {
            no_targs("Integer.to_i16")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I16 }))
        }
        // Integer.to_i32(x: Ix) -> I32
        ("Integer", "to_i32") => {
            no_targs("Integer.to_i32")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I32 }))
        }
        // Integer.to_i64(x: Ix) -> I64
        ("Integer", "to_i64") => {
            no_targs("Integer.to_i64")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_I64 }))
        }
        // Float.to_f16(x: Fx) -> F16
        ("Float", "to_f16") => {
            no_targs("Float.to_f16")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F16 }))
        }
        // Float.to_f32(x: Fx) -> F32
        ("Float", "to_f32") => {
            no_targs("Float.to_f32")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F32 }))
        }
        // Float.to_f64(x: Fx) -> F64
        ("Float", "to_f64") => {
            no_targs("Float.to_f64")?;
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F64 }))
        }
        // Math.sqrt(x: Fx) -> F64
        ("Math", "sqrt") => {
            no_targs("Math.sqrt")?;
            if args_len != 1 {
                return Err(err(span, "Math.sqrt expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Numeric], ret: T_F64 }))
        }
        // Bytes.new(len: I32) -> Bytes
        ("Bytes", "new") => {
            no_targs("Bytes.new")?;
            if args_len != 1 {
                return Err(err(span, "Bytes.new expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_I32)], ret: T_BYTES }))
        }
        // Bytes.len(bytes: Bytes) -> I32
        ("Bytes", "len") => {
            no_targs("Bytes.len")?;
            if args_len != 1 {
                return Err(err(span, "Bytes.len expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_BYTES)], ret: T_I32 }))
        }
        // Bytes.get_u8(bytes: Bytes, index: I32) -> I32
        ("Bytes", "get_u8") => {
            no_targs("Bytes.get_u8")?;
            if args_len != 2 {
                return Err(err(span, "Bytes.get_u8 expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_I32)],
                ret: T_I32,
            }))
        }
        // Bytes.set_u8(bytes: Bytes, index: I32, value: I32) -> Bytes
        ("Bytes", "set_u8") => {
            no_targs("Bytes.set_u8")?;
            if args_len != 3 {
                return Err(err(span, "Bytes.set_u8 expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::Exact(T_BYTES),
                    ArgConstraint::Exact(T_I32),
                    ArgConstraint::Exact(T_I32),
                ],
                ret: T_BYTES,
            }))
        }
        // Bytes.slice(bytes: Bytes, start: I32, len: I32) -> Bytes
        ("Bytes", "slice") => {
            no_targs("Bytes.slice")?;
            if args_len != 3 {
                return Err(err(span, "Bytes.slice expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::Exact(T_BYTES),
                    ArgConstraint::Exact(T_I32),
                    ArgConstraint::Exact(T_I32),
                ],
                ret: T_BYTES,
            }))
        }
        // Bytes.eq(a: Bytes, b: Bytes) -> Bool
        ("Bytes", "eq") => {
            no_targs("Bytes.eq")?;
            if args_len != 2 {
                return Err(err(span, "Bytes.eq expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_BYTES)],
                ret: T_BOOL,
            }))
        }
        // Atom.intern(bytes: Bytes) -> Atom
        ("Atom", "intern") => {
            no_targs("Atom.intern")?;
            if args_len != 1 {
                return Err(err(span, "Atom.intern expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_BYTES)], ret: T_ATOM }))
        }
        // Object.get(obj: Object, key: Atom) -> Dynamic
        ("Object", "get") => {
            if args_len != 2 {
                return Err(err(span, "Object.get expects 2 args"));
            }
            let ret = if type_args.is_empty() {
                expect.unwrap_or(T_DYNAMIC)
            } else if type_args.len() == 1 {
                tc.resolve_ty(&type_args[0])?
            } else {
                return Err(err(span, "Object.get<T>(obj, key): expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_OBJECT), ArgConstraint::Exact(T_ATOM)],
                ret,
            }))
        }
        // Object.set(obj: Object, key: Atom, value: Dynamic) -> Object
        ("Object", "set") => {
            no_targs("Object.set")?;
            if args_len != 3 {
                return Err(err(span, "Object.set expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_OBJECT), ArgConstraint::Exact(T_ATOM), ArgConstraint::Any],
                ret: T_OBJECT,
            }))
        }
        // Array.new(len: I32) -> Array<Dynamic>
        ("Array", "new") => {
            if args_len != 1 {
                return Err(err(span, "Array.new expects 1 arg"));
            }
            let ret = if type_args.is_empty() {
                if allow_ambiguous_generics {
                    expect.unwrap_or(T_DYNAMIC)
                } else {
                    match expect {
                        Some(t) if t == T_ARRAY_I32 || t == T_ARRAY_BYTES => t,
                        _ => return Err(err(span, "Array.new<T>(len): missing type argument")),
                    }
                }
            } else if type_args.len() == 1 {
                let elem = tc.resolve_ty(&type_args[0])?;
                match elem {
                    T_I32 => T_ARRAY_I32,
                    T_BYTES => T_ARRAY_BYTES,
                    _ => {
                        if allow_ambiguous_generics {
                            T_DYNAMIC
                        } else {
                            return Err(err(span, "only Array<I32> and Array<Bytes> supported for now"));
                        }
                    }
                }
            } else {
                return Err(err(span, "Array.new expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Exact(T_I32)], ret }))
        }
        // Array.len(array: Array<Dynamic>) -> I32
        ("Array", "len") => {
            no_targs("Array.len")?;
            if args_len != 1 {
                return Err(err(span, "Array.len expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_I32 }))
        }
        // Array.get(array: Array<Dynamic>, index: I32) -> Dynamic
        ("Array", "get") => {
            no_targs("Array.get")?;
            if args_len != 2 {
                return Err(err(span, "Array.get expects 2 args"));
            }
            // If the array type is known, we can produce a concrete element type; otherwise, Dynamic.
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Exact(T_I32)],
                ret: T_DYNAMIC,
            }))
        }
        // Array.set(array: Array<Dynamic>, index: I32, value: Dynamic) -> Array<Dynamic>
        ("Array", "set") => {
            no_targs("Array.set")?;
            if args_len != 3 {
                return Err(err(span, "Array.set expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Exact(T_I32), ArgConstraint::Any],
                ret: T_DYNAMIC,
            }))
        }
        // List.nil() -> List<Dynamic>
        ("List", "nil") => {
            if args_len != 0 {
                return Err(err(span, "List.nil expects 0 args"));
            }
            let ret = if type_args.is_empty() {
                if allow_ambiguous_generics {
                    expect.unwrap_or(T_DYNAMIC)
                } else {
                    match expect {
                        Some(t) if t == T_LIST_I32 || t == T_LIST_BYTES => t,
                        _ => return Err(err(span, "List.nil<T>(): missing type argument")),
                    }
                }
            } else if type_args.len() == 1 {
                let elem = tc.resolve_ty(&type_args[0])?;
                match elem {
                    T_I32 => T_LIST_I32,
                    T_BYTES => T_LIST_BYTES,
                    _ => {
                        if allow_ambiguous_generics {
                            T_DYNAMIC
                        } else {
                            return Err(err(span, "only List<I32> and List<Bytes> supported for now"));
                        }
                    }
                }
            } else {
                return Err(err(span, "List.nil expects 1 type arg"));
            };
            Ok(Some(BuiltinConstraints { args: vec![], ret }))
        }
        // List.cons(head: Dynamic, tail: List<Dynamic>) -> List<Dynamic>
        ("List", "cons") => {
            if args_len != 2 {
                return Err(err(span, "List.cons expects 2 args"));
            }
            // In strict mode, require T. In ambiguous mode, we can't constrain much.
            if type_args.len() == 1 {
                let elem = tc.resolve_ty(&type_args[0])?;
                let list_tid = match elem {
                    T_I32 => T_LIST_I32,
                    T_BYTES => T_LIST_BYTES,
                    _ => {
                        if allow_ambiguous_generics {
                            return Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any, ArgConstraint::Any], ret: T_DYNAMIC }));
                        }
                        return Err(err(span, "only List<I32> and List<Bytes> supported for now"));
                    }
                };
                return Ok(Some(BuiltinConstraints {
                    args: vec![ArgConstraint::Exact(elem), ArgConstraint::Exact(list_tid)],
                    ret: list_tid,
                }));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Any, ArgConstraint::Any],
                ret: T_DYNAMIC,
            }))
        }
        // List.head(list: List<Dynamic>) -> Dynamic
        ("List", "head") => {
            no_targs("List.head")?;
            if args_len != 1 {
                return Err(err(span, "List.head expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_DYNAMIC }))
        }
        // List.tail(list: List<Dynamic>) -> List<Dynamic>
        ("List", "tail") => {
            no_targs("List.tail")?;
            if args_len != 1 {
                return Err(err(span, "List.tail expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_DYNAMIC }))
        }
        // List.is_nil(list: List<Dynamic>) -> Bool
        ("List", "is_nil") => {
            no_targs("List.is_nil")?;
            if args_len != 1 {
                return Err(err(span, "List.is_nil expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints { args: vec![ArgConstraint::Any], ret: T_BOOL }))
        }
        _ => Ok(None),
    }
}

pub(super) fn try_lower_builtin_call(
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<Option<(VRegId, TypeId)>, CompileError> {
    let Some((ns, name)) = builtin_name(callee) else {
        return Ok(None);
    };

    // Keep lowering logic as the canonical behavioral definition.
    // Signature/constraint inference for `fn_infer` should mirror this behavior.

    // System.assert(cond: Bool) -> Bool (throws if cond is false)
    if ns == "System" && name == "assert" {
        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "assert does not take type arguments",
            ));
        }
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "assert expects 1 arg"));
        }
        let (vcond, tcond) = lower_expr_expect(&args[0], Some(T_BOOL), ctx, b)?;
        if tcond != T_BOOL {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "assert expects Bool"));
        }
        b.emit(e.span, IrOp::Assert { cond: vcond });
        return Ok(Some((vcond, T_BOOL)));
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
            let (v, t) = lower_expr_expect(&args[0], None, ctx, b)?;
            if !super::is_numeric(t) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    args[0].span,
                    "conversion expects a numeric argument",
                ));
            }
            let out = coerce_numeric(e.span, v, t, tid, b).map_err(|_| {
                CompileError::new(
                    ErrorKind::Type,
                    args[0].span,
                    "unsupported numeric conversion",
                )
            })?;
            return Ok(Some((out, tid)));
        }
    }

    // Math.sqrt(x): any numeric -> F64 (native builtin)
    if ns == "Math" && name == "sqrt" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Math.sqrt expects 1 arg"));
        }
        let (v, t) = lower_expr_expect(&args[0], None, ctx, b)?;
        if !super::is_numeric(t) {
            return Err(CompileError::new(
                ErrorKind::Type,
                args[0].span,
                "Math.sqrt expects a numeric argument",
            ));
        }
        let v_f64 = coerce_numeric(e.span, v, t, T_F64, b).map_err(|_| {
            CompileError::new(
                ErrorKind::Type,
                args[0].span,
                "unsupported numeric conversion for Math.sqrt",
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
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.new expects 1 arg"));
        }
        let (vlen, tlen) = lower_expr_expect(&args[0], Some(T_I32), ctx, b)?;
        if tlen != T_I32 {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Bytes.new(i32)"));
        }
        let out = b.new_vreg(T_BYTES);
        b.emit(e.span, IrOp::BytesNew { dst: out, len: vlen });
        return Ok(Some((out, T_BYTES)));
    }
    if ns == "Bytes" && name == "len" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.len expects 1 arg"));
        }
        let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
        if tb != T_BYTES {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.len(bytes)"));
        }
        let out = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::BytesLen { dst: out, bytes: vb });
        return Ok(Some((out, T_I32)));
    }
    if ns == "Bytes" && name == "get_u8" {
        if args.len() != 2 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.get_u8 expects 2 args"));
        }
        let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
        if tb != T_BYTES || ti != T_I32 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.get_u8(bytes, i32)"));
        }
        let out = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::BytesGetU8 { dst: out, bytes: vb, index: vi });
        return Ok(Some((out, T_I32)));
    }
    if ns == "Bytes" && name == "set_u8" {
        if args.len() != 3 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.set_u8 expects 3 args"));
        }
        let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
        let (vv, tv) = lower_expr_expect(&args[2], Some(T_I32), ctx, b)?;
        if tb != T_BYTES || ti != T_I32 || tv != T_I32 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.set_u8(bytes, i32, i32)"));
        }
        b.emit(e.span, IrOp::BytesSetU8 { bytes: vb, index: vi, value: vv });
        return Ok(Some((vb, T_BYTES)));
    }
    if ns == "Bytes" && name == "slice" {
        if args.len() != 3 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.slice expects 3 args"));
        }
        let (vb, tb) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
        let (v_start, t_start) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
        let (v_len, t_len) = lower_expr_expect(&args[2], Some(T_I32), ctx, b)?;
        if tb != T_BYTES || t_start != T_I32 || t_len != T_I32 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.slice(bytes, i32, i32)"));
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
        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "Bytes.eq does not take type arguments",
            ));
        }
        if args.len() != 2 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.eq expects 2 args"));
        }
        let (va, ta) = lower_expr_expect(&args[0], Some(T_BYTES), ctx, b)?;
        let (vb, tb) = lower_expr_expect(&args[1], Some(T_BYTES), ctx, b)?;
        if ta != T_BYTES || tb != T_BYTES {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.eq(bytes, bytes)"));
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
        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "Atom.intern does not take type arguments",
            ));
        }
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Atom.intern expects 1 arg"));
        }
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
        if args.len() != 2 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.get expects 2 args"));
        }
        let out_tid = if type_args.is_empty() {
            T_DYNAMIC
        } else {
            if type_args.len() != 1 {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "Object.get<T>(obj, key): expects 1 type arg",
                ));
            }
            ctx.type_ctx.resolve_ty(&type_args[0])?
        };
        let (v_obj, t_obj) = lower_expr_expect(&args[0], Some(T_OBJECT), ctx, b)?;
        if t_obj != T_OBJECT {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Object.get expects Object"));
        }
        let (v_key, t_key) = lower_expr_expect(&args[1], Some(T_ATOM), ctx, b)?;
        if t_key != T_ATOM {
            return Err(CompileError::new(ErrorKind::Type, args[1].span, "Object.get expects Atom key"));
        }
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ObjGet { dst: out, obj: v_obj, atom: v_key });
        return Ok(Some((out, out_tid)));
    }
    if ns == "Object" && name == "set" {
        if !type_args.is_empty() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "Object.set does not take type arguments",
            ));
        }
        if args.len() != 3 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.set expects 3 args"));
        }
        let (v_obj, t_obj) = lower_expr_expect(&args[0], Some(T_OBJECT), ctx, b)?;
        if t_obj != T_OBJECT {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Object.set expects Object"));
        }
        let (v_key, t_key) = lower_expr_expect(&args[1], Some(T_ATOM), ctx, b)?;
        if t_key != T_ATOM {
            return Err(CompileError::new(ErrorKind::Type, args[1].span, "Object.set expects Atom key"));
        }
        let (v_val, _t_val) = lower_expr_expect(&args[2], None, ctx, b)?;
        b.emit(e.span, IrOp::ObjSet { obj: v_obj, atom: v_key, value: v_val });
        return Ok(Some((v_obj, T_OBJECT)));
    }

    // Array.*
    if ns == "Array" && name == "new" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.new expects 1 arg"));
        }
        let (vlen, tlen) = lower_expr_expect(&args[0], Some(T_I32), ctx, b)?;
        if tlen != T_I32 {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Array.new(i32)"));
        }
        let arr_tid = if ns == "Array" {
            if type_args.is_empty() {
                match expect {
                    Some(t) if t == T_ARRAY_I32 || t == T_ARRAY_BYTES => t,
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "Array.new<T>(len): missing type argument",
                        ))
                    }
                }
            } else if type_args.len() == 1 {
                let elem = ctx.type_ctx.resolve_ty(&type_args[0])?;
                match elem {
                    T_I32 => T_ARRAY_I32,
                    T_BYTES => T_ARRAY_BYTES,
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "only Array<I32> and Array<Bytes> supported for now",
                        ))
                    }
                }
            } else {
                return Err(CompileError::new(ErrorKind::Type, e.span, "Array.new expects 1 type arg"));
            }
        } else {
            T_ARRAY_I32
        };
        let out = b.new_vreg(arr_tid);
        b.emit(e.span, IrOp::ArrayNew { dst: out, len: vlen });
        return Ok(Some((out, arr_tid)));
    }

    if ns == "Array" && name == "len" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.len expects 1 arg"));
        }
        let (va, ta) = lower_expr_expect(&args[0], None, ctx, b)?;
        if ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Array.len(Array<T>)"));
        }
        let out = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ArrayLen { dst: out, arr: va });
        return Ok(Some((out, T_I32)));
    }
    if ns == "Array" && name == "get" {
        if args.len() != 2 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get expects 2 args"));
        }
        let (va, ta) = lower_expr_expect(&args[0], None, ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
        if (ta != T_ARRAY_I32 && ta != T_ARRAY_BYTES) || ti != T_I32 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get(Array<T>, i32)"));
        }
        let out_tid = if ta == T_ARRAY_I32 { T_I32 } else { T_BYTES };
        let out = b.new_vreg(out_tid);
        b.emit(e.span, IrOp::ArrayGet { dst: out, arr: va, index: vi });
        return Ok(Some((out, out_tid)));
    }
    if ns == "Array" && name == "set" {
        if args.len() != 3 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set expects 3 args"));
        }
        let (va, ta) = lower_expr_expect(&args[0], None, ctx, b)?;
        let (vi, ti) = lower_expr_expect(&args[1], Some(T_I32), ctx, b)?;
        let want = if ta == T_ARRAY_I32 {
            T_I32
        } else if ta == T_ARRAY_BYTES {
            T_BYTES
        } else {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set(Array<T>, i32, T)"));
        };
        let (vv, tv) = lower_expr_expect(&args[2], Some(want), ctx, b)?;
        if ti != T_I32 || tv != want {
            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set(Array<T>, i32, T)"));
        }
        b.emit(e.span, IrOp::ArraySet { arr: va, index: vi, value: vv });
        return Ok(Some((va, ta)));
    }

    // List.*
    if ns == "List" && name == "nil" {
        if !args.is_empty() {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.nil expects 0 args"));
        }
        let list_tid = if type_args.is_empty() {
            match expect {
                Some(t) if t == T_LIST_I32 || t == T_LIST_BYTES => t,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "List.nil<T>(): missing type argument",
                    ))
                }
            }
        } else if type_args.len() == 1 {
            let elem = ctx.type_ctx.resolve_ty(&type_args[0])?;
            match elem {
                T_I32 => T_LIST_I32,
                T_BYTES => T_LIST_BYTES,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "only List<I32> and List<Bytes> supported for now",
                    ))
                }
            }
        } else {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.nil expects 1 type arg"));
        };
        let out = b.new_vreg(list_tid);
        b.emit(e.span, IrOp::ListNil { dst: out });
        return Ok(Some((out, list_tid)));
    }
    if ns == "List" && name == "cons" {
        if args.len() != 2 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons expects 2 args"));
        }
        let list_tid = if type_args.is_empty() {
            match expect {
                Some(t) if t == T_LIST_I32 || t == T_LIST_BYTES => t,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "List.cons<T>(head, tail): missing type argument",
                    ))
                }
            }
        } else if type_args.len() == 1 {
            let elem = ctx.type_ctx.resolve_ty(&type_args[0])?;
            match elem {
                T_I32 => T_LIST_I32,
                T_BYTES => T_LIST_BYTES,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "only List<I32> and List<Bytes> supported for now",
                    ))
                }
            }
        } else {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons expects 1 type arg"));
        };
        let elem_tid = super::elem_tid_for_list(list_tid).expect("list tid");
        let (vhead, thead) = lower_expr_expect(&args[0], Some(elem_tid), ctx, b)?;
        let (vtail, ttail) = lower_expr_expect(&args[1], Some(list_tid), ctx, b)?;
        if thead != elem_tid || ttail != list_tid {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons<T>(T, List<T>)"));
        }
        let out = b.new_vreg(list_tid);
        b.emit(e.span, IrOp::ListCons { dst: out, head: vhead, tail: vtail });
        return Ok(Some((out, list_tid)));
    }
    if ns == "List" && name == "head" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.head expects 1 arg"));
        }
        let (vl, tl) = lower_expr_expect(&args[0], None, ctx, b)?;
        let elem_tid = super::elem_tid_for_list(tl)
            .ok_or_else(|| CompileError::new(ErrorKind::Type, args[0].span, "List.head(List<T>)"))?;
        let out = b.new_vreg(elem_tid);
        b.emit(e.span, IrOp::ListHead { dst: out, list: vl });
        return Ok(Some((out, elem_tid)));
    }
    if ns == "List" && name == "tail" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.tail expects 1 arg"));
        }
        let (vl, tl) = lower_expr_expect(&args[0], None, ctx, b)?;
        if tl != T_LIST_I32 && tl != T_LIST_BYTES {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "List.tail(List<T>)"));
        }
        let out = b.new_vreg(tl);
        b.emit(e.span, IrOp::ListTail { dst: out, list: vl });
        return Ok(Some((out, tl)));
    }
    if ns == "List" && name == "is_nil" {
        if args.len() != 1 {
            return Err(CompileError::new(ErrorKind::Type, e.span, "List.is_nil expects 1 arg"));
        }
        let (vl, tl) = lower_expr_expect(&args[0], None, ctx, b)?;
        if tl != T_LIST_I32 && tl != T_LIST_BYTES {
            return Err(CompileError::new(ErrorKind::Type, args[0].span, "List.is_nil(List<T>)"));
        }
        let out = b.new_vreg(T_BOOL);
        b.emit(e.span, IrOp::ListIsNil { dst: out, list: vl });
        return Ok(Some((out, T_BOOL)));
    }

    Ok(None)
}

