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
use crate::typectx::{T_F16, T_F32, T_F64, T_I8, T_I16, T_I32, T_I64};
use super::{
    T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_OBJECT, T_ARRAY_I32, T_ARRAY_BYTES,
    T_LIST_I32, T_LIST_BYTES,
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
        let builtin = match &callee.node {
            ExprKind::Var(n) => Some(("".to_string(), n.clone())),
            ExprKind::Member { base, name } => match &base.node {
                ExprKind::Var(ns) => Some((ns.clone(), name.clone())),
                _ => None,
            },
            _ => None,
        };

        if let Some((ns, name)) = builtin {
            let ns = ns.as_str();
            let name = name.as_str();

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
                return Ok((vcond, T_BOOL));
            }

            // Numeric conversion: Integer.to_i8, Integer.to_i16, Integer.to_i32, Integer.to_i64,
            // Float.to_f16, Float.to_f32, Float.to_f64. Accept any numeric type and convert accordingly.
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
                    return Ok((out, tid));
                }
            }

            // Math.sqrt(x): any numeric -> F64 (prelude builtin)
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
                return Ok((out, T_F64));
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
                return Ok((out, T_BYTES));
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
                return Ok((out, T_I32));
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
                return Ok((out, T_I32));
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
                return Ok((vb, T_BYTES));
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
                return Ok((out, T_BYTES));
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
                return Ok((out, T_BOOL));
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
                return Ok((out, T_ATOM));
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
                return Ok((out, out_tid));
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
                // Return the object for convenience.
                return Ok((v_obj, T_OBJECT));
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
                return Ok((out, arr_tid));
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
                return Ok((out, T_I32));
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
                return Ok((out, out_tid));
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
                return Ok((va, ta));
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
                return Ok((out, list_tid));
            }

            if ns == "List" && name == "cons" {
                if args.len() != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "List.cons expects 2 args"));
                }
                // Determine list type from explicit type arg or expected type.
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
                return Ok((out, list_tid));
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
                return Ok((out, elem_tid));
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
                return Ok((out, tl));
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
                return Ok((out, T_BOOL));
            }
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

                    // If we know this export's declared function type, enforce it; otherwise,
                    // infer a best-effort function type from the call site.
                    let maybe_tid = ctx
                        .module_alias_exports
                        .get(alias)
                        .and_then(|m| m.get(name))
                        .copied();

                    let mut inferred_args: Option<Vec<(VRegId, TypeId)>> = None;
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
                        // Infer a signature (Dynamic return by default).
                        let mut arg_tids: Vec<TypeId> = Vec::with_capacity(args.len());
                        let mut arg_vals: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
                        for a in args.iter() {
                            let (v, t) = lower_expr_expect(a, None, ctx, b)?;
                            arg_vals.push((v, t));
                            arg_tids.push(t);
                        }
                        inferred_args = Some(arg_vals);
                        let ret_tid = expect.unwrap_or(T_DYNAMIC);
                        let sig_id = ctx.type_ctx.intern_sig(ret_tid, &arg_tids);
                        let fun_tid = ctx.type_ctx.intern_fun_type(ret_tid, &arg_tids);

                        // Reuse the already-lowered args below.
                        // (We return them via a dummy vector in place of sig_args.)
                        // Caller will ignore sig_args for inferred path.
                        (fun_tid, sig_id, arg_tids, ret_tid)
                    };

                    // Evaluate args left-to-right with expected types when available.
                    let arg_vals: Vec<(VRegId, TypeId)> = if let Some(v) = inferred_args.take() {
                        v
                    } else {
                        let mut out: Vec<(VRegId, TypeId)> = Vec::with_capacity(args.len());
                        for (i, a) in args.iter().enumerate() {
                            let et = sig_args.get(i).copied();
                            out.push(lower_expr_expect(a, et, ctx, b)?);
                        }
                        out
                    };

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
