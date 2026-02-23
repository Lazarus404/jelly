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

use crate::ast::{Expr, ExprKind, Ty};
use crate::builtin_constraints;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_ATOM, T_DYNAMIC, T_OBJECT};

use super::super::TypeChecker;

pub(super) fn try_type_builtin_like_call(
    tc: &mut TypeChecker,
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    expect: Option<TypeId>,
    shadowed_ns: bool,
) -> Result<Option<TypeId>, CompileError> {
    if shadowed_ns {
        return Ok(None);
    }

    if let Some(ret) = try_type_array_or_list_constraints(tc, e, callee, type_args, args)? {
        return Ok(Some(ret));
    }

    if let Some(ret) = try_type_object_set(tc, e, callee, type_args, args, expect)? {
        return Ok(Some(ret));
    }

    if let Some(ret) = try_type_generic_builtin_constraints(tc, e, callee, type_args, args, expect)?
    {
        return Ok(Some(ret));
    }

    Ok(None)
}

fn try_type_array_or_list_constraints(
    tc: &mut TypeChecker,
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
) -> Result<Option<TypeId>, CompileError> {
    if let ExprKind::Member { base, name } = &callee.node {
        if let ExprKind::Var(ns) = &base.node {
            if ns == "Array" && matches!(name.as_str(), "len" | "get" | "set") {
                if !type_args.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        format!("Array.{} does not take type arguments", name),
                    ));
                }
                // Determine the array type from arg0, then typecheck the rest using centralized rules.
                let t_arr = args
                    .get(0)
                    .ok_or_else(|| {
                        CompileError::new(ErrorKind::Type, e.span, "Array.* expects at least 1 arg")
                    })
                    .and_then(|a0| tc.check_expr(a0, None))?;
                if let Some(cs) = builtin_constraints::array_constraints_from_arr_tid(
                    name,
                    args.len(),
                    t_arr,
                    e.span,
                )? {
                    for (i, a) in args.iter().enumerate() {
                        let et = match cs
                            .args
                            .get(i)
                            .copied()
                            .unwrap_or(builtin_constraints::ArgConstraint::Any)
                        {
                            builtin_constraints::ArgConstraint::Exact(tid) => Some(tid),
                            _ => None,
                        };
                        let _ = tc.check_expr(a, et)?;
                    }
                    let arg_tids: Vec<TypeId> = cs
                        .args
                        .iter()
                        .map(|c| match c {
                            builtin_constraints::ArgConstraint::Exact(tid) => *tid,
                            _ => T_DYNAMIC,
                        })
                        .collect();
                    let fun_tid = tc.intern_fun_type(cs.ret, &arg_tids);
                    tc.record_expr(callee.span, fun_tid);
                    return Ok(Some(cs.ret));
                }
            }

            if ns == "List" && matches!(name.as_str(), "head" | "tail" | "is_nil") {
                if !type_args.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        format!("List.{} does not take type arguments", name),
                    ));
                }
                let t_list = args
                    .get(0)
                    .ok_or_else(|| {
                        CompileError::new(ErrorKind::Type, e.span, "List.* expects 1 arg")
                    })
                    .and_then(|a0| tc.check_expr(a0, None))?;
                if let Some(cs) = builtin_constraints::list_constraints_from_list_tid(
                    name,
                    args.len(),
                    t_list,
                    e.span,
                )? {
                    for (i, a) in args.iter().enumerate() {
                        let et = match cs
                            .args
                            .get(i)
                            .copied()
                            .unwrap_or(builtin_constraints::ArgConstraint::Any)
                        {
                            builtin_constraints::ArgConstraint::Exact(tid) => Some(tid),
                            _ => None,
                        };
                        let _ = tc.check_expr(a, et)?;
                    }
                    let arg_tids: Vec<TypeId> = cs
                        .args
                        .iter()
                        .map(|c| match c {
                            builtin_constraints::ArgConstraint::Exact(tid) => *tid,
                            _ => T_DYNAMIC,
                        })
                        .collect();
                    let fun_tid = tc.intern_fun_type(cs.ret, &arg_tids);
                    tc.record_expr(callee.span, fun_tid);
                    return Ok(Some(cs.ret));
                }
            }
        }
    }
    Ok(None)
}

fn try_type_object_set(
    tc: &mut TypeChecker,
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<Option<TypeId>, CompileError> {
    // Object.set(obj, key, value) returns the receiver's (possibly nominal) object kind.
    if let ExprKind::Member { base, name } = &callee.node {
        if let ExprKind::Var(ns) = &base.node {
            if ns == "Object" && name == "set" {
                if !type_args.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "Object.set does not take type arguments",
                    ));
                }
                if args.len() != 3 {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "Object.set expects 3 args",
                    ));
                }

                let recv_expect =
                    expect.filter(|&et| tc.is_object_kind(et) && !tc.is_tuple_type(et));
                let t_obj = if let Some(et) = recv_expect {
                    let t = tc.check_expr(&args[0], Some(et))?;
                    if t != et {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            args[0].span,
                            "Object.set receiver type mismatch",
                        ));
                    }
                    et
                } else {
                    let t0 = tc.check_expr(&args[0], None)?;
                    if t0 == T_DYNAMIC {
                        tc.check_expr(&args[0], Some(T_OBJECT))?
                    } else {
                        t0
                    }
                };

                if !tc.is_object_kind(t_obj) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        args[0].span,
                        "Object.set expects Object",
                    ));
                }

                let _ = tc.check_expr(&args[1], Some(T_ATOM))?;
                let _ = tc.check_expr(&args[2], None)?;

                // Best-effort callee type for dumps.
                let fun_tid = tc.intern_fun_type(t_obj, &[t_obj, T_ATOM, T_DYNAMIC]);
                tc.record_expr(callee.span, fun_tid);
                return Ok(Some(t_obj));
            }
        }
    }
    Ok(None)
}

fn try_type_generic_builtin_constraints(
    tc: &mut TypeChecker,
    e: &Expr,
    callee: &Expr,
    type_args: &[Ty],
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<Option<TypeId>, CompileError> {
    if let Some(cs) = builtin_constraints::builtin_constraints(
        callee,
        type_args,
        args.len(),
        expect,
        tc.type_ctx_mut(),
        true,
        e.span,
    )? {
        for (i, a) in args.iter().enumerate() {
            match cs
                .args
                .get(i)
                .copied()
                .unwrap_or(builtin_constraints::ArgConstraint::Any)
            {
                builtin_constraints::ArgConstraint::Exact(tid) => {
                    let _ = tc.check_expr(a, Some(tid))?;
                }
                builtin_constraints::ArgConstraint::ObjectKind => {
                    let t0 = tc.check_expr(a, None)?;
                    let t = if t0 == T_DYNAMIC {
                        tc.check_expr(a, Some(T_OBJECT))?
                    } else {
                        t0
                    };
                    if !tc.is_object_kind(t) {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            a.span,
                            "builtin expects an Object",
                        ));
                    }
                }
                builtin_constraints::ArgConstraint::Numeric => {
                    let t = tc.check_expr(a, None)?;
                    if !super::is_numeric_tid(t) {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            a.span,
                            "builtin expects numeric",
                        ));
                    }
                }
                builtin_constraints::ArgConstraint::Any => {
                    let _ = tc.check_expr(a, None)?;
                }
            }
        }
        // Callee type (for dumps) is a best-effort fun type.
        let arg_tids: Vec<TypeId> = cs
            .args
            .iter()
            .map(|c| match c {
                builtin_constraints::ArgConstraint::Exact(tid) => *tid,
                builtin_constraints::ArgConstraint::ObjectKind => T_OBJECT,
                _ => T_DYNAMIC,
            })
            .collect();
        let fun_tid = tc.intern_fun_type(cs.ret, &arg_tids);
        tc.record_expr(callee.span, fun_tid);
        return Ok(Some(cs.ret));
    }
    Ok(None)
}
