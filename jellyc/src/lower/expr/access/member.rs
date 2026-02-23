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

use super::super::*;
use crate::ir::IrOp;

pub(super) fn lower_member_expr(
    e: &Expr,
    base: &Expr,
    name: &str,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Namespaces must be used as call targets (handled in ExprKind::Call builtin logic).
    if let ExprKind::Var(ns) = &base.node {
        if matches!(
            ns.as_str(),
            "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
        ) {
            // Allow shadowing (e.g. `import foo as Bytes`), where `Bytes` is a normal binding.
            let shadowed = ctx.env_stack.iter().rev().any(|m| m.contains_key(ns))
                || ctx.module_alias_exports.contains_key(ns);
            if !shadowed {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "namespace members must be called (e.g. Bytes.len(x))",
                ));
            }
        }
    }

    let (v_obj, t_obj) = lower_expr(base, ctx, b)?;
    let te =
        ctx.type_ctx.types.get(t_obj as usize).ok_or_else(|| {
            CompileError::new(ErrorKind::Internal, e.span, "bad member base type id")
        })?;
    if te.kind != crate::jlyb::TypeKind::Object {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "member access currently only supported for Object (obj.field)",
        ));
    }

    let atom_id = intern_atom(name, ctx);

    // If this is a module namespace object, prefer the module's declared export type.
    if let ExprKind::Var(alias) = &base.node {
        if let Some(exports) = ctx.module_alias_exports.get(alias) {
            let out_tid = expect.ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    e.span,
                    "missing semantic type for module member access",
                )
            })?;
            if let Some(exp_tid) = exports.get(name).copied() {
                if exp_tid != out_tid && out_tid != T_DYNAMIC {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        e.span,
                        "semantic type mismatch for module export member access",
                    ));
                }
            }
            let out = b.new_vreg(out_tid);
            b.emit(
                e.span,
                IrOp::ObjGetAtom {
                    dst: out,
                    obj: v_obj,
                    atom_id,
                },
            );
            if let Some(et) = expect {
                if et == out_tid {
                    return Ok((out, out_tid));
                }
                if et == T_DYNAMIC && out_tid != T_DYNAMIC {
                    let vd = b.new_vreg(T_DYNAMIC);
                    b.emit(e.span, IrOp::ToDyn { dst: vd, src: out });
                    return Ok((vd, T_DYNAMIC));
                }
                if out_tid == T_DYNAMIC {
                    return Ok((out, T_DYNAMIC));
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "module export type mismatch",
                ));
            }
            return Ok((out, out_tid));
        }
    }

    if ctx.type_ctx.is_tuple_type(t_obj) {
        // Tuple element access: `t.0`, `t.1`, ...
        let idx: usize = name.parse().map_err(|_| {
            CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple element access must be .<index>",
            )
        })?;
        let elems = ctx
            .type_ctx
            .tuple_elems(t_obj)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad tuple type"))?;
        if idx >= elems.len() {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple index out of range",
            ));
        }
        let elem_tid = elems[idx];
        let out = b.new_vreg(elem_tid);
        b.emit(
            e.span,
            IrOp::ObjGetAtom {
                dst: out,
                obj: v_obj,
                atom_id,
            },
        );
        if let Some(et) = expect {
            if et == elem_tid {
                return Ok((out, elem_tid));
            }
            if et == T_DYNAMIC {
                let vd = b.new_vreg(T_DYNAMIC);
                b.emit(e.span, IrOp::ToDyn { dst: vd, src: out });
                return Ok((vd, T_DYNAMIC));
            }
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "tuple element type mismatch",
            ));
        }
        Ok((out, elem_tid))
    } else {
        // Method extraction binding (first attempt):
        //
        // If the surrounding context expects a function type, interpret `obj.m` as
        // extracting a method value *bound to* `obj` (so calling it later preserves `this`).
        //
        // This mirrors the call-site sugar in `lower/expr/call.rs` but applies when the
        // member is used as a first-class value.
        if let Some(bound_fun_tid) = expect {
            let te = ctx
                .type_ctx
                .types
                .get(bound_fun_tid as usize)
                .ok_or_else(|| {
                    CompileError::new(ErrorKind::Internal, e.span, "bad expected type id")
                })?;
            if te.kind == crate::jlyb::TypeKind::Function {
                let sig_id = te.p0;
                let sig = ctx
                    .type_ctx
                    .sigs
                    .get(sig_id as usize)
                    .ok_or_else(|| {
                        CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id")
                    })?
                    .clone();

                // Unbound method signature: (Object, A...) -> R
                let mut unbound_args: Vec<TypeId> = Vec::with_capacity(1 + sig.args.len());
                unbound_args.push(T_OBJECT);
                unbound_args.extend(sig.args.iter().copied());
                let unbound_sig_id = ctx.type_ctx.intern_sig(sig.ret_type, &unbound_args);
                let unbound_fun_tid = ctx.type_ctx.intern_fun_type(sig.ret_type, &unbound_args);
                let _ = unbound_sig_id; // documented by type table; call sites will use bound signature

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
                b.emit(
                    e.span,
                    IrOp::BindThis {
                        dst: v_bound,
                        func: v_unbound,
                        this: v_obj,
                    },
                );
                return Ok((v_bound, bound_fun_tid));
            }
        }

        // Object property access always yields Dynamic (boxed); convert if expect is typed.
        // Emit ObjGetAtom directly to typed dst when possible to avoid tmp+FromDyn* reg-alloc
        // issues (VM's vm_store_from_boxed unboxes based on reg_types[dst]).
        let out_tid = expect.ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "missing semantic type for object property access",
            )
        })?;
        if out_tid == T_DYNAMIC {
            let tmp = b.new_vreg(T_DYNAMIC);
            b.emit(
                e.span,
                IrOp::ObjGetAtom {
                    dst: tmp,
                    obj: v_obj,
                    atom_id,
                },
            );
            Ok((tmp, T_DYNAMIC))
        } else if is_numeric(out_tid)
            || out_tid == T_BOOL
            || out_tid == T_BYTES
            || out_tid == T_OBJECT
            || out_tid == T_ARRAY_I32
            || out_tid == T_ARRAY_BYTES
            || out_tid == T_LIST_I32
            || out_tid == T_LIST_BYTES
            || is_object_kind(&ctx.type_ctx, out_tid)
        {
            let out = b.new_vreg(out_tid);
            b.emit(
                e.span,
                IrOp::ObjGetAtom {
                    dst: out,
                    obj: v_obj,
                    atom_id,
                },
            );
            Ok((out, out_tid))
        } else {
            Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "object property cannot produce this type",
            ))
        }
    }
}
