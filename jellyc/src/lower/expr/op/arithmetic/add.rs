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

use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};
use crate::jlyb::{NATIVE_BUILTIN_F64_TO_BYTES, NATIVE_BUILTIN_I32_TO_BYTES};

use super::super::super::{
    coerce_numeric, is_numeric, lower_expr, lower_expr_expect, LowerCtx, T_ARRAY_BYTES, T_BYTES,
    T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8,
};

pub(super) fn lower_add_expr(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    // Special-case bytes concatenation: flatten long chains into concat_many(Array<bytes>)
    // to match the AST backend behavior.
    fn collect_add_terms<'e>(e: &'e Expr, out: &mut Vec<&'e Expr>) {
        match &e.node {
            ExprKind::Add(x, y) => {
                collect_add_terms(x, out);
                collect_add_terms(y, out);
            }
            _ => out.push(e),
        }
    }

    let mut terms: Vec<&Expr> = Vec::new();
    collect_add_terms(e, &mut terms);
    if terms.len() == 2 {
        // When one side is Member and types will mismatch, lower only the other side first
        // then lower_expr_expect the Member. Avoids redundant ObjGetAtom that clobbers
        // the object register before the second ObjGetAtom reads it.
        let (va, ta, vb, tb) = match (&terms[0].node, &terms[1].node) {
            (ExprKind::Member { .. }, _) => {
                let (vb, tb) = lower_expr(terms[1], ctx, b)?;
                let (va, ta) = lower_expr_expect(terms[0], ctx, b)?;
                (va, ta, vb, tb)
            }
            (_, ExprKind::Member { .. }) => {
                let (va, ta) = lower_expr(terms[0], ctx, b)?;
                let (vb, tb) = lower_expr_expect(terms[1], ctx, b)?;
                (va, ta, vb, tb)
            }
            _ => {
                let (va, ta) = lower_expr(terms[0], ctx, b)?;
                let (vb, tb) = lower_expr(terms[1], ctx, b)?;
                (va, ta, vb, tb)
            }
        };
        if ta == T_BYTES && tb == T_BYTES {
            let out = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::BytesConcat2 {
                    dst: out,
                    a: va,
                    b: vb,
                },
            );
            return Ok((out, T_BYTES));
        }
        if is_numeric(ta) && is_numeric(tb) {
            let (va2, _ta2, vb2, _tb2, out_t) =
                super::super::promote_numeric_bin(e.span, va, ta, vb, tb, "+", expect, b)?;
            let out = b.new_vreg(out_t);
            // 0 + rhs or lhs + 0: emit Mov directly to avoid AddI32/peephole issues with ObjGetAtom
            let emit_add = match (&terms[0].node, &terms[1].node) {
                (ExprKind::I32Lit(0), _) => {
                    b.emit(e.span, IrOp::Mov { dst: out, src: vb2 });
                    false
                }
                (_, ExprKind::I32Lit(0)) => {
                    b.emit(e.span, IrOp::Mov { dst: out, src: va2 });
                    false
                }
                _ => true,
            };
            if emit_add {
                match out_t {
                    T_I8 | T_I16 | T_I32 => b.emit(
                        e.span,
                        IrOp::AddI32 {
                            dst: out,
                            a: va2,
                            b: vb2,
                        },
                    ),
                    T_I64 => b.emit(
                        e.span,
                        IrOp::AddI64 {
                            dst: out,
                            a: va2,
                            b: vb2,
                        },
                    ),
                    T_F16 => b.emit(
                        e.span,
                        IrOp::AddF16 {
                            dst: out,
                            a: va2,
                            b: vb2,
                        },
                    ),
                    T_F32 => b.emit(
                        e.span,
                        IrOp::AddF32 {
                            dst: out,
                            a: va2,
                            b: vb2,
                        },
                    ),
                    T_F64 => b.emit(
                        e.span,
                        IrOp::AddF64 {
                            dst: out,
                            a: va2,
                            b: vb2,
                        },
                    ),
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Internal,
                            e.span,
                            "bad numeric promotion",
                        ))
                    }
                }
            }
            return Ok((out, out_t));
        }
        // bytes + numeric: convert numeric to bytes via I32.to_bytes (int) or F64.to_bytes (float), then concat
        if ta == T_BYTES && is_numeric(tb) {
            let (v_arg, t_arg, to_bytes) = if matches!(tb, T_F16 | T_F32 | T_F64) {
                let v_f64 = coerce_numeric(terms[1].span, vb, tb, T_F64, b)?;
                (v_f64, T_F64, NATIVE_BUILTIN_F64_TO_BYTES)
            } else {
                let v_i32 = coerce_numeric(terms[1].span, vb, tb, T_I32, b)?;
                (v_i32, T_I32, NATIVE_BUILTIN_I32_TO_BYTES)
            };
            let sig_args = [t_arg];
            let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
            let vcallee = b.new_vreg(ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args));
            b.emit(
                e.span,
                IrOp::ConstFun {
                    dst: vcallee,
                    func_index: to_bytes,
                },
            );
            let vb_bytes = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::Call {
                    dst: vb_bytes,
                    callee: vcallee,
                    sig_id,
                    arg_base: v_arg,
                    nargs: 1,
                },
            );
            let out = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::BytesConcat2 {
                    dst: out,
                    a: va,
                    b: vb_bytes,
                },
            );
            return Ok((out, T_BYTES));
        }
        if tb == T_BYTES && is_numeric(ta) {
            let (v_arg, t_arg, to_bytes) = if matches!(ta, T_F16 | T_F32 | T_F64) {
                let v_f64 = coerce_numeric(terms[0].span, va, ta, T_F64, b)?;
                (v_f64, T_F64, NATIVE_BUILTIN_F64_TO_BYTES)
            } else {
                let v_i32 = coerce_numeric(terms[0].span, va, ta, T_I32, b)?;
                (v_i32, T_I32, NATIVE_BUILTIN_I32_TO_BYTES)
            };
            let sig_args = [t_arg];
            let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
            let vcallee = b.new_vreg(ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args));
            b.emit(
                e.span,
                IrOp::ConstFun {
                    dst: vcallee,
                    func_index: to_bytes,
                },
            );
            let va_bytes = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::Call {
                    dst: va_bytes,
                    callee: vcallee,
                    sig_id,
                    arg_base: v_arg,
                    nargs: 1,
                },
            );
            let out = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::BytesConcat2 {
                    dst: out,
                    a: va_bytes,
                    b: vb,
                },
            );
            return Ok((out, T_BYTES));
        }
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "'+' expects bytes or numeric operands",
        ));
    }

    // Lower a flattened add chain mechanically from the semantic result type.
    let out_t = expect.ok_or_else(|| {
        CompileError::new(
            ErrorKind::Internal,
            e.span,
            "missing semantic type for '+' expression",
        )
    })?;

    if out_t == T_BYTES {
        // Lower each term and group by type. Consecutive numeric terms are added first, then
        // converted to bytes (so "result: " + a + b + c produces "result: 12.6" not "result: 1.11.21.3").
        let lowered: Vec<(VRegId, TypeId)> = terms
            .iter()
            .map(|t| lower_expr(t, ctx, b))
            .collect::<Result<Vec<_>, _>>()?;

        let mut parts: Vec<VRegId> = Vec::with_capacity(terms.len());
        let mut i = 0;
        while i < terms.len() {
            let (v, tt) = &lowered[i];
            if *tt == T_BYTES {
                parts.push(*v);
                i += 1;
            } else if is_numeric(*tt) {
                // Collect consecutive numeric terms and add them before converting to bytes.
                let mut numeric_vs: Vec<(VRegId, TypeId)> = vec![(*v, *tt)];
                i += 1;
                while i < terms.len() {
                    let (v2, tt2) = &lowered[i];
                    if !is_numeric(*tt2) {
                        break;
                    }
                    numeric_vs.push((*v2, *tt2));
                    i += 1;
                }
                // Find widest numeric type and add all terms.
                let out_num_t = numeric_vs
                    .iter()
                    .map(|(_, t)| *t)
                    .reduce(super::super::super::numeric::join_numeric)
                    .unwrap_or(T_I32);
                let coerced: Vec<VRegId> = numeric_vs
                    .iter()
                    .map(|(v, t)| coerce_numeric(e.span, *v, *t, out_num_t, b))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut acc = coerced[0];
                for rhs in &coerced[1..] {
                    let out_reg = b.new_vreg(out_num_t);
                    match out_num_t {
                        T_I8 | T_I16 | T_I32 => b.emit(
                            e.span,
                            IrOp::AddI32 {
                                dst: out_reg,
                                a: acc,
                                b: *rhs,
                            },
                        ),
                        T_I64 => b.emit(
                            e.span,
                            IrOp::AddI64 {
                                dst: out_reg,
                                a: acc,
                                b: *rhs,
                            },
                        ),
                        T_F16 => b.emit(
                            e.span,
                            IrOp::AddF16 {
                                dst: out_reg,
                                a: acc,
                                b: *rhs,
                            },
                        ),
                        T_F32 => b.emit(
                            e.span,
                            IrOp::AddF32 {
                                dst: out_reg,
                                a: acc,
                                b: *rhs,
                            },
                        ),
                        T_F64 => b.emit(
                            e.span,
                            IrOp::AddF64 {
                                dst: out_reg,
                                a: acc,
                                b: *rhs,
                            },
                        ),
                        _ => {
                            return Err(CompileError::new(
                                ErrorKind::Internal,
                                e.span,
                                "bad numeric type in bytes add",
                            ))
                        }
                    }
                    acc = out_reg;
                }
                let (v_arg, t_arg, to_bytes) = if matches!(out_num_t, T_F16 | T_F32 | T_F64) {
                    let v_f64 = coerce_numeric(e.span, acc, out_num_t, T_F64, b)?;
                    (v_f64, T_F64, NATIVE_BUILTIN_F64_TO_BYTES)
                } else {
                    let v_i32 = coerce_numeric(e.span, acc, out_num_t, T_I32, b)?;
                    (v_i32, T_I32, NATIVE_BUILTIN_I32_TO_BYTES)
                };
                let sig_args = [t_arg];
                let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);
                let vcallee = b.new_vreg(ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args));
                b.emit(
                    e.span,
                    IrOp::ConstFun {
                        dst: vcallee,
                        func_index: to_bytes,
                    },
                );
                let vb = b.new_vreg(T_BYTES);
                b.emit(
                    e.span,
                    IrOp::Call {
                        dst: vb,
                        callee: vcallee,
                        sig_id,
                        arg_base: v_arg,
                        nargs: 1,
                    },
                );
                parts.push(vb);
            } else {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    terms[i].span,
                    "'+' expects bytes or numeric operands",
                ));
            }
        }

        let n = parts.len() as i32;
        let v_n = b.new_vreg(T_I32);
        b.emit(e.span, IrOp::ConstI32 { dst: v_n, imm: n });

        let v_arr = b.new_vreg(T_ARRAY_BYTES);
        b.emit(
            e.span,
            IrOp::ArrayNew {
                dst: v_arr,
                len: v_n,
            },
        );

        for (i, v_part) in parts.iter().enumerate() {
            let v_i = b.new_vreg(T_I32);
            b.emit(
                e.span,
                IrOp::ConstI32 {
                    dst: v_i,
                    imm: i as i32,
                },
            );
            b.emit(
                e.span,
                IrOp::ArraySet {
                    arr: v_arr,
                    index: v_i,
                    value: *v_part,
                },
            );
        }

        let out = b.new_vreg(T_BYTES);
        b.emit(
            e.span,
            IrOp::BytesConcatMany {
                dst: out,
                parts: v_arr,
            },
        );
        return Ok((out, T_BYTES));
    }

    if is_numeric(out_t) {
        // Promote each term to the semantic result type, then fold left-to-right.
        let mut vs: Vec<VRegId> = Vec::with_capacity(terms.len());
        for t in &terms {
            let (v, tt) = lower_expr(t, ctx, b)?;
            if !is_numeric(tt) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    t.span,
                    "'+' expects numeric operands",
                ));
            }
            vs.push(coerce_numeric(e.span, v, tt, out_t, b)?);
        }

        let mut acc = vs[0];
        for rhs in &vs[1..] {
            let out = b.new_vreg(out_t);
            match out_t {
                T_I8 | T_I16 | T_I32 => b.emit(
                    e.span,
                    IrOp::AddI32 {
                        dst: out,
                        a: acc,
                        b: *rhs,
                    },
                ),
                T_I64 => b.emit(
                    e.span,
                    IrOp::AddI64 {
                        dst: out,
                        a: acc,
                        b: *rhs,
                    },
                ),
                T_F16 => b.emit(
                    e.span,
                    IrOp::AddF16 {
                        dst: out,
                        a: acc,
                        b: *rhs,
                    },
                ),
                T_F32 => b.emit(
                    e.span,
                    IrOp::AddF32 {
                        dst: out,
                        a: acc,
                        b: *rhs,
                    },
                ),
                T_F64 => b.emit(
                    e.span,
                    IrOp::AddF64 {
                        dst: out,
                        a: acc,
                        b: *rhs,
                    },
                ),
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        e.span,
                        "bad numeric add type",
                    ))
                }
            }
            acc = out;
        }
        return Ok((acc, out_t));
    }

    Err(CompileError::new(
        ErrorKind::Type,
        e.span,
        "'+' not supported for this type yet",
    ))
}
