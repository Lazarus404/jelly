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

mod call;
mod control;
mod fn_;
pub(super) mod fn_infer;
mod lit;
mod r#match;
mod new_;

use crate::ast::{Expr, ExprKind, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use super::{
    ensure_capture_binding, intern_atom, is_object_kind, lookup_var, LowerCtx,
    T_BYTES, T_BOOL, T_I8, T_I16, T_I32, T_I64, T_F16, T_F32, T_F64, T_DYNAMIC, T_ARRAY_I32, T_ARRAY_BYTES, T_OBJECT, T_LIST_I32, T_LIST_BYTES, T_ATOM,
};

/// Convert f32 to IEEE 754 binary16 bits (matches vm_f32_to_f16_bits in reg.c).
fn f32_to_f16_bits(f: f32) -> u16 {
    let u32_bits = f.to_bits();
    let sign = (u32_bits >> 16) & 0x8000;
    let exp = (u32_bits >> 23) & 0xFF;
    let mant = u32_bits & 0x7FFFFF;
    if exp == 0xFF {
        return (sign | 0x7C00 | if mant != 0 { 0x200 } else { 0 }) as u16;
    }
    if exp == 0 && mant == 0 {
        return sign as u16;
    }
    let exp16 = (exp as i32) - 127 + 15;
    if exp16 >= 31 {
        return (sign | 0x7C00) as u16;
    }
    if exp16 <= 0 {
        return sign as u16;
    }
    (sign | ((exp16 as u32) << 10) | (mant >> 13)) as u16
}

fn elem_tid_for_array(arr_tid: TypeId) -> Option<TypeId> {
    match arr_tid {
        T_ARRAY_I32 => Some(T_I32),
        T_ARRAY_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn elem_tid_for_list(list_tid: TypeId) -> Option<TypeId> {
    match list_tid {
        T_LIST_I32 => Some(T_I32),
        T_LIST_BYTES => Some(T_BYTES),
        _ => None,
    }
}

fn numeric_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        T_F16 => 4,
        T_F32 => 5,
        T_F64 => 6,
        _ => 255,
    }
}

pub(super) fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

pub(super) fn type_name(tid: TypeId) -> &'static str {
    match tid {
        T_I8 => "I8",
        T_I16 => "I16",
        T_I32 => "I32",
        T_I64 => "I64",
        T_F16 => "F16",
        T_F32 => "F32",
        T_F64 => "F64",
        _ => "?",
    }
}

/// True if converting from `from` to `to` is a narrowing conversion (may lose precision/range).
pub(crate) fn is_narrowing_numeric(from: TypeId, to: TypeId) -> bool {
    let r_from = numeric_rank(from);
    let r_to = numeric_rank(to);
    if r_from == 255 || r_to == 255 {
        return false;
    }
    // Float to int: always narrowing
    let from_int = r_from <= 3;
    let to_int = r_to <= 3;
    if !from_int && to_int {
        return true;
    }
    // Same category: narrowing when from has higher rank
    if from_int == to_int {
        return r_from > r_to;
    }
    // Int to float: not narrowing
    false
}

/// Coerce a numeric value from one type to another. Returns Err for unsupported conversions.
pub(crate) fn coerce_numeric(
    span: Span,
    v: VRegId,
    from: TypeId,
    to: TypeId,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    if from == to {
        return Ok(v);
    }
    let out = b.new_vreg(to);
    match (from, to) {
        // Integer widening
        (T_I8, T_I16) => b.emit(span, IrOp::SextI16 { dst: out, src: v }),
        (T_I8, T_I32) | (T_I16, T_I32) => b.emit(span, IrOp::Mov { dst: out, src: v }),
        (T_I32, T_I64) => b.emit(span, IrOp::SextI64 { dst: out, src: v }),
        (T_I8, T_I64) | (T_I16, T_I64) => {
            let v32 = b.new_vreg(T_I32);
            b.emit(span, IrOp::Mov { dst: v32, src: v });
            b.emit(span, IrOp::SextI64 { dst: out, src: v32 });
        }
        // Integer narrowing
        (T_I16, T_I8) | (T_I32, T_I8) => b.emit(span, IrOp::TruncI8 { dst: out, src: v }),
        (T_I32, T_I16) => b.emit(span, IrOp::TruncI16 { dst: out, src: v }),
        (T_I64, T_I32) => b.emit(span, IrOp::I32FromI64 { dst: out, src: v }),
        (T_I64, T_I16) => {
            let v32 = b.new_vreg(T_I32);
            b.emit(span, IrOp::I32FromI64 { dst: v32, src: v });
            b.emit(span, IrOp::TruncI16 { dst: out, src: v32 });
        }
        (T_I64, T_I8) => {
            let v32 = b.new_vreg(T_I32);
            b.emit(span, IrOp::I32FromI64 { dst: v32, src: v });
            b.emit(span, IrOp::TruncI8 { dst: out, src: v32 });
        }
        // Int to float
        (T_I32, T_F32) => b.emit(span, IrOp::F32FromI32 { dst: out, src: v }),
        (T_I32, T_F64) => b.emit(span, IrOp::F64FromI32 { dst: out, src: v }),
        (T_I64, T_F64) => b.emit(span, IrOp::F64FromI64 { dst: out, src: v }),
        (T_I64, T_F32) => b.emit(span, IrOp::F32FromI64 { dst: out, src: v }),
        (T_I32, T_F16) => b.emit(span, IrOp::F16FromI32 { dst: out, src: v }),
        (T_I64, T_F16) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromI64 { dst: v32, src: v });
            b.emit(span, IrOp::F16FromF32 { dst: out, src: v32 });
        }
        // Float widening
        (T_F16, T_F32) => b.emit(span, IrOp::F32FromF16 { dst: out, src: v }),
        (T_F16, T_F64) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromF16 { dst: v32, src: v });
            b.emit(span, IrOp::F64FromF32 { dst: out, src: v32 });
        }
        (T_F32, T_F64) => b.emit(span, IrOp::F64FromF32 { dst: out, src: v }),
        // Float narrowing
        (T_F32, T_F16) => b.emit(span, IrOp::F16FromF32 { dst: out, src: v }),
        (T_F64, T_F16) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromF64 { dst: v32, src: v });
            b.emit(span, IrOp::F16FromF32 { dst: out, src: v32 });
        }
        (T_F64, T_F32) => b.emit(span, IrOp::F32FromF64 { dst: out, src: v }),
        // Float to int
        (T_F64, T_I32) => b.emit(span, IrOp::I32FromF64 { dst: out, src: v }),
        (T_F64, T_I64) => b.emit(span, IrOp::I64FromF64 { dst: out, src: v }),
        (T_F32, T_I32) => b.emit(span, IrOp::I32FromF32 { dst: out, src: v }),
        (T_F32, T_I64) => b.emit(span, IrOp::I64FromF32 { dst: out, src: v }),
        (T_F16, T_I32) => b.emit(span, IrOp::I32FromF16 { dst: out, src: v }),
        (T_F16, T_I64) => {
            let v32 = b.new_vreg(T_F32);
            b.emit(span, IrOp::F32FromF16 { dst: v32, src: v });
            b.emit(span, IrOp::I64FromF32 { dst: out, src: v32 });
        }
        (T_F64, T_I8) | (T_F32, T_I8) | (T_F16, T_I8) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_F64 => b.emit(span, IrOp::I32FromF64 { dst: v32, src: v }),
                T_F32 => b.emit(span, IrOp::I32FromF32 { dst: v32, src: v }),
                T_F16 => b.emit(span, IrOp::I32FromF16 { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::TruncI8 { dst: out, src: v32 });
        }
        (T_F64, T_I16) | (T_F32, T_I16) | (T_F16, T_I16) => {
            let v32 = b.new_vreg(T_I32);
            match from {
                T_F64 => b.emit(span, IrOp::I32FromF64 { dst: v32, src: v }),
                T_F32 => b.emit(span, IrOp::I32FromF32 { dst: v32, src: v }),
                T_F16 => b.emit(span, IrOp::I32FromF16 { dst: v32, src: v }),
                _ => unreachable!(),
            }
            b.emit(span, IrOp::TruncI16 { dst: out, src: v32 });
        }
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "unsupported numeric conversion",
            ))
        }
    }
    Ok(out)
}

fn emit_bytes_eq(
    span: Span,
    dst: VRegId,
    a: VRegId,
    b2: VRegId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    let sig_args = [T_BYTES, T_BYTES];
    let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);
    let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args);

    let vcallee = b.new_vreg(fun_tid);
    b.emit(
        span,
        IrOp::ConstFun {
            dst: vcallee,
            func_index: crate::jlyb::PRELUDE_BYTES_EQ,
        },
    );

    // Marshal args into a contiguous vreg window.
    let arg0 = b.new_vreg(T_BYTES);
    b.emit(span, IrOp::Mov { dst: arg0, src: a });
    let arg1 = b.new_vreg(T_BYTES);
    b.emit(span, IrOp::Mov { dst: arg1, src: b2 });

    b.emit(
        span,
        IrOp::Call {
            dst,
            callee: vcallee,
            sig_id,
            arg_base: arg0,
            nargs: 2,
        },
    );
    Ok(())
}

fn lower_tuple_eq(
    span: Span,
    a: VRegId,
    b2: VRegId,
    tup_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    // Ensure we are in an open block.
    if !b.is_open() {
        let nb = b.new_block(Some("cont".to_string()));
        b.set_block(nb);
    }

    let elems = ctx
        .type_ctx
        .tuple_elems(tup_tid)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad tuple type"))?
        .to_vec();

    let out = b.new_vreg(T_BOOL);
    let fail_b = b.new_block(Some("tup_eq_fail".to_string()));
    let ok_b = b.new_block(Some("tup_eq_ok".to_string()));
    let join_b = b.new_block(Some("tup_eq_join".to_string()));

    if elems.is_empty() {
        // () == () is true.
        b.emit(span, IrOp::ConstBool { dst: out, imm: true });
        return Ok(out);
    } else {
        // Compare each element with early-exit on mismatch.
        for (i, &et) in elems.iter().enumerate() {
            let atom_id = intern_atom(&i.to_string(), ctx);
            let va = b.new_vreg(et);
            let vb = b.new_vreg(et);
            b.emit(span, IrOp::ObjGetAtom { dst: va, obj: a, atom_id });
            b.emit(span, IrOp::ObjGetAtom { dst: vb, obj: b2, atom_id });

            let v_eq = b.new_vreg(T_BOOL);
            match et {
                T_I32 => b.emit(span, IrOp::EqI32 { dst: v_eq, a: va, b: vb }),
                T_BOOL => b.emit(span, IrOp::Physeq { dst: v_eq, a: va, b: vb }),
                T_BYTES => emit_bytes_eq(span, v_eq, va, vb, ctx, b)?,
                _ if ctx.type_ctx.is_tuple_type(et) => {
                    // Nested tuples.
                    let nested = lower_tuple_eq(span, va, vb, et, ctx, b)?;
                    b.emit(span, IrOp::Mov { dst: v_eq, src: nested });
                }
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        span,
                        "tuple element equality not supported for this type yet",
                    ))
                }
            }

            let then_tgt = if i + 1 == elems.len() {
                ok_b
            } else {
                let nb = b.new_block(Some(format!("tup_eq_next{}", i)));
                nb
            };
            b.term(IrTerminator::JmpIf {
                cond: v_eq,
                then_tgt,
                else_tgt: fail_b,
            });
            b.set_block(then_tgt);
        }
    }

    // ok: true
    b.set_block(ok_b);
    b.emit(span, IrOp::ConstBool { dst: out, imm: true });
    b.term(IrTerminator::Jmp { target: join_b });

    // fail: false
    b.set_block(fail_b);
    b.emit(span, IrOp::ConstBool { dst: out, imm: false });
    b.term(IrTerminator::Jmp { target: join_b });

    // join: value in `out`
    b.set_block(join_b);
    Ok(out)
}

pub fn lower_expr(e: &Expr, ctx: &mut LowerCtx, b: &mut IrBuilder) -> Result<(VRegId, TypeId), CompileError> {
    lower_expr_expect(e, None, ctx, b)
}

pub(super) fn lower_expr_expect(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    fn numeric_rank(t: TypeId) -> u8 {
        match t {
            T_I8 => 0,
            T_I16 => 1,
            T_I32 => 2,
            T_I64 => 3,
            T_F16 => 4,
            T_F32 => 5,
            T_F64 => 6,
            _ => 255,
        }
    }

    fn promote_numeric_bin(
        span: Span,
        va: VRegId,
        ta: TypeId,
        vb: VRegId,
        tb: TypeId,
        op: &str,
        expect: Option<TypeId>,
        b: &mut IrBuilder,
    ) -> Result<(VRegId, TypeId, VRegId, TypeId, TypeId), CompileError> {
        if !is_numeric(ta) || !is_numeric(tb) {
            return Err(CompileError::new(ErrorKind::Type, span, format!("'{}' expects numeric operands", op)));
        }

        let out_t = match op {
            "/" => {
                if expect == Some(T_F64) {
                    T_F64
                } else if ta == T_F64 || tb == T_F64 {
                    T_F64
                } else {
                    T_F32
                }
            }
            _ => {
                if expect == Some(T_F64) {
                    T_F64
                } else if expect == Some(T_F32) && (numeric_rank(ta) >= 2 || numeric_rank(tb) >= 2) {
                    // If a float is involved and we have a float expectation, respect it.
                    T_F32
                } else {
                    // Widen to the "largest" participating numeric type.
                    if numeric_rank(ta) >= numeric_rank(tb) { ta } else { tb }
                }
            }
        };

        let va2 = coerce_numeric(span, va, ta, out_t, b)?;
        let vb2 = coerce_numeric(span, vb, tb, out_t, b)?;
        Ok((va2, out_t, vb2, out_t, out_t))
    }

    #[allow(unreachable_patterns)]
    match &e.node {
        ExprKind::BytesLit(bytes) => {
            let idx = ctx.const_bytes.len() as u32;
            ctx.const_bytes.push(bytes.clone());
            let v = b.new_vreg(T_BYTES);
            b.emit(e.span, IrOp::ConstBytes { dst: v, pool_index: idx });
            Ok((v, T_BYTES))
        }
        ExprKind::BoolLit(x) => {
            let v = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::ConstBool { dst: v, imm: *x });
            Ok((v, T_BOOL))
        }
        ExprKind::I32Lit(x) => {
            if let Some(et) = expect {
                if et == T_I8 && *x >= -128 && *x <= 127 {
                    let v = b.new_vreg(T_I8);
                    let imm = (*x as i16).to_le_bytes()[0];
                    b.emit(e.span, IrOp::ConstI8Imm { dst: v, imm });
                    return Ok((v, T_I8));
                }
                if et == T_I16 && *x >= -32768 && *x <= 32767 {
                    let v = b.new_vreg(T_I16);
                    b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
                    return Ok((v, T_I16));
                }
            }
            let v = b.new_vreg(T_I32);
            b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
            Ok((v, T_I32))
        }
        ExprKind::I8Lit(x) => {
            let v = b.new_vreg(T_I8);
            let imm = (*x as i16).to_le_bytes()[0];
            b.emit(e.span, IrOp::ConstI8Imm { dst: v, imm });
            Ok((v, T_I8))
        }
        ExprKind::I16Lit(x) => {
            let v = b.new_vreg(T_I16);
            b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
            Ok((v, T_I16))
        }
        ExprKind::F16Lit(x) => {
            let v = b.new_vreg(T_F16);
            let bits = f32_to_f16_bits(*x);
            b.emit(e.span, IrOp::ConstF16 { dst: v, bits });
            Ok((v, T_F16))
        }
        ExprKind::I64Lit(x) => {
            let idx = ctx.const_i64.len() as u32;
            ctx.const_i64.push(*x);
            let v = b.new_vreg(T_I64);
            b.emit(e.span, IrOp::ConstI64 { dst: v, pool_index: idx });
            Ok((v, T_I64))
        }
        ExprKind::F64Lit(x) => {
            // Default float literal type is F32 unless an explicit expected type is F64 or F16.
            if expect == Some(T_F64) {
                let idx = ctx.const_f64.len() as u32;
                ctx.const_f64.push(*x);
                let v = b.new_vreg(T_F64);
                b.emit(e.span, IrOp::ConstF64 { dst: v, pool_index: idx });
                Ok((v, T_F64))
            } else if expect == Some(T_F16) {
                let v = b.new_vreg(T_F16);
                let bits = f32_to_f16_bits(*x as f32);
                b.emit(e.span, IrOp::ConstF16 { dst: v, bits });
                Ok((v, T_F16))
            } else {
                let v = b.new_vreg(T_F32);
                let bits = (*x as f32).to_bits();
                b.emit(e.span, IrOp::ConstF32 { dst: v, bits });
                Ok((v, T_F32))
            }
        }
        ExprKind::AtomLit(name) => {
            let atom_id = super::intern_atom(name.as_str(), ctx);
            let v = b.new_vreg(T_ATOM);
            b.emit(e.span, IrOp::ConstAtom { dst: v, atom_id });
            Ok((v, T_ATOM))
        }
        ExprKind::Null => {
            // `null` is represented as Dynamic, but it is also allowed in pointer-typed contexts
            // (bytes/object/array/list/function/...) as a null pointer.
            let v = b.new_vreg(T_DYNAMIC);
            b.emit(e.span, IrOp::ConstNull { dst: v });
            if let Some(et) = expect {
                // Any Object-kind type is stored as a pointer slot in the VM.
                if et == T_BYTES || et == T_ARRAY_I32 || et == T_ARRAY_BYTES || et == T_LIST_I32 || et == T_LIST_BYTES || is_object_kind(&ctx.type_ctx, et) {
                    let out = b.new_vreg(et);
                    b.emit(e.span, IrOp::FromDynPtr { dst: out, src: v });
                    return Ok((out, et));
                }
            }
            Ok((v, T_DYNAMIC))
        }
        ExprKind::Var(name) => {
            if let Some(fc) = ctx.fn_stack.last() {
                if fc.self_name.as_deref() == Some(name.as_str()) {
                    let tid = fc
                        .self_fun_tid
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing self function type"))?;
                    let func_index = fc
                        .self_func_index
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing self func index"))?;
                    let v = b.new_vreg(tid);
                    b.emit(e.span, IrOp::ConstFun { dst: v, func_index });
                    return Ok((v, tid));
                }
            }
            if ctx.env_stack.iter().rev().find_map(|m| m.get(name)).is_none() {
                let _ = ensure_capture_binding(ctx, b, name, e.span)?;
            }
            let bd = lookup_var(ctx, name, e.span)?;
            if let Some(et) = expect {
                if bd.tid == et {
                    return Ok((bd.v, bd.tid));
                }
                // Nominal object upcast: allow `Box<I32>` where plain `Object` is expected.
                if et == T_OBJECT && is_object_kind(&ctx.type_ctx, bd.tid) && bd.tid != T_OBJECT {
                    let out = b.new_vreg(T_OBJECT);
                    b.emit(e.span, IrOp::Mov { dst: out, src: bd.v });
                    return Ok((out, T_OBJECT));
                }
                // Implicit boxing/unboxing at Dynamic boundaries.
                if et == T_DYNAMIC {
                    let out = b.new_vreg(T_DYNAMIC);
                    b.emit(e.span, IrOp::ToDyn { dst: out, src: bd.v });
                    return Ok((out, T_DYNAMIC));
                }
                if bd.tid == T_DYNAMIC {
                    let out = b.new_vreg(et);
                    match et {
                        T_I8 => b.emit(e.span, IrOp::FromDynI8 { dst: out, src: bd.v }),
                        T_I16 => b.emit(e.span, IrOp::FromDynI16 { dst: out, src: bd.v }),
                        T_I32 => b.emit(e.span, IrOp::FromDynI32 { dst: out, src: bd.v }),
                        T_I64 => b.emit(e.span, IrOp::FromDynI64 { dst: out, src: bd.v }),
                        T_F16 => b.emit(e.span, IrOp::FromDynF16 { dst: out, src: bd.v }),
                        T_F32 => b.emit(e.span, IrOp::FromDynF32 { dst: out, src: bd.v }),
                        T_F64 => b.emit(e.span, IrOp::FromDynF64 { dst: out, src: bd.v }),
                        T_BOOL => b.emit(e.span, IrOp::FromDynBool { dst: out, src: bd.v }),
                        // bytes/object/array ptr kinds
                        T_BYTES | T_OBJECT | T_ARRAY_I32 | T_ARRAY_BYTES => b.emit(e.span, IrOp::FromDynPtr { dst: out, src: bd.v }),
                        _ => {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "unsupported Dynamic->typed conversion",
                            ))
                        }
                    }
                    return Ok((out, et));
                }
                // Implicit numeric conversion (widen or narrow with warning)
                if is_numeric(bd.tid) && is_numeric(et) {
                    if let Ok(coerced) = coerce_numeric(e.span, bd.v, bd.tid, et, b) {
                        if is_narrowing_numeric(bd.tid, et) {
                            ctx.warnings.push(crate::error::CompileWarning::new(
                                e.span,
                                format!(
                                    "implicit narrowing conversion from {} to {}",
                                    type_name(bd.tid),
                                    type_name(et)
                                ),
                            ));
                        }
                        return Ok((coerced, et));
                    }
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "type mismatch (no implicit conversion)",
                ));
            }
            Ok((bd.v, bd.tid))
        }
        ExprKind::Member { base, name } => {
            // Namespaces must be used as call targets (handled in ExprKind::Call builtin logic).
            if let ExprKind::Var(ns) = &base.node {
                if matches!(
                    ns.as_str(),
                    "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
                ) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "namespace members must be called (e.g. Bytes.len(x))",
                    ));
                }
            }

            let (v_obj, t_obj) = lower_expr(base, ctx, b)?;
            let te = ctx
                .type_ctx
                .types
                .get(t_obj as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad member base type id"))?;
            if te.kind != crate::jlyb::TypeKind::Object {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "member access currently only supported for Object (obj.field)",
                ));
            }

            let atom_id = intern_atom(name.as_str(), ctx);

            // If this is a module namespace object, prefer the module's declared export type.
            if let ExprKind::Var(alias) = &base.node {
                if let Some(exports) = ctx.module_alias_exports.get(alias) {
                    let out_tid = exports.get(name).copied().or(expect).unwrap_or(T_DYNAMIC);
                    let out = b.new_vreg(out_tid);
                    b.emit(e.span, IrOp::ObjGetAtom { dst: out, obj: v_obj, atom_id });
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
                        return Err(CompileError::new(ErrorKind::Type, e.span, "module export type mismatch"));
                    }
                    return Ok((out, out_tid));
                }
            }

            if ctx.type_ctx.is_tuple_type(t_obj) {
                // Tuple element access: `t.0`, `t.1`, ...
                let idx: usize = name
                    .parse()
                    .map_err(|_| CompileError::new(ErrorKind::Type, e.span, "tuple element access must be .<index>"))?;
                let elems = ctx
                    .type_ctx
                    .tuple_elems(t_obj)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad tuple type"))?;
                if idx >= elems.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "tuple index out of range"));
                }
                let elem_tid = elems[idx];
                let out = b.new_vreg(elem_tid);
                b.emit(e.span, IrOp::ObjGetAtom { dst: out, obj: v_obj, atom_id });
                if let Some(et) = expect {
                    if et == elem_tid {
                        return Ok((out, elem_tid));
                    }
                    if et == T_DYNAMIC {
                        let vd = b.new_vreg(T_DYNAMIC);
                        b.emit(e.span, IrOp::ToDyn { dst: vd, src: out });
                        return Ok((vd, T_DYNAMIC));
                    }
                    return Err(CompileError::new(ErrorKind::Type, e.span, "tuple element type mismatch"));
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
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad expected type id"))?;
                    if te.kind == crate::jlyb::TypeKind::Function {
                        let sig_id = te.p0;
                        let sig = ctx
                            .type_ctx
                            .sigs
                            .get(sig_id as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?
                            .clone();

                        // Unbound method signature: (Object, A...) -> R
                        let mut unbound_args: Vec<TypeId> = Vec::with_capacity(1 + sig.args.len());
                        unbound_args.push(T_OBJECT);
                        unbound_args.extend(sig.args.iter().copied());
                        let unbound_sig_id = ctx.type_ctx.intern_sig(sig.ret_type, &unbound_args);
                        let unbound_fun_tid = ctx.type_ctx.intern_fun_type(sig.ret_type, &unbound_args);
                        let _ = unbound_sig_id; // documented by type table; call sites will use bound signature

                        let v_unbound = b.new_vreg(unbound_fun_tid);
                        b.emit(e.span, IrOp::ObjGetAtom { dst: v_unbound, obj: v_obj, atom_id });

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
                let out_tid = expect.unwrap_or(T_DYNAMIC);
                if out_tid == T_DYNAMIC {
                    let tmp = b.new_vreg(T_DYNAMIC);
                    b.emit(e.span, IrOp::ObjGetAtom { dst: tmp, obj: v_obj, atom_id });
                    Ok((tmp, T_DYNAMIC))
                } else if is_numeric(out_tid) || out_tid == T_BOOL || out_tid == T_BYTES || out_tid == T_OBJECT || out_tid == T_ARRAY_I32 || out_tid == T_ARRAY_BYTES {
                    let out = b.new_vreg(out_tid);
                    b.emit(e.span, IrOp::ObjGetAtom { dst: out, obj: v_obj, atom_id });
                    Ok((out, out_tid))
                } else {
                    Err(CompileError::new(ErrorKind::Type, e.span, "object property cannot produce this type"))
                }
            }
        }
        ExprKind::ArrayLit(elems) => lit::lower_array_lit(e, elems, expect, ctx, b),
        ExprKind::TupleLit(elems) => lit::lower_tuple_lit(e, elems, ctx, b),
        ExprKind::ObjLit(fields) => {
            let out_tid = expect.filter(|&et| is_object_kind(&ctx.type_ctx, et)).unwrap_or(T_OBJECT);
            let out = b.new_vreg(out_tid);
            b.emit(e.span, IrOp::ObjNew { dst: out });
            for (k, ve) in fields {
                let (v_val, _t_val) = lower_expr(ve, ctx, b)?;
                let atom_id = intern_atom(k.as_str(), ctx);
                b.emit(
                    e.span,
                    IrOp::ObjSetAtom {
                        obj: out,
                        atom_id,
                        value: v_val,
                    },
                );
            }
            Ok((out, out_tid))
        }
        ExprKind::Index { base, index } => {
            let (vb, tb) = lower_expr(base, ctx, b)?;
            let (vi_raw, ti_raw) = lower_expr(index, ctx, b)?;
            let (vi, ti) = match ti_raw {
                T_I32 => (vi_raw, T_I32),
                // Allow any integer index type (and Dynamic via unboxing), but the VM op expects I32.
                T_I8 | T_I16 | T_I64 => (coerce_numeric(index.span, vi_raw, ti_raw, T_I32, b)?, T_I32),
                T_DYNAMIC => (lower_expr_expect(index, Some(T_I32), ctx, b)?.0, T_I32),
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        index.span,
                        "index must be an integer",
                    ))
                }
            };
            match tb {
                T_ARRAY_I32 => {
                    let out = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ArrayGet { dst: out, arr: vb, index: vi });
                    Ok((out, T_I32))
                }
                T_ARRAY_BYTES => {
                    let out = b.new_vreg(T_BYTES);
                    b.emit(e.span, IrOp::ArrayGet { dst: out, arr: vb, index: vi });
                    Ok((out, T_BYTES))
                }
                T_BYTES => {
                    let out = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::BytesGetU8 { dst: out, bytes: vb, index: vi });
                    Ok((out, T_I32))
                }
                _ => Err(CompileError::new(ErrorKind::Type, e.span, "indexing not supported for this type yet")),
            }
        }
        ExprKind::Add(_a, _b) => {
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
                        let (va, ta) = lower_expr_expect(terms[0], Some(tb), ctx, b)?;
                        (va, ta, vb, tb)
                    }
                    (_, ExprKind::Member { .. }) => {
                        let (va, ta) = lower_expr(terms[0], ctx, b)?;
                        let (vb, tb) = lower_expr_expect(terms[1], Some(ta), ctx, b)?;
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
                    b.emit(e.span, IrOp::BytesConcat2 { dst: out, a: va, b: vb });
                    return Ok((out, T_BYTES));
                }
                if is_numeric(ta) && is_numeric(tb) {
                    let (va2, _ta2, vb2, _tb2, out_t) =
                        promote_numeric_bin(e.span, va, ta, vb, tb, "+", expect, b)?;
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
                        T_I8 | T_I16 | T_I32 => b.emit(e.span, IrOp::AddI32 { dst: out, a: va2, b: vb2 }),
                        T_I64 => b.emit(e.span, IrOp::AddI64 { dst: out, a: va2, b: vb2 }),
                        T_F16 => b.emit(e.span, IrOp::AddF16 { dst: out, a: va2, b: vb2 }),
                        T_F32 => b.emit(e.span, IrOp::AddF32 { dst: out, a: va2, b: vb2 }),
                        T_F64 => b.emit(e.span, IrOp::AddF64 { dst: out, a: va2, b: vb2 }),
                        _ => {
                            return Err(CompileError::new(ErrorKind::Internal, e.span, "bad numeric promotion"))
                        }
                    }
                    }
                    return Ok((out, out_t));
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "'+' expects bytes or numeric operands",
                ));
            }

            // Compile terms left-to-right, but allow member gets to be typed by a previously
            // inferred add-chain type (so `o.s + x` can treat `o.s` as bytes/i32).
            let mut compiled_opt: Vec<Option<(VRegId, TypeId)>> = vec![None; terms.len()];
            let mut deferred_member_idxs: Vec<usize> = Vec::new();
            let mut t0: Option<TypeId> = None;
            for (i, t) in terms.iter().enumerate() {
                if matches!(t.node, ExprKind::Member { .. }) {
                    deferred_member_idxs.push(i);
                    continue;
                }
                let (v, tt) = lower_expr(t, ctx, b)?;
                if let Some(t0v) = t0 {
                    if tt != t0v {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "'+' expects operands of same type",
                        ));
                    }
                } else {
                    t0 = Some(tt);
                }
                compiled_opt[i] = Some((v, tt));
            }
            let t0 = t0.or(expect).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "cannot infer '+' operand type (needs a non-member term or type context)",
                )
            })?;
            for i in deferred_member_idxs {
                let (v, tt) = lower_expr_expect(terms[i], Some(t0), ctx, b)?;
                if tt != t0 {
                    return Err(CompileError::new(ErrorKind::Type, terms[i].span, "member type mismatch"));
                }
                compiled_opt[i] = Some((v, tt));
            }
            let compiled: Vec<(VRegId, TypeId)> = compiled_opt
                .into_iter()
                .map(|x| x.expect("compiled term"))
                .collect();

            match t0 {
                T_BYTES => {
                    // Build Array<bytes> and concat_many.
                    let n = compiled.len() as i32;
                    let v_n = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v_n, imm: n });

                    let v_arr = b.new_vreg(T_ARRAY_BYTES);
                    b.emit(e.span, IrOp::ArrayNew { dst: v_arr, len: v_n });

                    for (i, (v_part, _)) in compiled.iter().enumerate() {
                        let v_i = b.new_vreg(T_I32);
                        b.emit(e.span, IrOp::ConstI32 { dst: v_i, imm: i as i32 });
                        b.emit(e.span, IrOp::ArraySet { arr: v_arr, index: v_i, value: *v_part });
                    }

                    let out = b.new_vreg(T_BYTES);
                    b.emit(e.span, IrOp::BytesConcatMany { dst: out, parts: v_arr });
                    Ok((out, T_BYTES))
                }
                T_I8 | T_I16 | T_I32 => {
                    // Fold i32 addition left-to-right; coerce I8/I16 to I32.
                    let mut acc = coerce_numeric(e.span, compiled[0].0, t0, T_I32, b)?;
                    for (rhs, _) in &compiled[1..] {
                        let rhs_i32 = coerce_numeric(e.span, *rhs, t0, T_I32, b)?;
                        let out = b.new_vreg(T_I32);
                        b.emit(e.span, IrOp::AddI32 { dst: out, a: acc, b: rhs_i32 });
                        acc = out;
                    }
                    Ok((acc, T_I32))
                }
                T_I64 => {
                    let mut acc = compiled[0].0;
                    for (rhs, _) in &compiled[1..] {
                        let out = b.new_vreg(T_I64);
                        b.emit(e.span, IrOp::AddI64 { dst: out, a: acc, b: *rhs });
                        acc = out;
                    }
                    Ok((acc, T_I64))
                }
                T_F16 => {
                    let mut acc = compiled[0].0;
                    for (rhs, _) in &compiled[1..] {
                        let out = b.new_vreg(T_F16);
                        b.emit(e.span, IrOp::AddF16 { dst: out, a: acc, b: *rhs });
                        acc = out;
                    }
                    Ok((acc, T_F16))
                }
                T_F32 => {
                    let mut acc = compiled[0].0;
                    for (rhs, _) in &compiled[1..] {
                        let out = b.new_vreg(T_F32);
                        b.emit(e.span, IrOp::AddF32 { dst: out, a: acc, b: *rhs });
                        acc = out;
                    }
                    Ok((acc, T_F32))
                }
                T_F64 => {
                    let mut acc = compiled[0].0;
                    for (rhs, _) in &compiled[1..] {
                        let out = b.new_vreg(T_F64);
                        b.emit(e.span, IrOp::AddF64 { dst: out, a: acc, b: *rhs });
                        acc = out;
                    }
                    Ok((acc, T_F64))
                }
                _ => Err(CompileError::new(ErrorKind::Type, e.span, "'+' not supported for this type yet")),
            }
        }
        ExprKind::Sub(a, bb) => {
            let (va, ta, vb, tb) = if let Some(et) = expect {
                if is_numeric(et) {
                    let (va, ta) = lower_expr_expect(a, Some(et), ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(et), ctx, b)?;
                    (va, ta, vb, tb)
                } else {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            } else {
                match (&a.node, &bb.node) {
                    (ExprKind::Member { .. }, ExprKind::Member { .. }) => {
                        let et = expect.filter(|&t| is_numeric(t)).unwrap_or(T_F64);
                        let (va, ta) = lower_expr_expect(a, Some(et), ctx, b)?;
                        let (vb, tb) = lower_expr_expect(bb, Some(et), ctx, b)?;
                        (va, ta, vb, tb)
                    }
                    (ExprKind::Member { .. }, _) => {
                        let (vb, tb) = lower_expr(bb, ctx, b)?;
                        if is_numeric(tb) {
                            let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                            (va, ta, vb, tb)
                        } else {
                            let (va, ta) = lower_expr(a, ctx, b)?;
                            (va, ta, vb, tb)
                        }
                    }
                    (_, ExprKind::Member { .. }) => {
                        let (va, ta) = lower_expr(a, ctx, b)?;
                        if is_numeric(ta) {
                            let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                            (va, ta, vb, tb)
                        } else {
                            let (vb, tb) = lower_expr(bb, ctx, b)?;
                            (va, ta, vb, tb)
                        }
                    }
                    _ => {
                        let (va, ta) = lower_expr(a, ctx, b)?;
                        let (vb, tb) = lower_expr(bb, ctx, b)?;
                        (va, ta, vb, tb)
                    }
                }
            };
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'-' expects numeric operands"));
            }
            let (va2, _ta2, vb2, _tb2, out_t) = promote_numeric_bin(e.span, va, ta, vb, tb, "-", expect, b)?;
            let out = b.new_vreg(out_t);
            match out_t {
                T_I8 | T_I16 | T_I32 => b.emit(e.span, IrOp::SubI32 { dst: out, a: va2, b: vb2 }),
                T_I64 => b.emit(e.span, IrOp::SubI64 { dst: out, a: va2, b: vb2 }),
                T_F16 => b.emit(e.span, IrOp::SubF16 { dst: out, a: va2, b: vb2 }),
                T_F32 => b.emit(e.span, IrOp::SubF32 { dst: out, a: va2, b: vb2 }),
                T_F64 => b.emit(e.span, IrOp::SubF64 { dst: out, a: va2, b: vb2 }),
                _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad numeric promotion")),
            }
            Ok((out, out_t))
        }
        ExprKind::Mul(a, bb) => {
            let (va, ta, vb, tb) = if let Some(et) = expect {
                if is_numeric(et) {
                    let (va, ta) = lower_expr_expect(a, Some(et), ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(et), ctx, b)?;
                    (va, ta, vb, tb)
                } else {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            } else {
                match (&a.node, &bb.node) {
                    (ExprKind::Member { .. }, ExprKind::Member { .. }) => {
                        let et = expect.filter(|&t| is_numeric(t)).unwrap_or(T_F64);
                        let (va, ta) = lower_expr_expect(a, Some(et), ctx, b)?;
                        let (vb, tb) = lower_expr_expect(bb, Some(et), ctx, b)?;
                        (va, ta, vb, tb)
                    }
                    (ExprKind::Member { .. }, _) => {
                        let (vb, tb) = lower_expr(bb, ctx, b)?;
                        if is_numeric(tb) {
                            let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                            (va, ta, vb, tb)
                        } else {
                            let (va, ta) = lower_expr(a, ctx, b)?;
                            (va, ta, vb, tb)
                        }
                    }
                    (_, ExprKind::Member { .. }) => {
                        let (va, ta) = lower_expr(a, ctx, b)?;
                        if is_numeric(ta) {
                            let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                            (va, ta, vb, tb)
                        } else {
                            let (vb, tb) = lower_expr(bb, ctx, b)?;
                            (va, ta, vb, tb)
                        }
                    }
                    _ => {
                        let (va, ta) = lower_expr(a, ctx, b)?;
                        let (vb, tb) = lower_expr(bb, ctx, b)?;
                        (va, ta, vb, tb)
                    }
                }
            };
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'*' expects numeric operands"));
            }
            let (va2, _ta2, vb2, _tb2, out_t) = promote_numeric_bin(e.span, va, ta, vb, tb, "*", expect, b)?;
            let out = b.new_vreg(out_t);
            match out_t {
                T_I8 | T_I16 | T_I32 => b.emit(e.span, IrOp::MulI32 { dst: out, a: va2, b: vb2 }),
                T_I64 => b.emit(e.span, IrOp::MulI64 { dst: out, a: va2, b: vb2 }),
                T_F16 => b.emit(e.span, IrOp::MulF16 { dst: out, a: va2, b: vb2 }),
                T_F32 => b.emit(e.span, IrOp::MulF32 { dst: out, a: va2, b: vb2 }),
                T_F64 => b.emit(e.span, IrOp::MulF64 { dst: out, a: va2, b: vb2 }),
                _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad numeric promotion")),
            }
            Ok((out, out_t))
        }
        ExprKind::Div(a, bb) => {
            let (va, ta, vb, tb) = if let Some(et) = expect {
                if is_numeric(et) {
                    let (va, ta) = lower_expr_expect(a, Some(et), ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(et), ctx, b)?;
                    (va, ta, vb, tb)
                } else {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            } else {
                match (&a.node, &bb.node) {
                    (ExprKind::Member { .. }, ExprKind::Member { .. }) => {
                        let et = expect.filter(|&t| is_numeric(t)).unwrap_or(T_F64);
                        let (va, ta) = lower_expr_expect(a, Some(et), ctx, b)?;
                        let (vb, tb) = lower_expr_expect(bb, Some(et), ctx, b)?;
                        (va, ta, vb, tb)
                    }
                    (ExprKind::Member { .. }, _) => {
                        let (vb, tb) = lower_expr(bb, ctx, b)?;
                        if is_numeric(tb) {
                            let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                            (va, ta, vb, tb)
                        } else {
                            let (va, ta) = lower_expr(a, ctx, b)?;
                            (va, ta, vb, tb)
                        }
                    }
                    (_, ExprKind::Member { .. }) => {
                        let (va, ta) = lower_expr(a, ctx, b)?;
                        if is_numeric(ta) {
                            let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                            (va, ta, vb, tb)
                        } else {
                            let (vb, tb) = lower_expr(bb, ctx, b)?;
                            (va, ta, vb, tb)
                        }
                    }
                    _ => {
                        let (va, ta) = lower_expr(a, ctx, b)?;
                        let (vb, tb) = lower_expr(bb, ctx, b)?;
                        (va, ta, vb, tb)
                    }
                }
            };
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'/' expects numeric operands"));
            }
            // Division always produces a float.
            let (va2, _ta2, vb2, _tb2, out_t) = promote_numeric_bin(e.span, va, ta, vb, tb, "/", expect, b)?;
            let out = b.new_vreg(out_t);
            match out_t {
                T_F32 => b.emit(e.span, IrOp::DivF32 { dst: out, a: va2, b: vb2 }),
                T_F64 => b.emit(e.span, IrOp::DivF64 { dst: out, a: va2, b: vb2 }),
                _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "division must produce float")),
            }
            Ok((out, out_t))
        }
        ExprKind::Neg(inner) => {
            let (v, t) = lower_expr(inner, ctx, b)?;
            if !is_numeric(t) {
                return Err(CompileError::new(ErrorKind::Type, e.span, "unary '-' expects numeric"));
            }
            let out = b.new_vreg(t);
            match t {
                T_I8 | T_I16 | T_I32 => b.emit(e.span, IrOp::NegI32 { dst: out, src: v }),
                T_I64 => b.emit(e.span, IrOp::NegI64 { dst: out, src: v }),
                T_F16 => {
                    // F16 has no NegF16; convert via F32
                    let v32 = b.new_vreg(T_F32);
                    b.emit(e.span, IrOp::F32FromF16 { dst: v32, src: v });
                    let neg32 = b.new_vreg(T_F32);
                    b.emit(e.span, IrOp::NegF32 { dst: neg32, src: v32 });
                    b.emit(e.span, IrOp::F16FromF32 { dst: out, src: neg32 });
                }
                T_F32 => b.emit(e.span, IrOp::NegF32 { dst: out, src: v }),
                T_F64 => b.emit(e.span, IrOp::NegF64 { dst: out, src: v }),
                _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad neg type")),
            }
            Ok((out, t))
        }
        ExprKind::Not(inner) => {
            let (v, t) = lower_expr(inner, ctx, b)?;
            if t != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'!' expects bool"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: v });
            Ok((out, T_BOOL))
        }
        ExprKind::Eq(a, bb) => {
            // If one side is a member access, allow it to take its type from the other side.
            let (va, ta, vb, tb) = match (&a.node, &bb.node) {
                (ExprKind::Member { .. }, _) => {
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                    (va, ta, vb, tb)
                }
                (_, ExprKind::Member { .. }) => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                    (va, ta, vb, tb)
                }
                _ => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            };
            let (va, ta, vb, tb) = if ta != tb && is_numeric(ta) && is_numeric(tb) {
                let (va2, _ta2, vb2, _tb2, out_t) = promote_numeric_bin(e.span, va, ta, vb, tb, "==", None, b)?;
                (va2, out_t, vb2, out_t)
            } else {
                (va, ta, vb, tb)
            };
            if ta != tb {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'==' expects operands of same type"));
            }

            // Tuple structural equality (element-wise).
            if ctx.type_ctx.is_tuple_type(ta) {
                let out = lower_tuple_eq(e.span, va, vb, ta, ctx, b)?;
                return Ok((out, T_BOOL));
            }

            let out = b.new_vreg(T_BOOL);
            match ta {
                T_BOOL => {
                    b.emit(e.span, IrOp::Physeq { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_ATOM => {
                    b.emit(e.span, IrOp::Physeq { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_I8 | T_I16 | T_I32 => {
                    b.emit(e.span, IrOp::EqI32 { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_I64 => {
                    b.emit(e.span, IrOp::EqI64 { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_F16 => {
                    let va32 = b.new_vreg(T_F32);
                    let vb32 = b.new_vreg(T_F32);
                    b.emit(e.span, IrOp::F32FromF16 { dst: va32, src: va });
                    b.emit(e.span, IrOp::F32FromF16 { dst: vb32, src: vb });
                    b.emit(e.span, IrOp::EqF32 { dst: out, a: va32, b: vb32 });
                    Ok((out, T_BOOL))
                }
                T_F32 => {
                    b.emit(e.span, IrOp::EqF32 { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_F64 => {
                    b.emit(e.span, IrOp::EqF64 { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                T_BYTES => {
                    emit_bytes_eq(e.span, out, va, vb, ctx, b)?;
                    Ok((out, T_BOOL))
                }
                T_DYNAMIC => {
                    // Dynamic equality is physical equality on boxed values.
                    // This is primarily used for `x == null` checks.
                    b.emit(e.span, IrOp::Physeq { dst: out, a: va, b: vb });
                    Ok((out, T_BOOL))
                }
                _ => Err(CompileError::new(ErrorKind::Type, e.span, "'==' not supported for this type yet")),
            }
        }
        ExprKind::Ne(a, bb) => {
            // a != b  ==  !(a == b)
            let (t, tt) = lower_expr(
                &crate::ast::Spanned::new(ExprKind::Eq(a.clone(), bb.clone()), e.span),
                ctx,
                b,
            )?;
            if tt != T_BOOL {
                return Err(CompileError::new(ErrorKind::Internal, e.span, "bad ne lowering"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: t });
            Ok((out, T_BOOL))
        }
        ExprKind::Lt(a, bb) => {
            let (va, ta, vb, tb) = match (&a.node, &bb.node) {
                (ExprKind::Member { .. }, _) => {
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    let (va, ta) = lower_expr_expect(a, Some(tb), ctx, b)?;
                    (va, ta, vb, tb)
                }
                (_, ExprKind::Member { .. }) => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr_expect(bb, Some(ta), ctx, b)?;
                    (va, ta, vb, tb)
                }
                _ => {
                    let (va, ta) = lower_expr(a, ctx, b)?;
                    let (vb, tb) = lower_expr(bb, ctx, b)?;
                    (va, ta, vb, tb)
                }
            };
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'<' expects numeric operands"));
            }
            let (va2, _ta2, vb2, _tb2, out_t) = promote_numeric_bin(e.span, va, ta, vb, tb, "<", None, b)?;
            let out = b.new_vreg(T_BOOL);
            match out_t {
                T_I8 | T_I16 | T_I32 => b.emit(e.span, IrOp::LtI32 { dst: out, a: va2, b: vb2 }),
                T_I64 => b.emit(e.span, IrOp::LtI64 { dst: out, a: va2, b: vb2 }),
                T_F16 => {
                    // F16 has no direct compare op; compare via F32.
                    let a32 = b.new_vreg(T_F32);
                    let b32 = b.new_vreg(T_F32);
                    b.emit(e.span, IrOp::F32FromF16 { dst: a32, src: va2 });
                    b.emit(e.span, IrOp::F32FromF16 { dst: b32, src: vb2 });
                    b.emit(e.span, IrOp::LtF32 { dst: out, a: a32, b: b32 });
                }
                T_F32 => b.emit(e.span, IrOp::LtF32 { dst: out, a: va2, b: vb2 }),
                T_F64 => b.emit(e.span, IrOp::LtF64 { dst: out, a: va2, b: vb2 }),
                _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad lt promotion")),
            }
            Ok((out, T_BOOL))
        }
        ExprKind::Gt(a, bb) => {
            // a > b  ==  b < a
            lower_expr(
                &crate::ast::Spanned::new(ExprKind::Lt(bb.clone(), a.clone()), e.span),
                ctx,
                b,
            )
        }
        ExprKind::Le(a, bb) => {
            // a <= b  ==  !(b < a)
            let (t, tt) = lower_expr(
                &crate::ast::Spanned::new(ExprKind::Lt(bb.clone(), a.clone()), e.span),
                ctx,
                b,
            )?;
            if tt != T_BOOL {
                return Err(CompileError::new(ErrorKind::Internal, e.span, "bad le lowering"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: t });
            Ok((out, T_BOOL))
        }
        ExprKind::Ge(a, bb) => {
            // a >= b  ==  !(a < b)
            let (t, tt) = lower_expr(
                &crate::ast::Spanned::new(ExprKind::Lt(a.clone(), bb.clone()), e.span),
                ctx,
                b,
            )?;
            if tt != T_BOOL {
                return Err(CompileError::new(ErrorKind::Internal, e.span, "bad ge lowering"));
            }
            let out = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::NotBool { dst: out, src: t });
            Ok((out, T_BOOL))
        }
        ExprKind::And(a, bb) => {
            // Short-circuit: if(!a) res=a(false) else res=b
            let (va, ta) = lower_expr(a, ctx, b)?;
            if ta != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
            }

            let rhs_b = b.new_block(Some("and_rhs".to_string()));
            let short_b = b.new_block(Some("and_short".to_string()));
            let join_b = b.new_block(Some("and_join".to_string()));

            b.term(IrTerminator::JmpIf {
                cond: va,
                then_tgt: rhs_b,
                else_tgt: short_b,
            });

            b.set_block(short_b);
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(rhs_b);
            let (vb, tb) = lower_expr(bb, ctx, b)?;
            if tb != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
            }
            let rhs_end = b.cur_block();
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(join_b);
            let v_res = b.new_vreg(T_BOOL);
            b.emit(
                e.span,
                IrOp::Phi {
                    dst: v_res,
                    incomings: vec![(short_b, va), (rhs_end, vb)],
                },
            );
            Ok((v_res, T_BOOL))
        }
        ExprKind::Or(a, bb) => {
            // Short-circuit:
            //   if(a) res=a(true) else res=b
            let (va, ta) = lower_expr(a, ctx, b)?;
            if ta != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
            }

            let short_b = b.new_block(Some("or_short".to_string()));
            let rhs_b = b.new_block(Some("or_rhs".to_string()));
            let join_b = b.new_block(Some("or_join".to_string()));

            b.term(IrTerminator::JmpIf {
                cond: va,
                then_tgt: short_b,
                else_tgt: rhs_b,
            });

            b.set_block(short_b);
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(rhs_b);
            let (vb, tb) = lower_expr(bb, ctx, b)?;
            if tb != T_BOOL {
                return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
            }
            let rhs_end = b.cur_block();
            b.term(IrTerminator::Jmp { target: join_b });

            b.set_block(join_b);
            let v_res = b.new_vreg(T_BOOL);
            b.emit(
                e.span,
                IrOp::Phi {
                    dst: v_res,
                    incomings: vec![(short_b, va), (rhs_end, vb)],
                },
            );
            Ok((v_res, T_BOOL))
        }
        ExprKind::TypeApp { .. } => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "unexpected type application (templates must be expanded before lowering)",
        )),
        ExprKind::Call { callee, type_args, args } => {
            return call::lower_call_expr(e, callee, type_args, args, expect, ctx, b);
        }
        ExprKind::Block { stmts, expr } => control::lower_block_expr(stmts, expr, ctx, b),
        ExprKind::If { cond, then_br, else_br } => control::lower_if_expr(e, cond, then_br, else_br, ctx, b),
        ExprKind::Try { body, catch_name, catch_body } => {
            control::lower_try_expr(e, body, catch_name, catch_body, ctx, b)
        }
        ExprKind::Match { subject, arms } => return r#match::lower_match_expr(e, subject, arms, expect, ctx, b),
        ExprKind::New { proto, args } => {
            return new_::lower_new_expr(e, proto, args, expect, ctx, b);
        }
        ExprKind::Fn { params, body, tail } => {
            return fn_::lower_fn_expr(e, params, body, tail, expect, ctx, b);
        }
        _ => Err(CompileError::new(
            ErrorKind::Codegen,
            e.span,
            "IR lowering: expression not implemented yet",
        )),
    }
}
