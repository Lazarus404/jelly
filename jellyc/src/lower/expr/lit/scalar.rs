use crate::ast::{Expr, ExprKind};
use crate::error::CompileError;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

use super::super::{f32_to_f16_bits, intern_atom, is_object_kind, LowerCtx};
use super::super::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16,
    T_I32, T_I64, T_I8, T_LIST_BYTES, T_LIST_I32,
};

pub(super) fn lower_scalar_lit_expr(
    e: &Expr,
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Option<Result<(VRegId, TypeId), CompileError>> {
    match &e.node {
        ExprKind::BytesLit(bytes) => Some({
            let idx = ctx.const_bytes.len() as u32;
            ctx.const_bytes.push(bytes.clone());
            let v = b.new_vreg(T_BYTES);
            b.emit(
                e.span,
                IrOp::ConstBytes {
                    dst: v,
                    pool_index: idx,
                },
            );
            Ok((v, T_BYTES))
        }),
        ExprKind::BoolLit(x) => Some({
            let v = b.new_vreg(T_BOOL);
            b.emit(e.span, IrOp::ConstBool { dst: v, imm: *x });
            Ok((v, T_BOOL))
        }),
        ExprKind::I32Lit(x) => Some({
            if let Some(et) = expect {
                if et == T_I8 && *x >= -128 && *x <= 127 {
                    let v = b.new_vreg(T_I8);
                    let imm = (*x as i16).to_le_bytes()[0];
                    b.emit(e.span, IrOp::ConstI8Imm { dst: v, imm });
                    Ok((v, T_I8))
                } else if et == T_I16 && *x >= -32768 && *x <= 32767 {
                    let v = b.new_vreg(T_I16);
                    b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
                    Ok((v, T_I16))
                } else if et == T_I64 {
                    let v32 = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v32, imm: *x });
                    let v64 = b.new_vreg(T_I64);
                    b.emit(e.span, IrOp::SextI64 { dst: v64, src: v32 });
                    Ok((v64, T_I64))
                } else if et == T_F16 {
                    let v32 = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v32, imm: *x });
                    let vf = b.new_vreg(T_F16);
                    b.emit(e.span, IrOp::F16FromI32 { dst: vf, src: v32 });
                    Ok((vf, T_F16))
                } else if et == T_F32 {
                    let v32 = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v32, imm: *x });
                    let vf = b.new_vreg(T_F32);
                    b.emit(e.span, IrOp::F32FromI32 { dst: vf, src: v32 });
                    Ok((vf, T_F32))
                } else if et == T_F64 {
                    let v32 = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v32, imm: *x });
                    let vf = b.new_vreg(T_F64);
                    b.emit(e.span, IrOp::F64FromI32 { dst: vf, src: v32 });
                    Ok((vf, T_F64))
                } else {
                    let v = b.new_vreg(T_I32);
                    b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
                    Ok((v, T_I32))
                }
            } else {
                let v = b.new_vreg(T_I32);
                b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
                Ok((v, T_I32))
            }
        }),
        ExprKind::I8Lit(x) => Some({
            let v = b.new_vreg(T_I8);
            let imm = (*x as i16).to_le_bytes()[0];
            b.emit(e.span, IrOp::ConstI8Imm { dst: v, imm });
            Ok((v, T_I8))
        }),
        ExprKind::I16Lit(x) => Some({
            let v = b.new_vreg(T_I16);
            b.emit(e.span, IrOp::ConstI32 { dst: v, imm: *x });
            Ok((v, T_I16))
        }),
        ExprKind::F16Lit(x) => Some({
            let v = b.new_vreg(T_F16);
            let bits = f32_to_f16_bits(*x);
            b.emit(e.span, IrOp::ConstF16 { dst: v, bits });
            Ok((v, T_F16))
        }),
        ExprKind::I64Lit(x) => Some({
            let idx = ctx.const_i64.len() as u32;
            ctx.const_i64.push(*x);
            let v = b.new_vreg(T_I64);
            b.emit(
                e.span,
                IrOp::ConstI64 {
                    dst: v,
                    pool_index: idx,
                },
            );
            Ok((v, T_I64))
        }),
        ExprKind::F64Lit(x) => Some({
            // Default float literal type is F32 unless an explicit expected type is F64 or F16.
            if expect == Some(T_F64) {
                let idx = ctx.const_f64.len() as u32;
                ctx.const_f64.push(*x);
                let v = b.new_vreg(T_F64);
                b.emit(
                    e.span,
                    IrOp::ConstF64 {
                        dst: v,
                        pool_index: idx,
                    },
                );
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
        }),
        ExprKind::AtomLit(name) => Some({
            let atom_id = intern_atom(name.as_str(), ctx);
            let v = b.new_vreg(T_ATOM);
            b.emit(e.span, IrOp::ConstAtom { dst: v, atom_id });
            Ok((v, T_ATOM))
        }),
        ExprKind::Null => Some({
            // `null` is represented as Dynamic, but it is also allowed in pointer-typed contexts
            // (bytes/object/array/list/function/...) as a null pointer.
            let v = b.new_vreg(T_DYNAMIC);
            b.emit(e.span, IrOp::ConstNull { dst: v });
            if let Some(et) = expect {
                // Any Object-kind type is stored as a pointer slot in the VM.
                if et == T_BYTES
                    || et == T_ARRAY_I32
                    || et == T_ARRAY_BYTES
                    || et == T_LIST_I32
                    || et == T_LIST_BYTES
                    || is_object_kind(&ctx.type_ctx, et)
                {
                    let out = b.new_vreg(et);
                    b.emit(e.span, IrOp::FromDynPtr { dst: out, src: v });
                    Ok((out, et))
                } else {
                    Ok((v, T_DYNAMIC))
                }
            } else {
                Ok((v, T_DYNAMIC))
            }
        }),
        _ => None,
    }
}
