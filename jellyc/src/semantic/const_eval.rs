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

use std::collections::HashSet;

use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, ConstValue, NodeId, SemanticInfo};
use crate::ir::TypeId;
use crate::typectx::{T_BYTES, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

fn is_int(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64)
}

fn is_float(t: TypeId) -> bool {
    matches!(t, T_F16 | T_F32 | T_F64)
}

fn expr_tid(info: &SemanticInfo, e: &Expr) -> Result<TypeId, CompileError> {
    info.expr_types.get(&NodeId(e.span)).copied().ok_or_else(|| {
        CompileError::new(
            ErrorKind::Internal,
            e.span,
            "missing semantic type for const initializer expression",
        )
    })
}

fn as_bool(v: &ConstValue, span: crate::ast::Span) -> Result<bool, CompileError> {
    match v {
        ConstValue::Bool(b) => Ok(*b),
        _ => Err(CompileError::new(ErrorKind::Type, span, "expected Bool constant")),
    }
}

fn as_int(v: &ConstValue, span: crate::ast::Span) -> Result<i64, CompileError> {
    match v {
        ConstValue::Int(x) => Ok(*x),
        _ => Err(CompileError::new(ErrorKind::Type, span, "expected integer constant")),
    }
}

fn as_float(v: &ConstValue, span: crate::ast::Span) -> Result<f64, CompileError> {
    match v {
        ConstValue::Float(x) => Ok(*x),
        _ => Err(CompileError::new(ErrorKind::Type, span, "expected float constant")),
    }
}

fn truthy(v: &ConstValue) -> bool {
    match v {
        ConstValue::Bool(false) => false,
        ConstValue::Null => false,
        _ => true,
    }
}

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

fn f16_bits_to_f32(bits: u16) -> f32 {
    let sign = ((bits & 0x8000) as u32) << 16;
    let exp = (bits >> 10) & 0x1F;
    let mant = (bits & 0x03FF) as u32;
    let out_bits = if exp == 0 {
        if mant == 0 {
            sign
        } else {
            // Subnormal: normalize.
            let mut e: i32 = -14;
            let mut m = mant;
            while (m & 0x0400) == 0 {
                m <<= 1;
                e -= 1;
            }
            m &= 0x03FF;
            let exp32 = ((e + 127) as u32) << 23;
            sign | exp32 | (m << 13)
        }
    } else if exp == 0x1F {
        // Inf/NaN
        sign | 0x7F80_0000 | (mant << 13)
    } else {
        let exp32 = ((exp as i32) - 15 + 127) as u32;
        sign | (exp32 << 23) | (mant << 13)
    };
    f32::from_bits(out_bits)
}

fn wrap_int(t: TypeId, x: i64) -> i64 {
    match t {
        T_I8 => (x as i8) as i64,
        T_I16 => (x as i16) as i64,
        T_I32 => (x as i32) as i64,
        T_I64 => x,
        _ => x,
    }
}

fn quantize_float(t: TypeId, x: f64) -> f64 {
    match t {
        T_F16 => {
            let bits = f32_to_f16_bits(x as f32);
            f16_bits_to_f32(bits) as f64
        }
        T_F32 => (x as f32) as f64,
        T_F64 => x,
        _ => x,
    }
}

fn coerce_value_to_type(t: TypeId, v: ConstValue, span: crate::ast::Span) -> Result<ConstValue, CompileError> {
    match t {
        T_I8 | T_I16 | T_I32 | T_I64 => match v {
            ConstValue::Int(x) => Ok(ConstValue::Int(wrap_int(t, x))),
            ConstValue::Float(x) => Ok(ConstValue::Int(wrap_int(t, x as i64))),
            _ => Err(CompileError::new(ErrorKind::Type, span, "expected numeric constant")),
        },
        T_F16 | T_F32 | T_F64 => match v {
            ConstValue::Int(x) => Ok(ConstValue::Float(quantize_float(t, x as f64))),
            ConstValue::Float(x) => Ok(ConstValue::Float(quantize_float(t, x))),
            _ => Err(CompileError::new(ErrorKind::Type, span, "expected numeric constant")),
        },
        T_BYTES => match v {
            ConstValue::Bytes(b) => Ok(ConstValue::Bytes(b)),
            ConstValue::Null => Ok(ConstValue::Null),
            _ => Err(CompileError::new(ErrorKind::Type, span, "expected Bytes constant")),
        },
        _ => Ok(v),
    }
}

fn resolve_alias_value(
    name: &str,
    span: crate::ast::Span,
    lookup_const: &mut dyn FnMut(&str) -> Option<ConstInit>,
    visiting: &mut HashSet<String>,
) -> Result<ConstValue, CompileError> {
    if !visiting.insert(name.to_string()) {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "cycle in const initializer aliases",
        ));
    }
    let init = lookup_const(name).ok_or_else(|| {
        CompileError::new(
            ErrorKind::Type,
            span,
            format!("unknown const '{name}' in const initializer"),
        )
    })?;
    match init {
        ConstInit::Value(v) => Ok(v),
        ConstInit::Alias(n2) => resolve_alias_value(&n2, span, lookup_const, visiting),
    }
}

fn force_value(
    init: ConstInit,
    span: crate::ast::Span,
    lookup_const: &mut dyn FnMut(&str) -> Option<ConstInit>,
) -> Result<ConstValue, CompileError> {
    match init {
        ConstInit::Value(v) => Ok(v),
        ConstInit::Alias(n) => resolve_alias_value(&n, span, lookup_const, &mut HashSet::new()),
    }
}

pub fn eval_const_expr(
    e: &Expr,
    info: &SemanticInfo,
    lookup_const: &mut dyn FnMut(&str) -> Option<ConstInit>,
) -> Result<ConstInit, CompileError> {
    match &e.node {
        ExprKind::BytesLit(b) => Ok(ConstInit::Value(ConstValue::Bytes(b.clone()))),
        ExprKind::BoolLit(b) => Ok(ConstInit::Value(ConstValue::Bool(*b))),
        ExprKind::AtomLit(s) => Ok(ConstInit::Value(ConstValue::Atom(s.clone()))),
        ExprKind::Null => Ok(ConstInit::Value(ConstValue::Null)),
        ExprKind::I32Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, *x as f64))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x as i64))))
            }
        }
        ExprKind::I8Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, *x as f64))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x as i64))))
            }
        }
        ExprKind::I16Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, *x as f64))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x as i64))))
            }
        }
        ExprKind::I64Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, *x as f64))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x))))
            }
        }
        ExprKind::F64Lit(x) => {
            let t = expr_tid(info, e)?;
            Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, *x))))
        }
        ExprKind::F16Lit(x) => {
            let t = expr_tid(info, e)?;
            Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, *x as f64))))
        }
        ExprKind::Var(name) => {
            let t = expr_tid(info, e)?;
            let init = lookup_const(name.as_str()).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    format!("const initializer references non-const '{name}'"),
                )
            })?;

            // Preserve identity for bytes-like constants when the usage type is Bytes.
            if t == T_BYTES {
                return Ok(ConstInit::Alias(name.clone()));
            }

            // Otherwise, inline the value (including numeric coercions) so `const b: F32 = a`
            // can still fold without emitting runtime conversions.
            let v = force_value(init, e.span, lookup_const)?;
            Ok(ConstInit::Value(coerce_value_to_type(t, v, e.span)?))
        }
        ExprKind::Truthy(inner) => {
            let v = force_value(eval_const_expr(inner, info, lookup_const)?, e.span, lookup_const)?;
            Ok(ConstInit::Value(ConstValue::Bool(truthy(&v))))
        }
        ExprKind::Not(inner) => {
            let v = force_value(eval_const_expr(inner, info, lookup_const)?, e.span, lookup_const)?;
            Ok(ConstInit::Value(ConstValue::Bool(!as_bool(&v, e.span)?)))
        }
        ExprKind::Neg(inner) => {
            let t = expr_tid(info, e)?;
            let v = force_value(eval_const_expr(inner, info, lookup_const)?, e.span, lookup_const)?;
            if is_int(t) {
                let x = as_int(&v, e.span)?;
                let out = match t {
                    T_I8 => (-(x as i8)) as i64,
                    T_I16 => (-(x as i16)) as i64,
                    T_I32 => (-(x as i32)) as i64,
                    T_I64 => (-(x as i64)) as i64,
                    _ => return Err(CompileError::new(ErrorKind::Type, e.span, "bad integer type for '-'")),
                };
                Ok(ConstInit::Value(ConstValue::Int(out)))
            } else if is_float(t) {
                let x = as_float(&v, e.span)?;
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, -x))))
            } else {
                Err(CompileError::new(ErrorKind::Type, e.span, "unary '-' expects numeric constant"))
            }
        }
        ExprKind::Add(a, b) | ExprKind::Sub(a, b) | ExprKind::Mul(a, b) | ExprKind::Div(a, b) => {
            let t = expr_tid(info, e)?;
            let va = force_value(eval_const_expr(a, info, lookup_const)?, e.span, lookup_const)?;
            let vb = force_value(eval_const_expr(b, info, lookup_const)?, e.span, lookup_const)?;

            if is_int(t) {
                let aa = as_int(&va, e.span)?;
                let bb = as_int(&vb, e.span)?;
                let out = match (&e.node, t) {
                    (ExprKind::Add(_, _), T_I8) => (aa as i8).wrapping_add(bb as i8) as i64,
                    (ExprKind::Add(_, _), T_I16) => (aa as i16).wrapping_add(bb as i16) as i64,
                    (ExprKind::Add(_, _), T_I32) => (aa as i32).wrapping_add(bb as i32) as i64,
                    (ExprKind::Add(_, _), T_I64) => (aa as i64).wrapping_add(bb as i64) as i64,
                    (ExprKind::Sub(_, _), T_I8) => (aa as i8).wrapping_sub(bb as i8) as i64,
                    (ExprKind::Sub(_, _), T_I16) => (aa as i16).wrapping_sub(bb as i16) as i64,
                    (ExprKind::Sub(_, _), T_I32) => (aa as i32).wrapping_sub(bb as i32) as i64,
                    (ExprKind::Sub(_, _), T_I64) => (aa as i64).wrapping_sub(bb as i64) as i64,
                    (ExprKind::Mul(_, _), T_I8) => (aa as i8).wrapping_mul(bb as i8) as i64,
                    (ExprKind::Mul(_, _), T_I16) => (aa as i16).wrapping_mul(bb as i16) as i64,
                    (ExprKind::Mul(_, _), T_I32) => (aa as i32).wrapping_mul(bb as i32) as i64,
                    (ExprKind::Mul(_, _), T_I64) => (aa as i64).wrapping_mul(bb as i64) as i64,
                    (ExprKind::Div(_, _), _) => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "division in const initializers must produce a float",
                        ))
                    }
                    _ => return Err(CompileError::new(ErrorKind::Type, e.span, "bad integer binop type")),
                };
                Ok(ConstInit::Value(ConstValue::Int(out)))
            } else if is_float(t) {
                let aa = as_float(&va, e.span)?;
                let bb = as_float(&vb, e.span)?;
                let out = match &e.node {
                    ExprKind::Add(_, _) => aa + bb,
                    ExprKind::Sub(_, _) => aa - bb,
                    ExprKind::Mul(_, _) => aa * bb,
                    ExprKind::Div(_, _) => aa / bb,
                    _ => unreachable!(),
                };
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, out))))
            } else {
                Err(CompileError::new(ErrorKind::Type, e.span, "binary op expects numeric constants"))
            }
        }
        ExprKind::Eq(a, b) | ExprKind::Ne(a, b) => {
            let va = force_value(eval_const_expr(a, info, lookup_const)?, e.span, lookup_const)?;
            let vb = force_value(eval_const_expr(b, info, lookup_const)?, e.span, lookup_const)?;
            let eq = match (&va, &vb) {
                (ConstValue::Bool(x), ConstValue::Bool(y)) => x == y,
                (ConstValue::Int(x), ConstValue::Int(y)) => x == y,
                (ConstValue::Float(x), ConstValue::Float(y)) => x == y,
                (ConstValue::Bytes(x), ConstValue::Bytes(y)) => x == y,
                (ConstValue::Atom(x), ConstValue::Atom(y)) => x == y,
                (ConstValue::Null, ConstValue::Null) => true,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "type mismatch in const equality",
                    ))
                }
            };
            let out = match &e.node {
                ExprKind::Eq(_, _) => eq,
                ExprKind::Ne(_, _) => !eq,
                _ => unreachable!(),
            };
            Ok(ConstInit::Value(ConstValue::Bool(out)))
        }
        ExprKind::Lt(a, b) | ExprKind::Le(a, b) | ExprKind::Gt(a, b) | ExprKind::Ge(a, b) => {
            let va = force_value(eval_const_expr(a, info, lookup_const)?, e.span, lookup_const)?;
            let vb = force_value(eval_const_expr(b, info, lookup_const)?, e.span, lookup_const)?;
            let out = match (&va, &vb) {
                (ConstValue::Int(x), ConstValue::Int(y)) => match &e.node {
                    ExprKind::Lt(_, _) => x < y,
                    ExprKind::Le(_, _) => x <= y,
                    ExprKind::Gt(_, _) => x > y,
                    ExprKind::Ge(_, _) => x >= y,
                    _ => unreachable!(),
                },
                (ConstValue::Float(x), ConstValue::Float(y)) => match &e.node {
                    ExprKind::Lt(_, _) => x < y,
                    ExprKind::Le(_, _) => x <= y,
                    ExprKind::Gt(_, _) => x > y,
                    ExprKind::Ge(_, _) => x >= y,
                    _ => unreachable!(),
                },
                _ => return Err(CompileError::new(ErrorKind::Type, e.span, "comparison expects numeric constants")),
            };
            Ok(ConstInit::Value(ConstValue::Bool(out)))
        }
        ExprKind::And(a, b) => {
            let va = force_value(eval_const_expr(a, info, lookup_const)?, e.span, lookup_const)?;
            let aa = as_bool(&va, e.span)?;
            if !aa {
                return Ok(ConstInit::Value(ConstValue::Bool(false)));
            }
            let vb = force_value(eval_const_expr(b, info, lookup_const)?, e.span, lookup_const)?;
            Ok(ConstInit::Value(ConstValue::Bool(as_bool(&vb, e.span)?)))
        }
        ExprKind::Or(a, b) => {
            let va = force_value(eval_const_expr(a, info, lookup_const)?, e.span, lookup_const)?;
            let aa = as_bool(&va, e.span)?;
            if aa {
                return Ok(ConstInit::Value(ConstValue::Bool(true)));
            }
            let vb = force_value(eval_const_expr(b, info, lookup_const)?, e.span, lookup_const)?;
            Ok(ConstInit::Value(ConstValue::Bool(as_bool(&vb, e.span)?)))
        }
        ExprKind::If { cond, then_br, else_br } => {
            let vc = force_value(eval_const_expr(cond, info, lookup_const)?, e.span, lookup_const)?;
            if as_bool(&vc, e.span)? {
                eval_const_expr(then_br, info, lookup_const)
            } else {
                eval_const_expr(else_br, info, lookup_const)
            }
        }
        // Not allowed in v1 const initializers.
        ExprKind::Member { .. }
        | ExprKind::Call { .. }
        | ExprKind::TypeApp { .. }
        | ExprKind::ArrayLit(_)
        | ExprKind::TupleLit(_)
        | ExprKind::ObjLit(_)
        | ExprKind::Index { .. }
        | ExprKind::Fn { .. }
        | ExprKind::Block { .. }
        | ExprKind::Try { .. }
        | ExprKind::Match { .. }
        | ExprKind::New { .. } => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "const initializer must be a compile-time constant expression",
        )),
    }
}

