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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, ConstValue};
use crate::ir::TypeId;

use super::super::numeric::{is_float, is_int, quantize_float};
use super::super::values::{as_float, as_int};

#[derive(Clone, Copy)]
pub(super) enum ArithKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
}

#[derive(Clone, Copy)]
pub(super) enum EqKind {
    Eq,
    Ne,
}

#[derive(Clone, Copy)]
pub(super) enum CmpKind {
    Lt,
    Le,
    Gt,
    Ge,
}

pub(super) fn eval_neg(t: TypeId, v: ConstValue, span: Span) -> Result<ConstInit, CompileError> {
    if is_int(t) {
        let x = as_int(&v, span)?;
        let out = match t {
            crate::typectx::T_I8 => (-(x as i8)) as i64,
            crate::typectx::T_I16 => (-(x as i16)) as i64,
            crate::typectx::T_I32 => (-(x as i32)) as i64,
            crate::typectx::T_I64 => (-(x as i64)) as i64,
            _ => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "bad integer type for '-'",
                ));
            }
        };
        Ok(ConstInit::Value(ConstValue::Int(out)))
    } else if is_float(t) {
        let x = as_float(&v, span)?;
        Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, -x))))
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            span,
            "unary '-' expects numeric constant",
        ))
    }
}

pub(super) fn eval_arith(
    t: TypeId,
    kind: ArithKind,
    va: ConstValue,
    vb: ConstValue,
    span: Span,
) -> Result<ConstInit, CompileError> {
    if is_int(t) {
        let aa = as_int(&va, span)?;
        let bb = as_int(&vb, span)?;

        if matches!(kind, ArithKind::Div) {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "division in const initializers must produce a float",
            ));
        }

        let out = match (kind, t) {
            (ArithKind::Add, crate::typectx::T_I8) => (aa as i8).wrapping_add(bb as i8) as i64,
            (ArithKind::Add, crate::typectx::T_I16) => (aa as i16).wrapping_add(bb as i16) as i64,
            (ArithKind::Add, crate::typectx::T_I32) => (aa as i32).wrapping_add(bb as i32) as i64,
            (ArithKind::Add, crate::typectx::T_I64) => (aa as i64).wrapping_add(bb as i64) as i64,
            (ArithKind::Sub, crate::typectx::T_I8) => (aa as i8).wrapping_sub(bb as i8) as i64,
            (ArithKind::Sub, crate::typectx::T_I16) => (aa as i16).wrapping_sub(bb as i16) as i64,
            (ArithKind::Sub, crate::typectx::T_I32) => (aa as i32).wrapping_sub(bb as i32) as i64,
            (ArithKind::Sub, crate::typectx::T_I64) => (aa as i64).wrapping_sub(bb as i64) as i64,
            (ArithKind::Mul, crate::typectx::T_I8) => (aa as i8).wrapping_mul(bb as i8) as i64,
            (ArithKind::Mul, crate::typectx::T_I16) => (aa as i16).wrapping_mul(bb as i16) as i64,
            (ArithKind::Mul, crate::typectx::T_I32) => (aa as i32).wrapping_mul(bb as i32) as i64,
            (ArithKind::Mul, crate::typectx::T_I64) => (aa as i64).wrapping_mul(bb as i64) as i64,
            (ArithKind::Mod, crate::typectx::T_I8) => (aa as i8 % bb as i8) as i64,
            (ArithKind::Mod, crate::typectx::T_I16) => (aa as i16 % bb as i16) as i64,
            (ArithKind::Mod, crate::typectx::T_I32) => (aa as i32 % bb as i32) as i64,
            (ArithKind::Mod, crate::typectx::T_I64) => (aa as i64 % bb as i64) as i64,
            (ArithKind::Shl, crate::typectx::T_I8) => ((aa as i8 as u8) << (bb as u32 & 7)) as i8 as i64,
            (ArithKind::Shl, crate::typectx::T_I16) => ((aa as i16 as u16) << (bb as u32 & 15)) as i16 as i64,
            (ArithKind::Shl, crate::typectx::T_I32) => ((aa as i32 as u32) << (bb as u32 & 31)) as i32 as i64,
            (ArithKind::Shl, crate::typectx::T_I64) => ((aa as i64 as u64) << (bb as u32 & 63)) as i64,
            (ArithKind::Shr, crate::typectx::T_I8) => ((aa as i8) >> (bb as i32 & 7)) as i64,
            (ArithKind::Shr, crate::typectx::T_I16) => ((aa as i16) >> (bb as i32 & 15)) as i64,
            (ArithKind::Shr, crate::typectx::T_I32) => (aa as i32 >> (bb as i32 & 31)) as i64,
            (ArithKind::Shr, crate::typectx::T_I64) => aa as i64 >> (bb as i32 & 63),
            _ => {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "bad integer binop type",
                ));
            }
        };
        Ok(ConstInit::Value(ConstValue::Int(out)))
    } else if is_float(t) {
        if matches!(kind, ArithKind::Mod | ArithKind::Shl | ArithKind::Shr) {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "modulo and shift require integer operands",
            ));
        }
        let aa = as_float(&va, span)?;
        let bb = as_float(&vb, span)?;
        let out = match kind {
            ArithKind::Add => aa + bb,
            ArithKind::Sub => aa - bb,
            ArithKind::Mul => aa * bb,
            ArithKind::Div => aa / bb,
            ArithKind::Mod | ArithKind::Shl | ArithKind::Shr => unreachable!(),
        };
        Ok(ConstInit::Value(ConstValue::Float(quantize_float(t, out))))
    } else {
        Err(CompileError::new(
            ErrorKind::Type,
            span,
            "binary op expects numeric constants",
        ))
    }
}

pub(super) fn eval_eq_ne(
    kind: EqKind,
    va: ConstValue,
    vb: ConstValue,
    span: Span,
) -> Result<ConstInit, CompileError> {
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
                span,
                "type mismatch in const equality",
            ));
        }
    };
    let out = match kind {
        EqKind::Eq => eq,
        EqKind::Ne => !eq,
    };
    Ok(ConstInit::Value(ConstValue::Bool(out)))
}

pub(super) fn eval_cmp(
    kind: CmpKind,
    va: ConstValue,
    vb: ConstValue,
    span: Span,
) -> Result<ConstInit, CompileError> {
    let out = match (&va, &vb) {
        (ConstValue::Int(x), ConstValue::Int(y)) => match kind {
            CmpKind::Lt => x < y,
            CmpKind::Le => x <= y,
            CmpKind::Gt => x > y,
            CmpKind::Ge => x >= y,
        },
        (ConstValue::Float(x), ConstValue::Float(y)) => match kind {
            CmpKind::Lt => x < y,
            CmpKind::Le => x <= y,
            CmpKind::Gt => x > y,
            CmpKind::Ge => x >= y,
        },
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                span,
                "comparison expects numeric constants",
            ));
        }
    };
    Ok(ConstInit::Value(ConstValue::Bool(out)))
}
