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
        let aa = as_float(&va, span)?;
        let bb = as_float(&vb, span)?;
        let out = match kind {
            ArithKind::Add => aa + bb,
            ArithKind::Sub => aa - bb,
            ArithKind::Mul => aa * bb,
            ArithKind::Div => aa / bb,
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
