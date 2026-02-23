use crate::error::{CompileError, ErrorKind};
use crate::hir::ConstValue;
use crate::ir::TypeId;
use crate::typectx::{T_BYTES, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

use super::numeric::{quantize_float, wrap_int};

pub(super) fn as_bool(v: &ConstValue, span: crate::ast::Span) -> Result<bool, CompileError> {
    match v {
        ConstValue::Bool(b) => Ok(*b),
        _ => Err(CompileError::new(
            ErrorKind::Type,
            span,
            "expected Bool constant",
        )),
    }
}

pub(super) fn as_int(v: &ConstValue, span: crate::ast::Span) -> Result<i64, CompileError> {
    match v {
        ConstValue::Int(x) => Ok(*x),
        _ => Err(CompileError::new(
            ErrorKind::Type,
            span,
            "expected integer constant",
        )),
    }
}

pub(super) fn as_float(v: &ConstValue, span: crate::ast::Span) -> Result<f64, CompileError> {
    match v {
        ConstValue::Float(x) => Ok(*x),
        _ => Err(CompileError::new(
            ErrorKind::Type,
            span,
            "expected float constant",
        )),
    }
}

pub(super) fn truthy(v: &ConstValue) -> bool {
    match v {
        ConstValue::Bool(false) => false,
        ConstValue::Null => false,
        _ => true,
    }
}

pub(super) fn coerce_value_to_type(
    t: TypeId,
    v: ConstValue,
    span: crate::ast::Span,
) -> Result<ConstValue, CompileError> {
    match t {
        T_I8 | T_I16 | T_I32 | T_I64 => match v {
            ConstValue::Int(x) => Ok(ConstValue::Int(wrap_int(t, x))),
            ConstValue::Float(x) => Ok(ConstValue::Int(wrap_int(t, x as i64))),
            _ => Err(CompileError::new(
                ErrorKind::Type,
                span,
                "expected numeric constant",
            )),
        },
        T_F16 | T_F32 | T_F64 => match v {
            ConstValue::Int(x) => Ok(ConstValue::Float(quantize_float(t, x as f64))),
            ConstValue::Float(x) => Ok(ConstValue::Float(quantize_float(t, x))),
            _ => Err(CompileError::new(
                ErrorKind::Type,
                span,
                "expected numeric constant",
            )),
        },
        T_BYTES => match v {
            ConstValue::Bytes(b) => Ok(ConstValue::Bytes(b)),
            ConstValue::Null => Ok(ConstValue::Null),
            _ => Err(CompileError::new(
                ErrorKind::Type,
                span,
                "expected Bytes constant",
            )),
        },
        _ => Ok(v),
    }
}
