use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, ConstValue, NodeId, SemanticInfo};
use crate::ir::TypeId;
use crate::typectx::T_BYTES;

use super::super::aliases::force_value;
use super::super::numeric::{is_float, quantize_float, wrap_int};
use super::super::values::{as_bool, coerce_value_to_type, truthy};
use super::ops::{eval_arith, eval_cmp, eval_eq_ne, eval_neg, ArithKind, CmpKind, EqKind};

fn expr_tid(info: &SemanticInfo, e: &Expr) -> Result<TypeId, CompileError> {
    info.expr_types
        .get(&NodeId(e.span))
        .copied()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                e.span,
                "missing semantic type for const initializer expression",
            )
        })
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
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(
                    t, *x as f64,
                ))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x as i64))))
            }
        }
        ExprKind::I8Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(
                    t, *x as f64,
                ))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x as i64))))
            }
        }
        ExprKind::I16Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(
                    t, *x as f64,
                ))))
            } else {
                Ok(ConstInit::Value(ConstValue::Int(wrap_int(t, *x as i64))))
            }
        }
        ExprKind::I64Lit(x) => {
            let t = expr_tid(info, e)?;
            if is_float(t) {
                Ok(ConstInit::Value(ConstValue::Float(quantize_float(
                    t, *x as f64,
                ))))
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
            Ok(ConstInit::Value(ConstValue::Float(quantize_float(
                t, *x as f64,
            ))))
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
            let v = force_value(
                eval_const_expr(inner, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            Ok(ConstInit::Value(ConstValue::Bool(truthy(&v))))
        }
        ExprKind::Not(inner) => {
            let v = force_value(
                eval_const_expr(inner, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            Ok(ConstInit::Value(ConstValue::Bool(!as_bool(&v, e.span)?)))
        }
        ExprKind::Neg(inner) => {
            let t = expr_tid(info, e)?;
            let v = force_value(
                eval_const_expr(inner, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            eval_neg(t, v, e.span)
        }
        ExprKind::Add(a, b) | ExprKind::Sub(a, b) | ExprKind::Mul(a, b) | ExprKind::Div(a, b) => {
            let t = expr_tid(info, e)?;
            let va = force_value(
                eval_const_expr(a, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let vb = force_value(
                eval_const_expr(b, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;

            let kind = match &e.node {
                ExprKind::Add(_, _) => ArithKind::Add,
                ExprKind::Sub(_, _) => ArithKind::Sub,
                ExprKind::Mul(_, _) => ArithKind::Mul,
                ExprKind::Div(_, _) => ArithKind::Div,
                _ => unreachable!(),
            };
            eval_arith(t, kind, va, vb, e.span)
        }
        ExprKind::Eq(a, b) | ExprKind::Ne(a, b) => {
            let va = force_value(
                eval_const_expr(a, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let vb = force_value(
                eval_const_expr(b, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let kind = match &e.node {
                ExprKind::Eq(_, _) => EqKind::Eq,
                ExprKind::Ne(_, _) => EqKind::Ne,
                _ => unreachable!(),
            };
            eval_eq_ne(kind, va, vb, e.span)
        }
        ExprKind::Lt(a, b) | ExprKind::Le(a, b) | ExprKind::Gt(a, b) | ExprKind::Ge(a, b) => {
            let va = force_value(
                eval_const_expr(a, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let vb = force_value(
                eval_const_expr(b, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let kind = match &e.node {
                ExprKind::Lt(_, _) => CmpKind::Lt,
                ExprKind::Le(_, _) => CmpKind::Le,
                ExprKind::Gt(_, _) => CmpKind::Gt,
                ExprKind::Ge(_, _) => CmpKind::Ge,
                _ => unreachable!(),
            };
            eval_cmp(kind, va, vb, e.span)
        }
        ExprKind::And(a, b) => {
            let va = force_value(
                eval_const_expr(a, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let aa = as_bool(&va, e.span)?;
            if !aa {
                return Ok(ConstInit::Value(ConstValue::Bool(false)));
            }
            let vb = force_value(
                eval_const_expr(b, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            Ok(ConstInit::Value(ConstValue::Bool(as_bool(&vb, e.span)?)))
        }
        ExprKind::Or(a, b) => {
            let va = force_value(
                eval_const_expr(a, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            let aa = as_bool(&va, e.span)?;
            if aa {
                return Ok(ConstInit::Value(ConstValue::Bool(true)));
            }
            let vb = force_value(
                eval_const_expr(b, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            Ok(ConstInit::Value(ConstValue::Bool(as_bool(&vb, e.span)?)))
        }
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => {
            let vc = force_value(
                eval_const_expr(cond, info, lookup_const)?,
                e.span,
                lookup_const,
            )?;
            if as_bool(&vc, e.span)? {
                eval_const_expr(then_br, info, lookup_const)
            } else {
                eval_const_expr(else_br, info, lookup_const)
            }
        }
        // Not allowed in v1 const initializers.
        ExprKind::Let { .. }
        | ExprKind::Member { .. }
        | ExprKind::Call { .. }
        | ExprKind::TypeApp { .. }
        | ExprKind::ArrayLit(_)
        | ExprKind::TupleLit(_)
        | ExprKind::ObjLit(_)
        | ExprKind::Index { .. }
        | ExprKind::IndexAssign { .. }
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
