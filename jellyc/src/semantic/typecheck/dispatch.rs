use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16,
    T_I32, T_I64, T_I8, T_LIST_BYTES, T_LIST_I32,
};

use super::{call, control, fn_, index, is_numeric, lit, match_, member, new_, op, TypeChecker};

pub(super) fn check_expr_impl(
    tc: &mut TypeChecker,
    e: &Expr,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    match &e.node {
        ExprKind::BytesLit(_) => Ok(T_BYTES),
        ExprKind::BoolLit(_) => Ok(T_BOOL),
        ExprKind::I32Lit(x) => {
            if expect == Some(T_I8) && *x >= -128 && *x <= 127 {
                Ok(T_I8)
            } else if expect == Some(T_I16) && *x >= -32768 && *x <= 32767 {
                Ok(T_I16)
            } else {
                Ok(T_I32)
            }
        }
        ExprKind::I8Lit(_) => Ok(T_I8),
        ExprKind::I16Lit(_) => Ok(T_I16),
        ExprKind::I64Lit(_) => Ok(T_I64),
        ExprKind::F16Lit(_) => Ok(T_F16),
        ExprKind::F64Lit(_) => {
            if expect == Some(T_F64) {
                Ok(T_F64)
            } else if expect == Some(T_F16) {
                Ok(T_F16)
            } else {
                Ok(T_F32)
            }
        }
        ExprKind::AtomLit(_) => Ok(T_ATOM),
        ExprKind::Null => {
            // Represented as Dynamic unless expected to flow to an object-kind type.
            if let Some(et) = expect {
                if et == T_BYTES
                    || et == T_ARRAY_I32
                    || et == T_ARRAY_BYTES
                    || et == T_LIST_I32
                    || et == T_LIST_BYTES
                    || tc.is_object_kind(et)
                {
                    return Ok(et);
                }
            }
            Ok(T_DYNAMIC)
        }
        ExprKind::Var(name) => {
            let bt = tc.lookup(name).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Name,
                    e.span,
                    format!("unknown variable '{}'", name),
                )
            })?;
            if let Some(et) = expect {
                return tc.coerce_type(bt, et, e.span);
            }
            Ok(bt)
        }
        ExprKind::Member { base, name } => member::type_member(tc, e, base, name.as_str(), expect),
        ExprKind::Call {
            callee,
            type_args,
            args,
        } => call::type_call(tc, e, callee, type_args, args, expect),
        ExprKind::TypeApp { .. } => Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "type application must be expanded before semantic analysis",
        )),
        ExprKind::ArrayLit(elems) => lit::type_array_lit(tc, e, elems, expect),
        ExprKind::TupleLit(elems) => lit::type_tuple_lit(tc, elems),
        ExprKind::ObjLit(fields) => lit::type_obj_lit(tc, fields, expect),
        ExprKind::Index { base, index } => index::type_index(tc, e, base, index),
        ExprKind::IndexAssign { base, index, expr } => {
            index::type_index_assign(tc, e, base, index, expr)
        }
        ExprKind::Fn { params, body, tail } => fn_::type_fn(tc, e, params, body, tail, expect),
        ExprKind::Truthy(x) => {
            let _ = tc.check_expr(x, None)?;
            Ok(T_BOOL)
        }
        ExprKind::Not(x) => {
            let t = tc.check_expr(x, Some(T_BOOL))?;
            if t != T_BOOL {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "'!' expects bool",
                ));
            }
            Ok(T_BOOL)
        }
        ExprKind::Neg(x) => {
            let t = tc.check_expr(x, None)?;
            if !is_numeric(t) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "unary '-' expects numeric",
                ));
            }
            Ok(t)
        }
        ExprKind::Add(a, b) => op::type_add(tc, e, a, b, expect),
        ExprKind::Sub(a, b) | ExprKind::Mul(a, b) => op::type_sub_mul(tc, e, a, b, expect),
        ExprKind::Div(a, b) => op::type_div(tc, e, a, b, expect),
        ExprKind::Eq(a, b) | ExprKind::Ne(a, b) => op::type_eq_ne(tc, e, a, b),
        ExprKind::Lt(a, b) | ExprKind::Le(a, b) | ExprKind::Gt(a, b) | ExprKind::Ge(a, b) => {
            op::type_rel(tc, e, a, b)
        }
        ExprKind::And(a, b) | ExprKind::Or(a, b) => op::type_and_or(tc, a, b),
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => control::type_if(tc, e, cond, then_br, else_br, expect),
        ExprKind::Block { stmts, expr } => control::type_block(tc, stmts, expr, expect),
        ExprKind::Let {
            is_const,
            name,
            type_params,
            ty,
            expr: init,
        } => super::expr_let::type_let_expr(tc, e, *is_const, name, type_params, ty.as_ref(), init, expect),
        ExprKind::Try {
            body,
            catch_name,
            catch_body,
        } => control::type_try(tc, e, body, catch_name, catch_body, expect),
        ExprKind::Match { subject, arms } => match_::type_match(tc, e, subject, arms, expect),
        ExprKind::New { proto, args } => new_::type_new(tc, e, proto, args, expect),
    }
}
