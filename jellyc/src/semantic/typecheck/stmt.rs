use crate::ast::{Expr, ExprKind, Stmt, StmtKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::typectx::{T_BYTES, T_DYNAMIC, T_I32, T_OBJECT};

use super::TypeChecker;

pub(super) fn check_stmt(tc: &mut TypeChecker, s: &Stmt) -> Result<(), CompileError> {
    match &s.node {
        StmtKind::ImportModule { alias, .. } => {
            tc.bind_local(alias, T_OBJECT);
            Ok(())
        }
        StmtKind::ImportFrom { items, .. } => {
            for (n, a) in items {
                let bind = a.as_deref().unwrap_or(n.as_str());
                // `typecheck_module_init` pre-binds these using module interfaces.
                if tc.lookup(bind).is_none() {
                    tc.bind_local(bind, T_DYNAMIC);
                }
            }
            Ok(())
        }
        StmtKind::Prototype { .. } => Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "prototype must be expanded before semantic analysis",
        )),
        StmtKind::Let {
            is_const,
            name,
            type_params,
            ty,
            expr,
            ..
        } => check_let(tc, s, *is_const, name, type_params, ty.as_ref(), expr),
        StmtKind::Assign { name, expr } => check_assign(tc, s, name, expr),
        StmtKind::While { cond, body } => check_while(tc, cond, body),
        StmtKind::Break | StmtKind::Continue => Ok(()),
        StmtKind::Throw { expr } => {
            let _ = tc.check_expr(expr, None)?;
            Ok(())
        }
        StmtKind::Return { expr } => check_return(tc, expr.as_ref()),
        StmtKind::Expr { expr } => {
            let _ = tc.check_expr(expr, None)?;
            Ok(())
        }
        StmtKind::MemberAssign { base, expr, .. } => check_member_assign(tc, s, base, expr),
        StmtKind::IndexAssign { base, index, expr } => check_index_assign(tc, s, base, index, expr),
    }
}

fn check_let(
    tc: &mut TypeChecker,
    s: &Stmt,
    is_const: bool,
    name: &str,
    type_params: &[String],
    ty: Option<&Ty>,
    expr: &Expr,
) -> Result<(), CompileError> {
    if !type_params.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "template lets must be expanded before semantic analysis",
        ));
    }
    let is_discard = name == "_";
    if is_discard {
        let _ = tc.check_expr(expr, None)?;
        if is_const {
            let _ = tc.eval_const_expr(expr)?;
        }
        return Ok(());
    }

    // Determine binding type.
    let bind_tid = if let Some(ann) = ty {
        tc.resolve_ann_tid(ann)?
    } else if let ExprKind::Fn { params, body, tail } = &expr.node {
        tc.infer_fn_tid_for_let(name, params, body, tail)?
    } else {
        // Default to the initializer's type when no annotation.
        tc.check_expr(expr, None)?
    };

    // Recursion sugar: let f = fn(...) { ... f(...) ... };
    if matches!(&expr.node, ExprKind::Fn { .. }) {
        tc.bind_local(name, bind_tid);
    }

    let init_tid = tc.check_expr(expr, Some(bind_tid))?;
    if init_tid != bind_tid {
        return Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "let initializer type mismatch",
        ));
    }
    if is_const {
        let init = tc.eval_const_expr(expr)?;
        tc.record_const_init(s.span, init.clone());
        tc.bind_const(name, init);
    }
    tc.record_binding(s.span, bind_tid);
    if !matches!(&expr.node, ExprKind::Fn { .. }) {
        tc.bind_local(name, bind_tid);
    }
    Ok(())
}

fn check_assign(
    tc: &mut TypeChecker,
    s: &Stmt,
    name: &str,
    expr: &Expr,
) -> Result<(), CompileError> {
    if tc.lookup_const(name).is_some() {
        return Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            format!("cannot assign to const binding '{}'", name),
        ));
    }
    let dst_tid = tc.lookup(name).ok_or_else(|| {
        CompileError::new(
            ErrorKind::Name,
            s.span,
            format!("unknown variable '{}'", name),
        )
    })?;
    let rhs_tid = tc.check_expr(expr, Some(dst_tid))?;
    if rhs_tid != dst_tid {
        return Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            format!("assignment to '{}' changes type", name),
        ));
    }
    Ok(())
}

fn check_while(tc: &mut TypeChecker, cond: &Expr, body: &[Stmt]) -> Result<(), CompileError> {
    // Jelly truthiness: loop conditions need not be `Bool` (everything except `null`
    // and `false` is truthy). Lowering will insert truthiness conversion if needed.
    let _ = tc.check_expr(cond, None)?;
    tc.with_scope(|tc| {
        for st in body {
            tc.check_stmt(st)?;
        }
        Ok(())
    })
}

fn check_return(tc: &mut TypeChecker, expr: Option<&Expr>) -> Result<(), CompileError> {
    if let Some(e) = expr {
        if let Some(ret_tid) = tc.current_ret_tid() {
            let got = tc.check_expr(e, Some(ret_tid))?;
            let coerced = tc.coerce_type(got, ret_tid, e.span)?;
            // Record the coerced type so lowering can be mechanical.
            tc.record_expr(e.span, coerced);
        } else {
            let _ = tc.check_expr(e, None)?;
        }
    }
    Ok(())
}

fn check_member_assign(
    tc: &mut TypeChecker,
    s: &Stmt,
    base: &Expr,
    expr: &Expr,
) -> Result<(), CompileError> {
    let t0 = tc.check_expr(base, None)?;
    let t_obj = if t0 == T_DYNAMIC {
        tc.check_expr(base, Some(T_OBJECT))?
    } else {
        t0
    };
    if !tc.is_object_kind(t_obj) {
        return Err(CompileError::new(
            ErrorKind::Type,
            s.span,
            "member assignment expects an Object",
        ));
    }
    let _ = tc.check_expr(expr, None)?;
    Ok(())
}

fn check_index_assign(
    tc: &mut TypeChecker,
    s: &Stmt,
    base: &Expr,
    index: &Expr,
    expr: &Expr,
) -> Result<(), CompileError> {
    let tb = tc.check_expr(base, None)?;
    let _ = tc.check_expr(index, Some(T_I32))?;
    match tb {
        crate::typectx::T_ARRAY_I32 => {
            let _ = tc.check_expr(expr, Some(T_I32))?;
        }
        crate::typectx::T_ARRAY_BYTES => {
            let _ = tc.check_expr(expr, Some(T_BYTES))?;
        }
        crate::typectx::T_BYTES => {
            let _ = tc.check_expr(expr, Some(T_I32))?;
        }
        _ => {
            return Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "index assignment not supported for this type yet",
            ));
        }
    }
    Ok(())
}
