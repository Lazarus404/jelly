use crate::ast::{Expr, Stmt, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::T_DYNAMIC;

use super::TypeChecker;

pub(super) fn type_fn(
    tc: &mut TypeChecker,
    e: &Expr,
    params: &[(String, Option<Ty>)],
    body: &[Stmt],
    tail: &Option<Box<Expr>>,
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let fun_tid = if let Some(et) = expect {
        et
    } else {
        let arg_tids = vec![T_DYNAMIC; params.len()];
        tc.intern_fun_type(T_DYNAMIC, &arg_tids)
    };
    let (sig_args, sig_ret) = tc.fun_sig(fun_tid, e.span)?;
    if sig_args.len() != params.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "fn param count does not match expected function type",
        ));
    }

    // Function literals see outer bindings (captures, self-recursion sugar).
    tc.with_scope(|tc| {
        tc.push_ret_tid(sig_ret);
        let res = (|| {
            for (i, (pn, ann)) in params.iter().enumerate() {
                let tid = if let Some(t) = ann {
                    tc.resolve_ann_tid(t)?
                } else {
                    sig_args[i]
                };
                tc.bind_local(pn, tid);
            }
            for st in body {
                tc.check_stmt(st)?;
            }
            if let Some(t) = tail {
                let got = tc.check_expr(t, Some(sig_ret))?;
                let coerced = tc.coerce_type(got, sig_ret, t.span)?;
                tc.record_expr(t.span, coerced);
            }
            Ok(())
        })();
        tc.pop_ret_tid();
        res
    })?;
    Ok(fun_tid)
}
