use crate::ast::{Span, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, TypeId, VRegId};

use super::LowerCtx;

#[derive(Clone, Copy)]
pub(crate) struct Binding {
    pub v: VRegId,
    pub tid: TypeId,
    /// When the value is from ConstFun (e.g. fn literal with no captures), the func index.
    pub func_index: Option<u32>,
}

pub(crate) fn lookup_var(ctx: &LowerCtx, name: &str, span: Span) -> Result<Binding, CompileError> {
    ctx.env_stack
        .iter()
        .rev()
        .find_map(|m| m.get(name))
        .copied()
        .ok_or_else(|| {
            CompileError::new(
                ErrorKind::Name,
                span,
                format!("unknown variable '{}'", name),
            )
        })
}

pub(crate) fn bind_local(ctx: &mut LowerCtx, name: &str, v: VRegId, tid: TypeId) {
    bind_local_with_func_index(ctx, name, v, tid, None);
}

pub(crate) fn bind_local_with_func_index(
    ctx: &mut LowerCtx,
    name: &str,
    v: VRegId,
    tid: TypeId,
    func_index: Option<u32>,
) {
    ctx.env_stack.last_mut().expect("env stack").insert(
        name.to_string(),
        Binding {
            v,
            tid,
            func_index,
        },
    );
}

pub(crate) fn ensure_capture_binding(
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    name: &str,
    span: Span,
) -> Result<Option<Binding>, CompileError> {
    let Some(fc) = ctx.fn_stack.last_mut() else {
        return Ok(None);
    };
    let Some(outer_env) = fc.outer_env.as_ref() else {
        return Ok(None);
    };

    if ctx.env_stack.iter().rev().any(|m| m.contains_key(name)) {
        return Ok(None);
    }

    let outer = outer_env.iter().rev().find_map(|m| m.get(name)).copied();
    let Some(ob) = outer else {
        return Ok(None);
    };

    if let Some((_, _slot)) = fc.captures.get(name) {
        let bd = lookup_var(ctx, name, span)?;
        return Ok(Some(bd));
    }

    let slot = b.new_vreg(ob.tid);
    fc.captures.insert(name.to_string(), (ob, slot));
    fc.capture_order.push(name.to_string());
    bind_local(ctx, name, slot, ob.tid);
    Ok(Some(Binding {
        v: slot,
        tid: ob.tid,
        func_index: ob.func_index,
    }))
}

pub(crate) fn resolve_opt_ty(
    t: &Option<Ty>,
    ctx: &mut LowerCtx,
) -> Result<Option<TypeId>, CompileError> {
    match t {
        Some(ty) => Ok(Some(ctx.type_ctx.resolve_ty(ty)?)),
        None => Ok(None),
    }
}
