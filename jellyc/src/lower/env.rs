/*
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
