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

use crate::ast::Stmt;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp};

use super::super::super::{bind_local, ensure_open_block, intern_atom, lookup_var, LowerCtx};

pub(super) fn lower_import_module_stmt(
    s: &Stmt,
    key: &str,
    alias: &str,
    ctx: &mut LowerCtx,
) -> Result<(), CompileError> {
    let canon = ctx.module_key_to_alias.get(key).cloned().ok_or_else(|| {
        CompileError::new(ErrorKind::Name, s.span, format!("unknown module '{}'", key))
    })?;
    let bd = lookup_var(ctx, &canon, s.span)?;
    if canon.as_str() != alias {
        bind_local(ctx, alias, bd.v, bd.tid);
    }
    Ok(())
}

pub(super) fn lower_import_from_stmt(
    s: &Stmt,
    items: &[(String, Option<String>)],
    key: &str,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);

    let canon = ctx.module_key_to_alias.get(key).cloned().ok_or_else(|| {
        CompileError::new(ErrorKind::Name, s.span, format!("unknown module '{}'", key))
    })?;
    let mod_bd = lookup_var(ctx, &canon, s.span)?;

    for (name, alias) in items {
        let bind = alias.as_ref().unwrap_or(name);
        let tid = ctx
            .module_alias_exports
            .get(&canon)
            .and_then(|m| m.get(name))
            .copied()
            .ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Name,
                    s.span,
                    format!("unknown export '{}.{}'", canon, name),
                )
            })?;
        let atom_id = intern_atom(name, ctx);
        let out = b.new_vreg(tid);
        b.emit(
            s.span,
            IrOp::ObjGetAtom {
                dst: out,
                obj: mod_bd.v,
                atom_id,
            },
        );
        bind_local(ctx, bind, out, tid);
        // REPL: when importing from __repl_session__, track so assignments update the session.
        if key == "__repl_session__" {
            ctx.session_obj = Some(mod_bd.v);
            ctx.session_imported_names.insert(bind.clone());
        }
    }
    Ok(())
}
