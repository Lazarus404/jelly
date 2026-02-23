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
    }
    Ok(())
}
