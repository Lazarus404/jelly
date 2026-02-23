use std::collections::HashMap;

use crate::ast::{Expr, Stmt};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator};

use crate::lower::expr::{lower_expr, lower_truthy};
use crate::lower::{ensure_open_block, lookup_var, LoopTargets, LowerCtx, T_BOOL};

pub(super) fn lower_while_stmt(
    _s: &Stmt,
    cond: &Expr,
    body: &[Stmt],
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let from = b.cur_block();
    let cond_b = b.new_block(Some("while_cond".to_string()));
    let body_b = b.new_block(Some("while_body".to_string()));
    let latch_b = b.new_block(Some("while_latch".to_string()));
    let exit_b = b.new_block(Some("while_exit".to_string()));

    b.set_block(from);
    b.term(IrTerminator::Jmp { target: cond_b });

    b.set_block(body_b);
    ctx.loop_stack.push(LoopTargets {
        break_tgt: exit_b,
        continue_tgt: latch_b,
    });
    ctx.with_env_scope(|ctx| {
        for st in body {
            super::super::lower_stmt(st, ctx, b)?;
        }
        Ok(())
    })?;
    ctx.loop_stack.pop();

    ensure_open_block(b);
    let tail = b.cur_block();
    b.set_block(tail);
    b.term(IrTerminator::Jmp { target: latch_b });

    b.set_block(latch_b);
    b.term(IrTerminator::Jmp { target: cond_b });

    let assigned = super::super::vars_assigned_in(body);
    let used_in_cond = super::super::vars_used_in_expr(cond);
    let loop_vars: Vec<String> = assigned
        .intersection(&used_in_cond)
        .filter(|name| ctx.env_stack.iter().rev().any(|m| m.contains_key(*name)))
        .cloned()
        .collect();

    b.set_block(cond_b);
    let mut cond_env = HashMap::new();
    for name in &loop_vars {
        let bd = lookup_var(ctx, name, cond.span)?;
        let phi_dst = b.new_vreg(bd.tid);
        b.emit(
            crate::ast::Span::point(0),
            IrOp::Phi {
                dst: phi_dst,
                incomings: vec![(from, bd.v), (latch_b, bd.v)],
            },
        );
        cond_env.insert(
            name.clone(),
            crate::lower::Binding {
                v: phi_dst,
                tid: bd.tid,
                func_index: bd.func_index,
            },
        );
    }
    let (v_cond, t_cond) = ctx.with_env(cond_env, |ctx| lower_expr(cond, ctx, b))?;
    let v_cond = if t_cond == T_BOOL {
        v_cond
    } else {
        lower_truthy(cond.span, v_cond, t_cond, ctx, b)?
    };
    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: body_b,
        else_tgt: exit_b,
    });

    b.set_block(exit_b);
    Ok(())
}

pub(super) fn lower_break_stmt(
    s: &Stmt,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let tgt = ctx
        .loop_stack
        .last()
        .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "break used outside of while"))?
        .break_tgt;
    b.term(IrTerminator::Jmp { target: tgt });
    let nb = b.new_block(Some("after_break".to_string()));
    b.set_block(nb);
    Ok(())
}

pub(super) fn lower_continue_stmt(
    s: &Stmt,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    ensure_open_block(b);
    let tgt = ctx
        .loop_stack
        .last()
        .ok_or_else(|| {
            CompileError::new(ErrorKind::Name, s.span, "continue used outside of while")
        })?
        .continue_tgt;
    b.term(IrTerminator::Jmp { target: tgt });
    let nb = b.new_block(Some("after_continue".to_string()));
    b.set_block(nb);
    Ok(())
}
