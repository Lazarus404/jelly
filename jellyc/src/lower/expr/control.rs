/**
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

// Control-flow expression lowering (block, if, try).

use std::collections::HashMap;

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use crate::lower::{lower_stmt, Binding, LowerCtx};

use super::lower_expr;
use super::lower_truthy;
use super::T_BOOL;
use super::T_DYNAMIC;

pub fn lower_block_expr(
    stmts: &[crate::ast::Stmt],
    expr: &crate::ast::Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    ctx.env_stack.push(HashMap::new());
    for st in stmts {
        lower_stmt(st, ctx, b)?;
    }
    let out = lower_expr(expr, ctx, b)?;
    ctx.env_stack.pop();
    Ok(out)
}

pub fn lower_if_expr(
    e: &crate::ast::Expr,
    cond: &crate::ast::Expr,
    then_br: &crate::ast::Expr,
    else_br: &crate::ast::Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (v_cond, t_cond) = lower_expr(cond, ctx, b)?;
    let v_cond = if t_cond == T_BOOL {
        v_cond
    } else {
        lower_truthy(cond.span, v_cond, t_cond, ctx, b)?
    };

    let then_b = b.new_block(Some("if_then".to_string()));
    let else_b = b.new_block(Some("if_else".to_string()));
    let join_b = b.new_block(Some("if_join".to_string()));

    b.term(IrTerminator::JmpIf {
        cond: v_cond,
        then_tgt: then_b,
        else_tgt: else_b,
    });

    b.set_block(then_b);
    let (v_then, t_then) = lower_expr(then_br, ctx, b)?;
    let then_end = b.cur_block();
    let then_reaches_join = b.is_open();
    if then_reaches_join {
        b.term(IrTerminator::Jmp { target: join_b });
    }

    b.set_block(else_b);
    let (v_else, t_else) = lower_expr(else_br, ctx, b)?;
    let else_end = b.cur_block();
    let else_reaches_join = b.is_open();
    if else_reaches_join {
        b.term(IrTerminator::Jmp { target: join_b });
    }

    if t_then != t_else {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "if branches must return same type",
        ));
    }

    b.set_block(join_b);
    let v_res = if then_reaches_join && else_reaches_join {
        let v = b.new_vreg(t_then);
        b.emit(
            e.span,
            IrOp::Phi {
                dst: v,
                incomings: vec![(then_end, v_then), (else_end, v_else)],
            },
        );
        v
    } else if then_reaches_join {
        let v = b.new_vreg(t_then);
        b.emit(
            e.span,
            IrOp::Phi {
                dst: v,
                incomings: vec![(then_end, v_then)],
            },
        );
        v
    } else if else_reaches_join {
        let v = b.new_vreg(t_else);
        b.emit(
            e.span,
            IrOp::Phi {
                dst: v,
                incomings: vec![(else_end, v_else)],
            },
        );
        v
    } else {
        // Neither branch reaches join (both return/break/etc); join is unreachable
        let v = b.new_vreg(t_then);
        b.emit(
            e.span,
            IrOp::Phi {
                dst: v,
                incomings: vec![(then_end, v_then)],
            },
        );
        v
    };
    Ok((v_res, t_then))
}

pub fn lower_try_expr(
    e: &crate::ast::Expr,
    body: &crate::ast::Expr,
    catch_name: &Option<String>,
    catch_body: &crate::ast::Expr,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let catch_b = b.new_block(Some("catch".to_string()));
    let join_b = b.new_block(Some("try_join".to_string()));

    let v_exc = b.new_vreg(T_DYNAMIC);
    b.emit(
        e.span,
        IrOp::Try {
            catch_dst: v_exc,
            catch_block: catch_b,
            trap_only: catch_name.is_none(),
        },
    );

    let (v_body, t_body) = lower_expr(body, ctx, b)?;
    b.emit(e.span, IrOp::EndTry);
    let body_end = b.cur_block();
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(catch_b);
    ctx.env_stack.push(HashMap::new());
    if let Some(n) = catch_name {
        ctx.env_stack
            .last_mut()
            .expect("env stack")
            .insert(n.clone(), Binding { v: v_exc, tid: T_DYNAMIC });
    }
    let (v_catch, t_catch) = lower_expr(catch_body, ctx, b)?;
    ctx.env_stack.pop();
    if t_catch != t_body {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "try and catch must return same type",
        ));
    }
    let catch_end = b.cur_block();
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(join_b);
    let v_res = b.new_vreg(t_body);
    b.emit(
        e.span,
        IrOp::Phi {
            dst: v_res,
            incomings: vec![(body_end, v_body), (catch_end, v_catch)],
        },
    );
    Ok((v_res, t_body))
}
