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

// Function literal lowering.

use std::collections::HashMap;

use crate::error::{CompileError, ErrorKind};
use crate::hir::NodeId;
use crate::ir::{BlockId, IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use crate::lower::{lower_stmt, Binding, FnCtx, LowerCtx};

use super::lower_expr;

pub fn lower_fn_expr(
    e: &crate::ast::Expr,
    params: &[(String, Option<crate::ast::Ty>)],
    body: &[crate::ast::Stmt],
    tail: &Option<Box<crate::ast::Expr>>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let expect_tid = ctx
        .sem_expr_types
        .get(&NodeId(e.span))
        .copied()
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing semantic type for function literal"))?;

    let te = ctx
        .type_ctx
        .types
        .get(expect_tid as usize)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad function type id"))?;
    if te.kind != crate::jlyb::TypeKind::Function {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "expected a function type annotation",
        ));
    }
    let (sig_args, sig_ret) = {
        let sig = ctx
            .type_ctx
            .sigs
            .get(te.p0 as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?;
        (sig.args.clone(), sig.ret_type)
    };
    if sig_args.len() != params.len() {
        return Err(CompileError::new(
            ErrorKind::Type,
            e.span,
            "function param arity mismatch",
        ));
    }

    // Logical index: 0=native, 1..4=prelude, 5=main, 6+=nested. Add 1 for native slot.
    //
    // IMPORTANT: use a monotonic counter; `nested_funcs.len()` is not safe because we can lower
    // a nested lambda while its parent lambda is still being built (and thus not yet pushed).
    let func_index =
        crate::jlyb::PRELUDE_FUN_COUNT + 1 + ctx.user_top_level_fun_count + ctx.next_nested_fun_index;
    ctx.next_nested_fun_index += 1;
    let mut fb = IrBuilder::new(Some(format!("lambda{}", func_index)));
    fb.func.param_count = params.len() as u8;

    let saved_env = std::mem::replace(&mut ctx.env_stack, vec![HashMap::new()]);
    let saved_loops = std::mem::take(&mut ctx.loop_stack);
    let saved_fn = std::mem::take(&mut ctx.fn_stack);

    ctx.loop_stack = Vec::new();
    ctx.fn_stack = vec![FnCtx {
        ret_tid: sig_ret,
        outer_env: Some(saved_env.clone()),
        captures: HashMap::new(),
        capture_order: Vec::new(),
        self_name: ctx.pending_fn_self.as_ref().map(|(n, _)| n.clone()),
        self_func_index: ctx.pending_fn_self.as_ref().map(|_| func_index),
        self_fun_tid: ctx.pending_fn_self.as_ref().map(|(_, tid)| *tid),
        self_loop_head: BlockId(0),
    }];

    // Bind params to vregs 0..n-1
    for (i, (name, _ann)) in params.iter().enumerate() {
        let tid = sig_args[i];
        let v = fb.new_vreg(tid);
        ctx.env_stack
            .last_mut()
            .expect("env stack")
            .insert(name.clone(), Binding { v, tid });
    }

    // Recursion sugar for `let f: T = fn(...) { ... f(...) ... }`:
    // inside the function body, bind `f` to a `CONST_FUN` of the function itself.
    if let Some((self_name, self_tid)) = ctx.pending_fn_self.clone() {
        if self_tid != expect_tid {
            return Err(CompileError::new(
                ErrorKind::Internal,
                e.span,
                "semantic type mismatch for self-recursive fn literal",
            ));
        }
        let env = ctx.env_stack.last_mut().expect("env stack");
        if !env.contains_key(&self_name) {
            let vself = fb.new_vreg(expect_tid);
            fb.emit(e.span, IrOp::ConstFun {
                dst: vself,
                func_index,
            });
            env.insert(self_name, Binding {
                v: vself,
                tid: expect_tid,
            });
        }
    }

    // If this function has a self-binding, keep prologue in block0 and lower the body
    // in a fresh loop header block so tail-self-calls can jump there without re-running
    // the prologue (avoids allocating a fresh function object per iteration).
    if ctx.pending_fn_self.is_some() {
        let head = fb.new_block(Some("fn_body".to_string()));
        if fb.is_open() {
            fb.term(IrTerminator::Jmp { target: head });
        }
        fb.set_block(head);
        if let Some(top) = ctx.fn_stack.last_mut() {
            top.self_loop_head = head;
        }
    }

    for st in body {
        lower_stmt(st, ctx, &mut fb)?;
    }

    if fb.is_open() {
        if let Some(t) = tail {
            let (v, tid) = lower_expr(t, ctx, &mut fb)?;
            if tid != sig_ret {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    t.span,
                    "function tail type mismatch",
                ));
            }
            fb.term(IrTerminator::Ret { value: v });
        } else {
            return Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "missing return",
            ));
        }
    }

    // Extract capture info from the fn lowering context.
    let fcx = ctx
        .fn_stack
        .pop()
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing fn ctx"))?;
    let mut cap_slots: Vec<VRegId> = Vec::new();
    let mut cap_srcs: Vec<Binding> = Vec::new();
    for name in &fcx.capture_order {
        let (ob, slot) = *fcx
            .captures
            .get(name)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "missing capture entry"))?;
        cap_slots.push(slot);
        cap_srcs.push(ob);
    }
    fb.func.cap_vregs = cap_slots;

    // Restore outer context
    ctx.env_stack = saved_env;
    ctx.loop_stack = saved_loops;
    ctx.fn_stack = saved_fn;
    ctx.pending_fn_self = None;

    // Emit the function value in the outer function.
    let out = b.new_vreg(expect_tid);
    if cap_srcs.is_empty() {
        b.emit(e.span, IrOp::ConstFun {
            dst: out,
            func_index,
        });
    } else {
        // Marshal captures into a contiguous vreg window so `CLOSURE` can box them.
        let cap_sig_args: Vec<TypeId> = cap_srcs.iter().map(|bd| bd.tid).collect();
        let cap_sig_id = ctx.type_ctx.intern_sig(super::super::T_DYNAMIC, &cap_sig_args);

        let cap_base = b.new_vreg(cap_srcs[0].tid);
        b.emit(e.span, IrOp::Mov {
            dst: cap_base,
            src: cap_srcs[0].v,
        });
        for bd in &cap_srcs[1..] {
            let slot = b.new_vreg(bd.tid);
            b.emit(e.span, IrOp::Mov { dst: slot, src: bd.v });
        }
        b.emit(
            e.span,
            IrOp::Closure {
                dst: out,
                func_index,
                cap_sig_id,
                cap_base,
                ncaps: cap_srcs.len() as u8,
            },
        );
    }
    ctx.nested_funcs.push(fb.func);
    Ok((out, expect_tid))
}
