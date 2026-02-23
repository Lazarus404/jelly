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

//! ## Codegen ABI / register layout (compiler ↔ VM contract)
//!
//! Jelly bytecode functions execute in a **typed** register frame with **≤256 physical regs**.
//! `jellyc`’s codegen is responsible for producing a `reg_types[]` table and an instruction stream
//! that satisfies the ISA rules in `docs/ISA.md` and the compiler↔VM layout invariants in
//! `docs/call_abi.md`.
//!
//! ### Fixed / pinned windows
//!
//! Some vregs are **pinned** to ABI-defined physical register windows and therefore must never be
//! moved or spilled:
//!
//! - **Callee params**: pregs `0..param_count-1` (fixed mapping).
//! - **Callee capture slots**: immediately after params; `cap_start` is set accordingly (or `0`
//!   when there are no captures).
//! - **Call arg windows**: each distinct call signature gets one contiguous physical arg block; at
//!   each call site, the IR’s contiguous arg window vregs are pinned into that block.
//! - **Closure capture windows** (`CLOSURE a b c imm`): the capture source window `rB..r(B+c-1)`
//!   must be contiguous and is treated as a pinned window (so spills cannot corrupt capture args).
//!
//! Additional invariants:
//!
//! - **CALLR callee never spills**: the closure/function object in `rB:function` for `CALLR` is
//!   excluded from spilling (spilling/reloading can corrupt the callee object).
//! - **Spill is boxed-only**: spill ops operate only on `Dynamic` regs; codegen reserves 3 dedicated
//!   `Dynamic` spill-reload regs that never overlap ABI windows.
//! - **Typed-slot invariant**: for each live vreg, `vreg_to_reg[v]` must point at a preg whose
//!   `reg_types[preg]` exactly matches the vreg’s type.
//!
//! Enforcement of these rules lives primarily in `phase2.rs` (layout + invariants) and
//! `phase3.rs` (final mapping + spill insertion).

use std::collections::{HashMap, HashSet};

use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrFunction, IrModule, IrOp, IrTerminator, VRegId};
use crate::jlyb::{self, Function, Module, Op};
use crate::regalloc::InstrInfo;
use crate::regalloc::spill::VInsn;
use crate::typectx::T_DYNAMIC;

mod phase1;
mod phase2;
mod phase3;

pub(super) fn reg(v: VRegId) -> u32 {
    v.0
}

pub(super) struct VirtualStream {
    // Virtual instruction stream; operands are global vreg indices (u32).
    pub(super) vinsns: Vec<VInsn>,
    // Per-instruction uses/defs for regalloc/spill insertion; global vreg indices.
    pub(super) infos: Vec<InstrInfo>,
    // (sig_id, arg_base_vreg, nargs): used to pin arg windows to ABI arg blocks.
    pub(super) call_windows: Vec<(u32, u32, u8)>,
}

pub(super) struct Allocation {
    pub(super) vreg_to_reg: Vec<u8>,
    pub(super) vreg_to_spill: Vec<Option<u32>>,
    pub(super) reg_types: Vec<u32>,
    pub(super) cap_start: u32,
    pub(super) spill_reload_regs: [u8; 3],
    pub(super) last_def_pc: Vec<u32>,
    pub(super) f64_escape_per_param: Vec<u8>,
}

pub(super) fn term_size(term: &IrTerminator) -> u32 {
    match term {
        IrTerminator::JmpIf { .. } => 2,
        IrTerminator::SwitchKind { cases, .. } => 1 + (cases.len() as u32),
        IrTerminator::Jmp { .. } | IrTerminator::Ret { .. } => 1,
        IrTerminator::Unreachable => 1,
    }
}

pub(super) fn delta(from_pc: u32, to_pc: u32) -> u32 {
    // VM uses signed deltas, encoded as u32.
    let d: i32 = (to_pc as i32) - ((from_pc + 1) as i32);
    d as u32
}

pub(super) fn blk_pc(block_start: &[u32], b: BlockId) -> Result<u32, CompileError> {
    block_start
        .get(b.0 as usize)
        .copied()
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "bad block id"))
}

pub(super) fn block_successors(term: &IrTerminator) -> Vec<BlockId> {
    match term {
        IrTerminator::Jmp { target } => vec![*target],
        IrTerminator::JmpIf { then_tgt, else_tgt, .. } => {
            // Note: DFS→postorder→reverse means successor visitation order is reversed in
            // the final RPO. Visit else first so "then" (eg loop bodies) tend to appear
            // earlier in the linear stream than exits.
            vec![*else_tgt, *then_tgt]
        }
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut out: Vec<BlockId> = Vec::with_capacity(cases.len() + 1);
            for &(_, b) in cases {
                out.push(b);
            }
            out.push(*default);
            out
        }
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => vec![],
    }
}

pub(super) fn block_order_rpo(f: &IrFunction) -> Vec<BlockId> {
    let n = f.blocks.len();
    if n == 0 {
        return vec![];
    }
    let mut seen: Vec<bool> = vec![false; n];
    let mut post: Vec<BlockId> = Vec::with_capacity(n);

    fn dfs(f: &IrFunction, b: BlockId, seen: &mut [bool], post: &mut Vec<BlockId>) {
        let bi = b.0 as usize;
        if bi >= seen.len() || seen[bi] {
            return;
        }
        seen[bi] = true;
        let term = &f.blocks[bi].term;
        for s in block_successors(term) {
            dfs(f, s, seen, post);
        }
        post.push(b);
    }

    // Entry is always block 0.
    dfs(f, BlockId(0), &mut seen, &mut post);
    post.reverse(); // reverse postorder

    // Append unreachable blocks deterministically.
    for (bi, _) in f.blocks.iter().enumerate() {
        if !seen[bi] {
            post.push(BlockId(bi as u32));
        }
    }
    post
}

pub(super) fn pin_call_arg_blocks_and_validate(
    ir: &IrModule,
    call_windows: &[(u32, u32, u8)], // (sig_id, arg_base_vreg, nargs)
    call_callee_vregs: &HashSet<u32>,
    vinsns: &[VInsn],
    vreg_to_reg: &mut [u8],
    vreg_to_spill: &[Option<u32>],
    reg_types: &mut Vec<u32>,
) -> Result<HashMap<u32, u8>, CompileError> {
    let mut arg_block_start: HashMap<u32, u8> = HashMap::new();
    if call_windows.is_empty() {
        return Ok(arg_block_start);
    }

    // Arg blocks must not start at reg 0 when params occupy 0..param_count-1.
    // Otherwise F64 arg vregs could be pinned to reg 0 (Object param).
    if reg_types.is_empty() {
        reg_types.push(T_DYNAMIC);
    }

    // Arg block must not overlap CallR callee regs (pinning args would overwrite the closure).
    let max_callee_reg: u8 = call_callee_vregs
        .iter()
        .filter_map(|&gv| ((gv as usize) < vreg_to_reg.len()).then(|| vreg_to_reg[gv as usize]))
        .max()
        .unwrap_or(0);
    while (reg_types.len() as u8) <= max_callee_reg {
        reg_types.push(T_DYNAMIC);
    }

    let mut need_sigs: Vec<u32> = call_windows.iter().map(|(sig_id, _, _)| *sig_id).collect();
    need_sigs.sort();
    need_sigs.dedup();
    for sig_id in need_sigs {
        let sig = ir
            .sigs
            .get(sig_id as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "bad fun sig id"))?;
        if reg_types.len() + sig.args.len() > 256 {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "register allocation exceeded 256 regs",
            ));
        }
        let start = reg_types.len() as u8;
        for &tid in &sig.args {
            reg_types.push(tid);
        }
        arg_block_start.insert(sig_id, start);
    }

    for &(sig_id, arg_base_v, nargs) in call_windows {
        if nargs == 0 {
            continue;
        }
        let start = *arg_block_start
            .get(&sig_id)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "missing arg block"))?;
        for i in 0..(nargs as u32) {
            let v = arg_base_v + i;
            if (v as usize) >= vreg_to_reg.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call arg vreg out of range",
                ));
            }
            let dst = start
                .checked_add(i as u8)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "arg block overflow"))?;
            vreg_to_reg[v as usize] = dst;
        }
    }

    // Validate pinned arg windows after pinning:
    // - each arg vreg maps to the expected physical reg in its signature's arg block
    // - each such reg has the signature's static type in `reg_types`
    // - pinned arg vregs are never spilled
    for &(sig_id, arg_base_v, nargs) in call_windows {
        if nargs == 0 {
            continue;
        }
        let sig = ir
            .sigs
            .get(sig_id as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "bad fun sig id"))?;
        if sig.args.len() != (nargs as usize) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "call window arg count does not match signature",
            ));
        }
        let start = *arg_block_start
            .get(&sig_id)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "missing arg block"))?;
        for i in 0..(nargs as u32) {
            let v = (arg_base_v + i) as usize;
            let expected_r = start
                .checked_add(i as u8)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "arg block overflow"))?
                as usize;
            let r = *vreg_to_reg.get(v).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call arg vreg out of range",
                )
            })? as usize;
            if r != expected_r {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: arg vreg {} mapped to reg {} but expected {} (sig_id={})",
                        v, r, expected_r, sig_id
                    ),
                ));
            }
            if vreg_to_spill.get(v).is_some_and(|s| s.is_some()) {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("call ABI invariant violated: pinned arg vreg {} was spilled", v),
                ));
            }
            let rt = *reg_types.get(r).ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call ABI invariant violated: arg reg missing reg_types entry",
                )
            })?;
            let expected_tid = sig.args[i as usize];
            if rt != expected_tid {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: arg reg {} has tid {} but signature expects {} (sig_id={})",
                        r, rt, expected_tid, sig_id
                    ),
                ));
            }
        }
    }

    // CALLR-specific ABI invariant: the callee register must not overlap the argument window.
    for vi in vinsns.iter().filter(|vi| vi.op == Op::CallR as u8) {
        let callee_v = vi.b as usize;
        let arg_base_v = vi.imm as usize;
        let nargs = vi.c as usize;
        if callee_v >= vreg_to_reg.len() {
            continue;
        }
        let callee_r = vreg_to_reg[callee_v];
        for i in 0..nargs {
            let av = arg_base_v.saturating_add(i);
            if av >= vreg_to_reg.len() {
                break;
            }
            let ar = vreg_to_reg[av];
            if ar == callee_r {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "call ABI invariant violated: CALLR callee reg {} overlaps arg window reg (callee_vreg={} arg_vreg={})",
                        callee_r, callee_v, av
                    ),
                ));
            }
        }
    }

    Ok(arg_block_start)
}

/// Collect prelude function indices (1..=4) used in the IR via ConstFun, Call, or Closure.
fn collect_used_prelude_indices(ir: &IrModule) -> HashSet<u32> {
    let mut used = HashSet::new();
    for f in &ir.funcs {
        let nvregs = f.vreg_types.len();
        let mut const_fun_of_vreg: Vec<Option<u32>> = vec![None; nvregs];
        let order = block_order_rpo_static(f);
        for b in &order {
            let bi = b.0 as usize;
            let blk = &f.blocks[bi];
            for ins in &blk.insns {
                match &ins.op {
                    IrOp::ConstFun { dst, func_index } => {
                        if (1..=4).contains(func_index) {
                            used.insert(*func_index);
                        }
                        let di = dst.0 as usize;
                        if di < nvregs {
                            const_fun_of_vreg[di] = Some(*func_index);
                        }
                    }
                    IrOp::Mov { dst, src } => {
                        let di = dst.0 as usize;
                        let si = src.0 as usize;
                        if di < nvregs && si < nvregs {
                            const_fun_of_vreg[di] = const_fun_of_vreg[si];
                        }
                    }
                    IrOp::Call { callee, .. } => {
                        let ci = callee.0 as usize;
                        if ci < nvregs {
                            if let Some(fi) = const_fun_of_vreg[ci] {
                                if (1..=4).contains(&fi) {
                                    used.insert(fi);
                                }
                            }
                        }
                    }
                    IrOp::Closure { func_index, .. } => {
                        if (1..=4).contains(func_index) {
                            used.insert(*func_index);
                        }
                    }
                    _ => {}
                }
                // Clear def before updating (so Mov/ConstFun set fresh)
                if let Some(d) = ins.op.def() {
                    let di = d.0 as usize;
                    if di < nvregs {
                        const_fun_of_vreg[di] = None;
                    }
                }
                match &ins.op {
                    IrOp::ConstFun { dst, func_index } => {
                        let di = dst.0 as usize;
                        if di < nvregs {
                            const_fun_of_vreg[di] = Some(*func_index);
                        }
                    }
                    IrOp::Mov { dst, src } => {
                        let di = dst.0 as usize;
                        let si = src.0 as usize;
                        if di < nvregs && si < nvregs {
                            const_fun_of_vreg[di] = const_fun_of_vreg[si];
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    used
}

fn block_order_rpo_static(f: &IrFunction) -> Vec<BlockId> {
    fn block_successors_static(term: &IrTerminator) -> Vec<BlockId> {
        match term {
            IrTerminator::Jmp { target } => vec![*target],
            IrTerminator::JmpIf { then_tgt, else_tgt, .. } => vec![*else_tgt, *then_tgt],
            IrTerminator::SwitchKind { cases, default, .. } => {
                let mut out: Vec<BlockId> = cases.iter().map(|(_, b)| *b).collect();
                out.push(*default);
                out
            }
            IrTerminator::Ret { .. } | IrTerminator::Unreachable => vec![],
        }
    }
    let n = f.blocks.len();
    if n == 0 {
        return vec![];
    }
    let mut seen: Vec<bool> = vec![false; n];
    let mut post: Vec<BlockId> = Vec::with_capacity(n);
    fn dfs(f: &IrFunction, b: BlockId, seen: &mut [bool], post: &mut Vec<BlockId>) {
        let bi = b.0 as usize;
        if bi >= seen.len() || seen[bi] {
            return;
        }
        seen[bi] = true;
        let term = &f.blocks[bi].term;
        for s in block_successors_static(term) {
            dfs(f, s, seen, post);
        }
        post.push(b);
    }
    dfs(f, BlockId(0), &mut seen, &mut post);
    post.reverse();
    for (bi, _) in f.blocks.iter().enumerate() {
        if !seen[bi] {
            post.push(BlockId(bi as u32));
        }
    }
    post
}

pub fn emit_ir_module(ir: &IrModule) -> Result<Module, CompileError> {
    if ir.funcs.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            crate::ast::Span::point(0),
            "IR module has no functions",
        ));
    }
    if ir.entry >= ir.funcs.len() {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            crate::ast::Span::point(0),
            "IR module entry function index out of range",
        ));
    }

    let used_prelude: Vec<u32> = {
        let mut v: Vec<u32> = collect_used_prelude_indices(ir).into_iter().collect();
        v.sort_unstable();
        v
    };
    let prelude_funcs = jlyb::prelude_funcs_for_used(&used_prelude);
    let prelude_count = prelude_funcs.len() as u32;

    // Map IR "logical function indices" (0=native, 1..4=full prelude, 5=main, 6+=nested)
    // to the bytecode CALL/CALLR function index space (0=native, 1..=funcs.len()).
    //
    // The previous arithmetic mapping assumed nested function indices were assigned in the same
    // order they appear in `ir.funcs`. That assumption breaks when we lower nested lambdas in a
    // different traversal order (eg. outer lambda assigned before inner lambda is emitted).
    let mut user_fun_to_bc_fi: std::collections::HashMap<u32, u32> = std::collections::HashMap::new();
    for (i, f) in ir.funcs.iter().enumerate() {
        // Bytecode function array index (0-based) in the final module:
        // prelude funcs first, then `ir.funcs` in order.
        let bc_array_idx: u32 = prelude_count + (i as u32);
        // Bytecode CALL/CALLR function index space is 1-based (0 reserved for native).
        let bc_fi: u32 = 1 + bc_array_idx;

        let logical_idx: u32 = if i == 0 {
            5 // main/program entry
        } else {
            // Nested functions are named `lambda{logical_idx}` by lowering.
            let Some(name) = &f.name else {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "missing nested function name for mapping",
                ));
            };
            let Some(s) = name.strip_prefix("lambda") else {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("unexpected function name '{name}' for mapping"),
                ));
            };
            s.parse::<u32>().map_err(|_| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("bad lambda name '{name}' for mapping"),
                )
            })?
        };
        user_fun_to_bc_fi.insert(logical_idx, bc_fi);
    }

    let map_func_index = |old: u32| -> u32 {
        if old == 0 {
            return 0;
        }
        if (1..=4).contains(&old) {
            // Old prelude index (1..=4) -> used-prelude index (1..=prelude_count).
            return used_prelude
                .iter()
                .position(|&u| u == old)
                .map(|p| (p + 1) as u32)
                .unwrap_or(old);
        }
        user_fun_to_bc_fi.get(&old).copied().unwrap_or(old)
    };

    fn emit_function(
        ir: &IrModule,
        f: &crate::ir::IrFunction,
        map_func_index: &impl Fn(u32) -> u32,
    ) -> Result<Function, CompileError> {
        // Phase 1: IR -> virtual instruction stream (vregs) + regalloc info.
        let vs = phase1::build_virtual_stream(ir, f, map_func_index)?;
        // Phase 2: regalloc + ABI layout.
        let alloc = phase2::allocate_registers(ir, f, &vs)?;
        // Phase 3: operand mapping + spill insertion + peephole.
        let insns = phase3::emit_final_insns(&vs, &alloc)?;
        Ok(Function {
            reg_types: alloc.reg_types,
            cap_start: alloc.cap_start,
            insns,
        })
    }

    let const_f64 = ir.const_f64.clone();

    let mut funcs: Vec<Function> = Vec::with_capacity((prelude_count as usize) + ir.funcs.len());
    funcs.extend(prelude_funcs);
    for f in &ir.funcs {
        funcs.push(emit_function(ir, f, &map_func_index)?);
    }

    Ok(Module {
        types: ir.types.clone(),
        sigs: ir.sigs.clone(),
        atoms: ir.atoms.clone(),
        const_i64: ir.const_i64.clone(),
        const_f64,
        const_bytes: ir.const_bytes.clone(),
        funcs,
        // Module entry is an index into the `funcs` array (0-based, including any prelude funcs).
        entry: prelude_count + (ir.entry as u32),
        prelude_count,
        used_prelude: used_prelude.clone(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExprKind, Program, Span, Spanned};
    use crate::lower::lower_program_to_ir;
    use crate::parse;
    use crate::phi;

    #[test]
    fn emit_minimal_bytes_ir() {
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(0, 2)),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }
        // entry is logical index; funcs array index = entry - 1 (0 is native)
        let entry_func = (m.entry as usize).saturating_sub(1);
        assert_eq!(m.const_bytes.len(), 1);
        assert!(!m.funcs[entry_func].insns.is_empty());
        // We may emit additional prologue ops (eg. global object materialization),
        // but the function must still load the bytes constant and return it.
        assert!(
            m.funcs[entry_func]
                .insns
                .iter()
                .any(|i| i.op == Op::ConstBytes as u8),
            "expected ConstBytes in emitted bytecode"
        );
        assert_eq!(m.funcs[entry_func].insns.last().unwrap().op, Op::Ret as u8);
    }

    #[test]
    fn emit_match_basic_ir() {
        let src = "let x = 2; match (x) { 1 => { \"bad\" }, 2 => { \"ok\" }, _ => { \"bad\" }, }";
        let prog = parse::parse_program(src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap_or_else(|e| {
            panic!("emit failed: {}\nIR:\n{:#?}", e.render(src, None), ir.funcs[0]);
        });
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }
    }

    #[test]
    fn emit_if_expression_ir_has_jumps() {
        let if_sp = Span::new(0, 1);
        let cond_sp = Span::new(2, 3);
        let then_sp = Span::new(4, 5);
        let else_sp = Span::new(6, 7);
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(
                ExprKind::If {
                    cond: Box::new(Spanned::new(ExprKind::BoolLit(true), cond_sp)),
                    then_br: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), then_sp)),
                    else_br: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), else_sp)),
                },
                if_sp,
            ),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }
        let entry_func = (m.entry as usize).saturating_sub(1);
        assert!(m.funcs[entry_func].insns.iter().any(|i| i.op == Op::JmpIf as u8));
        assert!(m.funcs[entry_func].insns.iter().any(|i| i.op == Op::Jmp as u8));
    }

    #[test]
    fn emits_spills_under_high_dynamic_pressure() {
        // Force spills in a nested function by creating 255 live Dynamic vregs.
        // (Entry module must return Bytes and also reserves extra vregs; a nested
        // function avoids those constraints.)
        //
        // Keep total vregs in the nested function <= 256:
        // - 1 self-binding vreg (inserted by lowering for typed `let f = fn...` to allow recursion)
        // - 255 `Dynamic` vregs (`a0..a254`)
        let n: usize = 255;
        let mut src = String::new();
        src.push_str("let f: () -> Any = fn() {\n");
        for i in 0..n {
            src.push_str(&format!("  let a{i} = null;\n"));
        }
        for i in 0..n {
            src.push_str(&format!("  throw a{i};\n"));
        }
        src.push_str("  a0\n");
        src.push_str("};\n");
        src.push_str("let _x = f();\n");
        src.push_str("\"ok\"");

        let prog = parse::parse_program(&src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap_or_else(|e| {
            let lens: Vec<usize> = ir.funcs.iter().map(|f| f.vreg_types.len()).collect();
            panic!("emit failed: {:?}; vreg lens={lens:?}", e)
        });
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }
        let any_spill_push = m.funcs.iter().any(|f| f.insns.iter().any(|i| i.op == Op::SpillPush as u8));
        let any_spill_pop = m.funcs.iter().any(|f| f.insns.iter().any(|i| i.op == Op::SpillPop as u8));
        assert!(any_spill_push, "expected SpillPush in some emitted function");
        assert!(any_spill_pop, "expected SpillPop in some emitted function");

        // Spill stack is boxed-only: SpillPush/SpillPop must operate on Dynamic regs only.
        for f in &m.funcs {
            for ins in &f.insns {
                if ins.op != Op::SpillPush as u8 && ins.op != Op::SpillPop as u8 {
                    continue;
                }
                let r = ins.a as usize;
                let rt = f.reg_types.get(r).copied().unwrap_or(0);
                assert_eq!(
                    rt,
                    crate::typectx::T_DYNAMIC,
                    "spill op used non-Dynamic reg type (reg {r} tid={rt})"
                );
            }
        }
    }

    #[test]
    fn typed_register_pressure_errors() {
        // Typed vregs are non-spillable. Force allocation failure by creating >256
        // simultaneously-live I32 vregs inside a nested function.
        let n: usize = 300;
        let mut src = String::new();
        src.push_str("let g: () -> I32 = fn() {\n");
        for i in 0..n {
            src.push_str(&format!("  let a{i}: I32 = {i};\n"));
        }
        // Use all locals in a linear chain to keep them live without creating a
        // deeply nested parse tree (which can overflow the Rust test stack).
        src.push_str("  let s0: I32 = 0;\n");
        for i in 0..n {
            src.push_str(&format!("  let s{}: I32 = s{} + a{};\n", i + 1, i, i));
        }
        src.push_str(&format!("  s{}\n", n));
        src.push_str("};\n");
        src.push_str("let _x = g();\n");
        src.push_str("\"ok\"");

        let prog = parse::parse_program(&src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let e = match emit_ir_module(&ir) {
            Ok(_) => panic!("expected codegen error due to typed register pressure"),
            Err(e) => e,
        };
        assert_eq!(e.kind, crate::error::ErrorKind::Codegen, "expected Codegen error, got {:?}", e.kind);
        assert!(
            e.message.contains("register pressure too high"),
            "unexpected error message: {}",
            e.message
        );
    }

    #[test]
    fn try_catch_closure_and_call_windows_validate() {
        // Stress a few invariants together:
        // - try/catch lowering + exception-aware SSA/pinning
        // - closure creation (capture window pinning)
        // - a larger-arity call (arg window pinning)
        //
        // We mostly care that the emitted bytecode validates and includes the expected ops.
        let mut src = String::new();
        src.push_str("let h: (Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any) -> Any = fn(a,b,c,d,e,f,g,h,i,j,k,l) { a };\n");
        src.push_str("let f = fn() {\n");
        for i in 0..32 {
            src.push_str(&format!("  let d{i} = null;\n"));
        }
        // Closure with captures (forces CLOSURE + capture-window handling).
        src.push_str("  let g = fn() { d0; d1; d2; d3; d4; d5; d6; d7; d8; d9; d10; d11; d12; d13; d14; d15; d0 };\n");
        // Try/catch around a call + closure call.
        src.push_str("  let r = try {\n");
        src.push_str("    let x = h(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11);\n");
        src.push_str("    g();\n");
        src.push_str("    x\n");
        src.push_str("  } catch (e) { e };\n");
        src.push_str("  r\n");
        src.push_str("};\n");
        src.push_str("let _x = f();\n");
        src.push_str("\"ok\"");

        let prog = parse::parse_program(&src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }

        let mut saw_try = false;
        let mut saw_endtry = false;
        let mut saw_closure = false;
        let mut saw_call = false;
        for f in &m.funcs {
            for ins in &f.insns {
                if ins.op == Op::Try as u8 {
                    saw_try = true;
                } else if ins.op == Op::EndTry as u8 {
                    saw_endtry = true;
                } else if ins.op == Op::Closure as u8 {
                    saw_closure = true;
                } else if ins.op == Op::Call as u8 || ins.op == Op::CallR as u8 {
                    saw_call = true;
                }
            }
        }
        assert!(saw_try, "expected TRY in emitted bytecode");
        assert!(saw_endtry, "expected ENDTRY in emitted bytecode");
        assert!(saw_closure, "expected CLOSURE in emitted bytecode");
        assert!(saw_call, "expected a CALL/CALLR in emitted bytecode");
    }

    #[test]
    fn spills_never_touch_call_or_capture_windows() {
        // This test enforces an important ABI invariant:
        // vregs pinned to call arg windows and closure capture windows must never spill.
        //
        // We check it at the bytecode level: any SpillPush/SpillPop reg must not be a reg that
        // participates in any Call/CallR arg window or Closure capture window in that function.
        let mut src = String::new();
        src.push_str("let h: (Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any) -> Any = fn(a,b,c,d,e,f,g,h,i,j,k,l) { a };\n");
        src.push_str("let f: () -> Any = fn() {\n");
        // Many Dynamic locals to force spills.
        let n: usize = 300;
        for i in 0..n {
            src.push_str(&format!("  let a{i} = null;\n"));
        }
        // Closure with captures (capture window).
        src.push_str("  let g = fn() { a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a0 };\n");
        // A big-arity call (call arg window).
        src.push_str("  let r = h(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);\n");
        // Use the closure so it's not trivially dead.
        src.push_str("  let _x = g();\n");
        // Add many throws to keep values live long enough to pressure regalloc.
        for i in 0..n {
            src.push_str(&format!("  throw a{i};\n"));
        }
        src.push_str("  r\n");
        src.push_str("};\n");
        // Call f so it is emitted.
        src.push_str("let _y = f();\n");
        src.push_str("\"ok\"");

        let prog = parse::parse_program(&src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }

        let mut saw_spill = false;
        for func in &m.funcs {
            // Collect "window regs" for this function from its insn stream.
            let mut window_regs: [bool; 256] = [false; 256];
            for ins in &func.insns {
                if ins.op == Op::Call as u8 {
                    let base = ins.b as usize;
                    let nargs = ins.c as usize;
                    for r in base..base.saturating_add(nargs) {
                        if r < 256 {
                            window_regs[r] = true;
                        }
                    }
                } else if ins.op == Op::CallR as u8 {
                    let base = ins.imm as usize;
                    let nargs = ins.c as usize;
                    for r in base..base.saturating_add(nargs) {
                        if r < 256 {
                            window_regs[r] = true;
                        }
                    }
                } else if ins.op == Op::Closure as u8 {
                    let base = ins.b as usize;
                    let ncaps = ins.c as usize;
                    for r in base..base.saturating_add(ncaps) {
                        if r < 256 {
                            window_regs[r] = true;
                        }
                    }
                }
            }

            for ins in &func.insns {
                if ins.op != Op::SpillPush as u8 && ins.op != Op::SpillPop as u8 {
                    continue;
                }
                saw_spill = true;
                let r = ins.a as usize;
                assert!(
                    r < 256 && !window_regs[r],
                    "spill op used a call/capture window reg (r={r})"
                );
            }
        }
        assert!(saw_spill, "expected at least one spill op in emitted module");
    }

    #[test]
    fn callr_callee_reg_is_never_spilled_under_pressure() {
        // `CALLR` callee must never be spilled/reloaded (it can corrupt the closure object).
        // This is enforced in codegen invariants; here we assert it at the bytecode level under
        // high Dynamic pressure (so spills definitely occur).
        let mut src = String::new();
        src.push_str("let f: () -> Any = fn() {\n");
        let n: usize = 300;
        for i in 0..n {
            src.push_str(&format!("  let a{i} = null;\n"));
        }
        // Closure value lives in a local and is called indirectly (CALLR).
        src.push_str("  let g = fn() { a0 };\n");
        src.push_str("  let r = g();\n");
        // Keep lots of locals live to force spills around/after the call site.
        for i in 0..n {
            src.push_str(&format!("  throw a{i};\n"));
        }
        src.push_str("  r\n");
        src.push_str("};\n");
        src.push_str("let _x = f();\n");
        src.push_str("\"ok\"");

        let prog = parse::parse_program(&src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }

        let mut saw_callr = false;
        let mut saw_spill = false;
        for func in &m.funcs {
            let mut callee_regs: [bool; 256] = [false; 256];
            for ins in &func.insns {
                if ins.op == Op::CallR as u8 {
                    saw_callr = true;
                    let r = ins.b as usize;
                    if r < 256 {
                        callee_regs[r] = true;
                    }
                }
            }
            for ins in &func.insns {
                if ins.op != Op::SpillPush as u8 && ins.op != Op::SpillPop as u8 {
                    continue;
                }
                saw_spill = true;
                let r = ins.a as usize;
                assert!(
                    r < 256 && !callee_regs[r],
                    "spill op used a CALLR callee reg (r={r})"
                );
            }
        }
        assert!(saw_callr, "expected CALLR in emitted module");
        assert!(saw_spill, "expected at least one spill op in emitted module");
    }

    #[test]
    fn strict_validation_checks_direct_call_arg_types() {
        // Ensure the stricter validator sees at least one direct CALL and that caller arg types
        // match the callee's param register types.
        let src = r#"
let two: I32 = 2;
let f: (I32) -> I32 = fn(x: I32) { let one: I32 = 1; x + one };
let _y: I32 = f(two);
"ok"
"#;
        let prog = parse::parse_program(src).unwrap();
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();

        if let Err(msg) = crate::jlyb::validate_module(&m) {
            panic!("bytecode validation failed: {msg}");
        }
        if let Err(msg) = crate::jlyb::validate_module_strict(&m) {
            panic!("strict bytecode validation failed: {msg}");
        }

        let saw_call = m.funcs.iter().any(|f| f.insns.iter().any(|i| i.op == Op::Call as u8));
        assert!(saw_call, "expected at least one direct CALL (peephole should fold CONST_FUN+CALLR)");
    }
}

