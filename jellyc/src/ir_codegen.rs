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

use std::collections::HashSet;

use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrFunction, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::jlyb::{self, Function, Insn, Module, Op, TypeKind};
use crate::regalloc::{self, spill, InstrInfo, SpillPolicy, VReg};
use crate::regalloc::spill::VInsn;
use crate::typectx::{T_DYNAMIC, T_F64};

fn reg(v: VRegId) -> u32 {
    v.0
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
        return Err(CompileError::new(ErrorKind::Codegen, crate::ast::Span::point(0), "IR module has no functions"));
    }
    if ir.entry >= ir.funcs.len() {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            crate::ast::Span::point(0),
            "IR module entry function index out of range",
        ));
    }

    fn term_size(term: &IrTerminator) -> u32 {
        match term {
            IrTerminator::JmpIf { .. } => 2,
            IrTerminator::SwitchKind { cases, .. } => 1 + (cases.len() as u32),
            IrTerminator::Jmp { .. } | IrTerminator::Ret { .. } => 1,
            IrTerminator::Unreachable => 1,
        }
    }

    fn delta(from_pc: u32, to_pc: u32) -> u32 {
        // VM uses signed deltas, encoded as u32.
        let d: i32 = (to_pc as i32) - ((from_pc + 1) as i32);
        d as u32
    }

    fn blk_pc(block_start: &[u32], b: BlockId) -> Result<u32, CompileError> {
        block_start
            .get(b.0 as usize)
            .copied()
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "bad block id"))
    }

    fn block_successors(term: &IrTerminator) -> Vec<BlockId> {
        match term {
            IrTerminator::Jmp { target } => vec![*target],
            IrTerminator::JmpIf {
                then_tgt, else_tgt, ..
            } => {
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

    fn block_order_rpo(f: &IrFunction) -> Vec<BlockId> {
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

    let used_prelude: Vec<u32> = {
        let mut v: Vec<u32> = collect_used_prelude_indices(ir).into_iter().collect();
        v.sort_unstable();
        v
    };
    let prelude_funcs = jlyb::prelude_funcs_for_used(&used_prelude);
    let prelude_count = prelude_funcs.len() as u32;
    let map_func_index = |old: u32| -> u32 {
        if old == 0 {
            return 0;
        }
        if (1..=4).contains(&old) {
            return used_prelude
                .iter()
                .position(|&u| u == old)
                .map(|p| (p + 1) as u32)
                .unwrap_or(old);
        }
        prelude_count + 1 + (old - 5)
    };

    fn emit_function(
        ir: &IrModule,
        f: &crate::ir::IrFunction,
        map_func_index: &impl Fn(u32) -> u32,
    ) -> Result<Function, CompileError> {
        // Allow >256 vregs; regalloc can spill typed values (VM SpillPush/Pop now support any type).

        // Precompute block start PCs in vinsn space.
        //
        // NOTE: allocation is based on a linearized instruction stream. Emitting
        // blocks in reverse-postorder (RPO) avoids placing loop exits/returns "in
        // the middle" of the linear stream, which can otherwise break liveness
        // approximations and yield incorrect allocations.
        let order: Vec<BlockId> = block_order_rpo(f);
        let mut block_start: Vec<u32> = vec![0; f.blocks.len()];
        let mut pc: u32 = 0;
        for b in order.iter().copied() {
            let bi = b.0 as usize;
            let blk = &f.blocks[bi];
            block_start[bi] = pc;
            pc += blk.insns.len() as u32;
            pc += term_size(&blk.term);
        }

        let mut vinsns: Vec<VInsn> = Vec::new(); // operands are vreg indices (u32)
        let mut infos: Vec<InstrInfo> = Vec::new();
        // Track call arg windows so we can allocate/pin them contiguously.
        let mut call_windows: Vec<(u32, u32, u8)> = Vec::new(); // (sig_id, arg_base_vreg, nargs)
        // Track which vregs are known `ConstFun(func_index)` so we can emit direct CALL.
        let mut const_fun_of_vreg: Vec<Option<u32>> = vec![None; f.vreg_types.len()];

        for b in order.iter().copied() {
            let bi = b.0 as usize;
            let blk = &f.blocks[bi];
            for (ii, ins) in blk.insns.iter().enumerate() {
                let this_pc = block_start[bi] + (ii as u32);
                let mut is_direct_call: bool = false;
                match &ins.op {
                    IrOp::ConstBytes { dst, pool_index } => vinsns.push(VInsn {
                        op: Op::ConstBytes as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *pool_index,
                    }),
                    IrOp::ConstI64 { dst, pool_index } => vinsns.push(VInsn {
                        op: Op::ConstI64 as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *pool_index,
                    }),
                    IrOp::ConstBool { dst, imm } => vinsns.push(VInsn {
                        op: Op::ConstBool as u8,
                        a: reg(*dst),
                        b: 0,
                        c: if *imm { 1 } else { 0 },
                        imm: 0,
                    }),
                    IrOp::ConstI32 { dst, imm } => vinsns.push(VInsn {
                        op: Op::ConstI32 as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *imm as u32,
                    }),
                    IrOp::ConstF32 { dst, bits } => vinsns.push(VInsn {
                        op: Op::ConstF32 as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *bits,
                    }),
                    IrOp::ConstI8Imm { dst, imm } => vinsns.push(VInsn {
                        op: Op::ConstI8Imm as u8,
                        a: reg(*dst),
                        b: 0,
                        c: *imm as u32,
                        imm: 0,
                    }),
                    IrOp::ConstF16 { dst, bits } => vinsns.push(VInsn {
                        op: Op::ConstF16 as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *bits as u32,
                    }),
                    IrOp::ConstF64 { dst, pool_index } => vinsns.push(VInsn {
                        op: Op::ConstF64 as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *pool_index,
                    }),
                    IrOp::ConstNull { dst } => vinsns.push(VInsn { op: Op::ConstNull as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                    IrOp::ConstAtom { dst, atom_id } => vinsns.push(VInsn { op: Op::ConstAtom as u8, a: reg(*dst), b: 0, c: 0, imm: *atom_id }),
                    IrOp::ConstFun { dst, func_index } => vinsns.push(VInsn {
                        op: Op::ConstFun as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: map_func_index(*func_index),
                    }),
                    IrOp::Mov { dst, src } => {
                        // VM rejects Mov when src/dst have different kinds or slot sizes.
                        // Emit FromDyn* for Dynamic->primitive; emit conversion ops for prim->prim.
                        let src_tid = f.vreg_types.get(src.0 as usize).copied();
                        let dst_tid = f.vreg_types.get(dst.0 as usize).copied();
                        let op = match (src_tid, dst_tid) {
                            (Some(st), Some(dt)) => {
                                let src_kind = ir.types.get(st as usize).map(|te| te.kind);
                                let dst_kind = ir.types.get(dt as usize).map(|te| te.kind);
                                match (src_kind, dst_kind) {
                                    (Some(TypeKind::Dynamic), Some(dk)) if dk != TypeKind::Dynamic => match dk {
                                        TypeKind::I8 => Op::FromDynI8,
                                        TypeKind::I16 => Op::FromDynI16,
                                        TypeKind::I32 => Op::FromDynI32,
                                        TypeKind::I64 => Op::FromDynI64,
                                        TypeKind::F16 => Op::FromDynF16,
                                        TypeKind::F32 => Op::FromDynF32,
                                        TypeKind::F64 => Op::FromDynF64,
                                        TypeKind::Bool => Op::FromDynBool,
                                        _ => Op::Mov,
                                    },
                                    (Some(sk), Some(dk)) if sk != dk => {
                                        // Primitive-to-primitive with different kinds: need conversion.
                                        // Only single-op conversions; multi-step (eg F16<->F64) should be expanded earlier.
                                        match (sk, dk) {
                                            (TypeKind::F32, TypeKind::F64) => Op::F64FromF32,
                                            (TypeKind::F64, TypeKind::F32) => Op::F32FromF64,
                                            (TypeKind::F16, TypeKind::F32) => Op::F32FromF16,
                                            (TypeKind::F32, TypeKind::F16) => Op::F16FromF32,
                                            (TypeKind::I32, TypeKind::F32) => Op::F32FromI32,
                                            (TypeKind::I32, TypeKind::F64) => Op::F64FromI32,
                                            (TypeKind::I64, TypeKind::F32) => Op::F32FromI64,
                                            (TypeKind::I64, TypeKind::F64) => Op::F64FromI64,
                                            (TypeKind::F32, TypeKind::I32) => Op::I32FromF32,
                                            (TypeKind::F32, TypeKind::I64) => Op::I64FromF32,
                                            (TypeKind::F64, TypeKind::I32) => Op::I32FromF64,
                                            (TypeKind::F64, TypeKind::I64) => Op::I64FromF64,
                                            (TypeKind::I64, TypeKind::I32) => Op::I32FromI64,
                                            (TypeKind::I32, TypeKind::I64) => Op::SextI64,
                                            _ => Op::Mov,
                                        }
                                    }
                                    _ => Op::Mov,
                                }
                            }
                            _ => Op::Mov,
                        };
                        vinsns.push(VInsn { op: op as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 });
                    }
                    IrOp::AddI32 { dst, a, b } => vinsns.push(VInsn { op: Op::AddI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::SubI32 { dst, a, b } => vinsns.push(VInsn { op: Op::SubI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::MulI32 { dst, a, b } => vinsns.push(VInsn { op: Op::MulI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NegI32 { dst, src } => vinsns.push(VInsn { op: Op::NegI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::EqI32 { dst, a, b } => vinsns.push(VInsn { op: Op::EqI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::LtI32 { dst, a, b } => vinsns.push(VInsn { op: Op::LtI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::AddI64 { dst, a, b } => vinsns.push(VInsn { op: Op::AddI64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::SubI64 { dst, a, b } => vinsns.push(VInsn { op: Op::SubI64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::MulI64 { dst, a, b } => vinsns.push(VInsn { op: Op::MulI64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NegI64 { dst, src } => vinsns.push(VInsn { op: Op::NegI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::EqI64 { dst, a, b } => vinsns.push(VInsn { op: Op::EqI64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::LtI64 { dst, a, b } => vinsns.push(VInsn { op: Op::LtI64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::AddF16 { dst, a, b } => vinsns.push(VInsn { op: Op::AddF16 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::SubF16 { dst, a, b } => vinsns.push(VInsn { op: Op::SubF16 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::MulF16 { dst, a, b } => vinsns.push(VInsn { op: Op::MulF16 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::AddF32 { dst, a, b } => vinsns.push(VInsn { op: Op::AddF32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::SubF32 { dst, a, b } => vinsns.push(VInsn { op: Op::SubF32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::MulF32 { dst, a, b } => vinsns.push(VInsn { op: Op::MulF32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::DivF32 { dst, a, b } => vinsns.push(VInsn { op: Op::DivF32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NegF32 { dst, src } => vinsns.push(VInsn { op: Op::NegF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::EqF32 { dst, a, b } => vinsns.push(VInsn { op: Op::EqF32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::LtF32 { dst, a, b } => vinsns.push(VInsn { op: Op::LtF32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::AddF64 { dst, a, b } => vinsns.push(VInsn { op: Op::AddF64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::SubF64 { dst, a, b } => vinsns.push(VInsn { op: Op::SubF64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::MulF64 { dst, a, b } => vinsns.push(VInsn { op: Op::MulF64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::DivF64 { dst, a, b } => vinsns.push(VInsn { op: Op::DivF64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NegF64 { dst, src } => vinsns.push(VInsn { op: Op::NegF64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::EqF64 { dst, a, b } => vinsns.push(VInsn { op: Op::EqF64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::LtF64 { dst, a, b } => vinsns.push(VInsn { op: Op::LtF64 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::Physeq { dst, a, b } => vinsns.push(VInsn { op: Op::Physeq as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NotBool { dst, src } => vinsns.push(VInsn { op: Op::NotBool as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::Kindof { dst, src } => vinsns.push(VInsn { op: Op::Kindof as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::Assert { cond } => vinsns.push(VInsn { op: Op::Assert as u8, a: reg(*cond), b: 0, c: 0, imm: 0 }),
                    IrOp::SextI64 { dst, src } => vinsns.push(VInsn { op: Op::SextI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::SextI16 { dst, src } => vinsns.push(VInsn { op: Op::SextI16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::TruncI8 { dst, src } => vinsns.push(VInsn { op: Op::TruncI8 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::TruncI16 { dst, src } => vinsns.push(VInsn { op: Op::TruncI16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F16FromF32 { dst, src } => vinsns.push(VInsn { op: Op::F16FromF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F32FromF16 { dst, src } => vinsns.push(VInsn { op: Op::F32FromF16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F32FromI32 { dst, src } => vinsns.push(VInsn { op: Op::F32FromI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F64FromI32 { dst, src } => vinsns.push(VInsn { op: Op::F64FromI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F64FromI64 { dst, src } => vinsns.push(VInsn { op: Op::F64FromI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F64FromF32 { dst, src } => vinsns.push(VInsn { op: Op::F64FromF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F32FromF64 { dst, src } => vinsns.push(VInsn { op: Op::F32FromF64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F32FromI64 { dst, src } => vinsns.push(VInsn { op: Op::F32FromI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::I64FromF32 { dst, src } => vinsns.push(VInsn { op: Op::I64FromF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::I32FromI64 { dst, src } => vinsns.push(VInsn { op: Op::I32FromI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::I32FromF64 { dst, src } => vinsns.push(VInsn { op: Op::I32FromF64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::I64FromF64 { dst, src } => vinsns.push(VInsn { op: Op::I64FromF64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::I32FromF32 { dst, src } => vinsns.push(VInsn { op: Op::I32FromF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::F16FromI32 { dst, src } => vinsns.push(VInsn { op: Op::F16FromI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::I32FromF16 { dst, src } => vinsns.push(VInsn { op: Op::I32FromF16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::BytesNew { dst, len } => vinsns.push(VInsn { op: Op::BytesNew as u8, a: reg(*dst), b: reg(*len), c: 0, imm: 0 }),
                    IrOp::BytesLen { dst, bytes } => vinsns.push(VInsn { op: Op::BytesLen as u8, a: reg(*dst), b: reg(*bytes), c: 0, imm: 0 }),
                    IrOp::BytesGetU8 { dst, bytes, index } => vinsns.push(VInsn {
                        op: Op::BytesGetU8 as u8,
                        a: reg(*dst),
                        b: reg(*bytes),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::BytesSetU8 { bytes, index, value } => vinsns.push(VInsn {
                        op: Op::BytesSetU8 as u8,
                        a: reg(*value),
                        b: reg(*bytes),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::BytesConcat2 { dst, a, b } => vinsns.push(VInsn {
                        op: Op::BytesConcat2 as u8,
                        a: reg(*dst),
                        b: reg(*a),
                        c: reg(*b),
                        imm: 0,
                    }),
                    IrOp::BytesConcatMany { dst, parts } => vinsns.push(VInsn {
                        op: Op::BytesConcatMany as u8,
                        a: reg(*dst),
                        b: reg(*parts),
                        c: 0,
                        imm: 0,
                    }),
                    IrOp::ListNil { dst } => vinsns.push(VInsn { op: Op::ListNil as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                    IrOp::ListCons { dst, head, tail } => vinsns.push(VInsn {
                        op: Op::ListCons as u8,
                        a: reg(*dst),
                        b: reg(*head),
                        c: reg(*tail),
                        imm: 0,
                    }),
                    IrOp::ListHead { dst, list } => {
                        vinsns.push(VInsn { op: Op::ListHead as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 })
                    }
                    IrOp::ListTail { dst, list } => {
                        vinsns.push(VInsn { op: Op::ListTail as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 })
                    }
                    IrOp::ListIsNil { dst, list } => {
                        vinsns.push(VInsn { op: Op::ListIsNil as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 })
                    }
                    IrOp::ArrayNew { dst, len } => vinsns.push(VInsn { op: Op::ArrayNew as u8, a: reg(*dst), b: reg(*len), c: 0, imm: 0 }),
                    IrOp::ArrayLen { dst, arr } => vinsns.push(VInsn { op: Op::ArrayLen as u8, a: reg(*dst), b: reg(*arr), c: 0, imm: 0 }),
                    IrOp::ArrayGet { dst, arr, index } => vinsns.push(VInsn {
                        op: Op::ArrayGet as u8,
                        a: reg(*dst),
                        b: reg(*arr),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::ArraySet { arr, index, value } => vinsns.push(VInsn {
                        op: Op::ArraySet as u8,
                        a: reg(*value),
                        b: reg(*arr),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::ObjNew { dst } => vinsns.push(VInsn { op: Op::ObjNew as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                    IrOp::ObjHasAtom { dst, obj, atom_id } => vinsns.push(VInsn {
                        op: Op::ObjHasAtom as u8,
                        a: reg(*dst),
                        b: reg(*obj),
                        c: 0,
                        imm: *atom_id,
                    }),
                    IrOp::ObjGetAtom { dst, obj, atom_id } => vinsns.push(VInsn {
                        op: Op::ObjGetAtom as u8,
                        a: reg(*dst),
                        b: reg(*obj),
                        c: 0,
                        imm: *atom_id,
                    }),
                    IrOp::ObjSetAtom { obj, atom_id, value } => vinsns.push(VInsn {
                        op: Op::ObjSetAtom as u8,
                        a: reg(*value),
                        b: reg(*obj),
                        c: 0,
                        imm: *atom_id,
                    }),
                    IrOp::ObjGet { dst, obj, atom } => vinsns.push(VInsn { op: Op::ObjGet as u8, a: reg(*dst), b: reg(*obj), c: reg(*atom), imm: 0 }),
                    IrOp::ObjSet { obj, atom, value } => vinsns.push(VInsn { op: Op::ObjSet as u8, a: reg(*value), b: reg(*obj), c: reg(*atom), imm: 0 }),
                    IrOp::ToDyn { dst, src } => vinsns.push(VInsn { op: Op::ToDyn as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::FromDynI8 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynI8 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynI16 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynI16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynI32 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynI64 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynF16 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynF16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynF32 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynF64 { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynF64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynBool { dst, src } => {
                        vinsns.push(VInsn { op: Op::FromDynBool as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynPtr { dst, src } => vinsns.push(VInsn { op: Op::FromDynPtr as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::Throw { payload } => vinsns.push(VInsn { op: Op::Throw as u8, a: reg(*payload), b: 0, c: 0, imm: 0 }),
                    IrOp::Try { catch_dst, catch_block, trap_only } => {
                        let catch_pc = blk_pc(&block_start, *catch_block)?;
                        vinsns.push(VInsn {
                            op: Op::Try as u8,
                            a: reg(*catch_dst),
                            b: if *trap_only { 1 } else { 0 },
                            c: 0,
                            imm: delta(this_pc, catch_pc),
                        });
                    }
                    IrOp::EndTry => vinsns.push(VInsn { op: Op::EndTry as u8, a: 0, b: 0, c: 0, imm: 0 }),
                    IrOp::Call {
                        dst,
                        callee,
                        sig_id,
                        arg_base,
                        nargs,
                    } => {
                        if let Some(fi) = const_fun_of_vreg.get(callee.0 as usize).copied().flatten() {
                            is_direct_call = true;
                            // For the virtual stream we keep vregs in a/b and patch later.
                            vinsns.push(VInsn {
                                op: Op::Call as u8,
                                a: reg(*dst),
                                b: reg(*arg_base),
                                c: *nargs as u32,
                                imm: fi,
                            });
                        } else {
                            // Encode arg_base vreg in imm for the virtual stream. We'll map it to
                            // a physical register index during final emission.
                            vinsns.push(VInsn {
                                op: Op::CallR as u8,
                                a: reg(*dst),
                                b: reg(*callee),
                                c: *nargs as u32,
                                imm: arg_base.0,
                            });
                        }
                        call_windows.push((*sig_id, arg_base.0, *nargs));
                    }
                    IrOp::Closure {
                        dst,
                        func_index,
                        cap_sig_id,
                        cap_base,
                        ncaps,
                    } => {
                        vinsns.push(VInsn {
                            op: Op::Closure as u8,
                            a: reg(*dst),
                            b: reg(*cap_base),
                            c: *ncaps as u32,
                            imm: map_func_index(*func_index),
                        });
                        call_windows.push((*cap_sig_id, cap_base.0, *ncaps));
                    }
                    IrOp::BindThis { dst, func, this } => {
                        vinsns.push(VInsn { op: Op::BindThis as u8, a: reg(*dst), b: reg(*func), c: reg(*this), imm: 0 });
                    }
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Codegen,
                            ins.span,
                            "IR→bytecode bridge: unsupported instruction op",
                        ))
                    }
                }

                // InstrInfo for regalloc
                let mut uses: Vec<VReg> = Vec::new();
                let mut defs: Vec<VReg> = Vec::new();
                match &ins.op {
                    IrOp::ConstI32 { dst, .. }
                    | IrOp::ConstI8Imm { dst, .. }
                    | IrOp::ConstF16 { dst, .. }
                    | IrOp::ConstI64 { dst, .. }
                    | IrOp::ConstF32 { dst, .. }
                    | IrOp::ConstF64 { dst, .. }
                    | IrOp::ConstBool { dst, .. }
                    | IrOp::ConstNull { dst }
                    | IrOp::ConstBytes { dst, .. }
                    | IrOp::ConstAtom { dst, .. }
                    | IrOp::ConstFun { dst, .. } => {
                        defs.push(VReg(dst.0));
                    }
                    IrOp::Mov { dst, src } => {
                        uses.push(VReg(src.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::AddI32 { dst, a, b }
                    | IrOp::SubI32 { dst, a, b }
                    | IrOp::MulI32 { dst, a, b }
                    | IrOp::EqI32 { dst, a, b }
                    | IrOp::LtI32 { dst, a, b }
                    | IrOp::AddI64 { dst, a, b }
                    | IrOp::SubI64 { dst, a, b }
                    | IrOp::MulI64 { dst, a, b }
                    | IrOp::EqI64 { dst, a, b }
                    | IrOp::LtI64 { dst, a, b }
                    | IrOp::AddF16 { dst, a, b }
                    | IrOp::SubF16 { dst, a, b }
                    | IrOp::MulF16 { dst, a, b }
                    | IrOp::AddF32 { dst, a, b }
                    | IrOp::SubF32 { dst, a, b }
                    | IrOp::MulF32 { dst, a, b }
                    | IrOp::DivF32 { dst, a, b }
                    | IrOp::EqF32 { dst, a, b }
                    | IrOp::LtF32 { dst, a, b }
                    | IrOp::AddF64 { dst, a, b }
                    | IrOp::SubF64 { dst, a, b }
                    | IrOp::MulF64 { dst, a, b }
                    | IrOp::DivF64 { dst, a, b }
                    | IrOp::EqF64 { dst, a, b }
                    | IrOp::LtF64 { dst, a, b }
                    | IrOp::Physeq { dst, a, b } => {
                        uses.push(VReg(a.0));
                        uses.push(VReg(b.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::NegI32 { dst, src }
                    | IrOp::NegI64 { dst, src }
                    | IrOp::NegF32 { dst, src }
                    | IrOp::NegF64 { dst, src }
                    | IrOp::NotBool { dst, src }
                    | IrOp::Kindof { dst, src }
                    | IrOp::ToDyn { dst, src }
                    | IrOp::FromDynI8 { dst, src }
                    | IrOp::FromDynI16 { dst, src }
                    | IrOp::SextI16 { dst, src }
                    | IrOp::TruncI8 { dst, src }
                    | IrOp::TruncI16 { dst, src }
                    | IrOp::F16FromF32 { dst, src }
                    | IrOp::F32FromF16 { dst, src }
                    | IrOp::FromDynI32 { dst, src }
                    | IrOp::FromDynI64 { dst, src }
                    | IrOp::FromDynF16 { dst, src }
                    | IrOp::FromDynF32 { dst, src }
                    | IrOp::FromDynF64 { dst, src }
                    | IrOp::FromDynBool { dst, src }
                    | IrOp::FromDynPtr { dst, src } => {
                        uses.push(VReg(src.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::Assert { cond } => {
                        uses.push(VReg(cond.0));
                    }
                    IrOp::SextI64 { dst, src }
                    | IrOp::F32FromI32 { dst, src }
                    | IrOp::F64FromI32 { dst, src }
                    | IrOp::F64FromI64 { dst, src }
                    | IrOp::F64FromF32 { dst, src }
                    | IrOp::F32FromF64 { dst, src }
                    | IrOp::F32FromI64 { dst, src }
                    | IrOp::I64FromF32 { dst, src }
                    | IrOp::I32FromI64 { dst, src }
                    | IrOp::I32FromF64 { dst, src }
                    | IrOp::I64FromF64 { dst, src }
                    | IrOp::I32FromF32 { dst, src }
                    | IrOp::F16FromI32 { dst, src }
                    | IrOp::I32FromF16 { dst, src } => {
                        uses.push(VReg(src.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::BytesNew { dst, len } => {
                        uses.push(VReg(len.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::BytesLen { dst, bytes } => {
                        uses.push(VReg(bytes.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::BytesGetU8 { dst, bytes, index } => {
                        uses.push(VReg(bytes.0));
                        uses.push(VReg(index.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::BytesSetU8 { bytes, index, value } => {
                        uses.push(VReg(value.0));
                        uses.push(VReg(bytes.0));
                        uses.push(VReg(index.0));
                    }
                    IrOp::BytesConcat2 { dst, a, b } => {
                        uses.push(VReg(a.0));
                        uses.push(VReg(b.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::BytesConcatMany { dst, parts } => {
                        uses.push(VReg(parts.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ListNil { dst } => {
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ListCons { dst, head, tail } => {
                        uses.push(VReg(head.0));
                        uses.push(VReg(tail.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ListHead { dst, list } | IrOp::ListTail { dst, list } | IrOp::ListIsNil { dst, list } => {
                        uses.push(VReg(list.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ArrayNew { dst, len } => {
                        uses.push(VReg(len.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ArrayLen { dst, arr } => {
                        uses.push(VReg(arr.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ArrayGet { dst, arr, index } => {
                        uses.push(VReg(arr.0));
                        uses.push(VReg(index.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ArraySet { arr, index, value } => {
                        uses.push(VReg(value.0));
                        uses.push(VReg(arr.0));
                        uses.push(VReg(index.0));
                    }
                    IrOp::ObjNew { dst } => {
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ObjHasAtom { dst, obj, .. } => {
                        uses.push(VReg(obj.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ObjGetAtom { dst, obj, .. } => {
                        uses.push(VReg(obj.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ObjSetAtom { obj, value, .. } => {
                        uses.push(VReg(value.0));
                        uses.push(VReg(obj.0));
                    }
                    IrOp::ObjGet { dst, obj, atom } => {
                        uses.push(VReg(obj.0));
                        uses.push(VReg(atom.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::ObjSet { obj, atom, value } => {
                        uses.push(VReg(value.0));
                        uses.push(VReg(obj.0));
                        uses.push(VReg(atom.0));
                    }
                    IrOp::Throw { payload } => {
                        uses.push(VReg(payload.0));
                    }
                    IrOp::Try { catch_dst, .. } => {
                        defs.push(VReg(catch_dst.0));
                    }
                    IrOp::EndTry => {}
                    IrOp::Call {
                        dst,
                        callee,
                        sig_id: _,
                        arg_base,
                        nargs,
                    } => {
                        if !is_direct_call {
                            uses.push(VReg(callee.0));
                        }
                        for i in 0..(*nargs as u32) {
                            uses.push(VReg(arg_base.0 + i));
                        }
                        defs.push(VReg(dst.0));
                    }
                    IrOp::Closure { dst, cap_base, ncaps, .. } => {
                        for i in 0..(*ncaps as u32) {
                            uses.push(VReg(cap_base.0 + i));
                        }
                        defs.push(VReg(dst.0));
                    }
                    IrOp::BindThis { dst, func, this } => {
                        uses.push(VReg(func.0));
                        uses.push(VReg(this.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::Phi { .. } => {
                        return Err(CompileError::new(
                            ErrorKind::Internal,
                            ins.span,
                            "Phi survived into emission (run phi elimination first)",
                        ));
                    }
                    // Anything else is rejected above.
                }
                infos.push(InstrInfo { uses, defs });

                // Update local constant-function tracking after computing defs.
                for &d in &infos.last().unwrap().defs {
                    let di = d.0 as usize;
                    if di < const_fun_of_vreg.len() {
                        const_fun_of_vreg[di] = None;
                    }
                }
                match &ins.op {
                    IrOp::ConstFun { dst, func_index } => {
                        let di = dst.0 as usize;
                        if di < const_fun_of_vreg.len() {
                            const_fun_of_vreg[di] = Some(map_func_index(*func_index));
                        }
                    }
                    IrOp::Mov { dst, src } => {
                        let di = dst.0 as usize;
                        let si = src.0 as usize;
                        if di < const_fun_of_vreg.len() && si < const_fun_of_vreg.len() {
                            const_fun_of_vreg[di] = const_fun_of_vreg[si];
                        }
                    }
                    _ => {}
                }
            }

            // Terminator
            let this_pc = block_start[bi] + (blk.insns.len() as u32);
            match &blk.term {
                IrTerminator::Ret { value } => {
                    vinsns.push(VInsn { op: Op::Ret as u8, a: reg(*value), b: 0, c: 0, imm: 0 });
                    infos.push(InstrInfo { uses: vec![VReg(value.0)], defs: vec![] });
                }
                IrTerminator::Jmp { target } => {
                    let to = blk_pc(&block_start, *target)?;
                    vinsns.push(VInsn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: delta(this_pc, to) });
                    infos.push(InstrInfo { uses: vec![], defs: vec![] });
                }
                IrTerminator::JmpIf { cond, then_tgt, else_tgt } => {
                    let then_pc = blk_pc(&block_start, *then_tgt)?;
                    let else_pc = blk_pc(&block_start, *else_tgt)?;
                    vinsns.push(VInsn { op: Op::JmpIf as u8, a: reg(*cond), b: 0, c: 0, imm: delta(this_pc, then_pc) });
                    vinsns.push(VInsn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: delta(this_pc + 1, else_pc) });
                    infos.push(InstrInfo { uses: vec![VReg(cond.0)], defs: vec![] });
                    infos.push(InstrInfo { uses: vec![], defs: vec![] });
                }
                IrTerminator::SwitchKind { kind, cases, default } => {
                    let ncases = cases.len();
                    if ncases > 255 {
                        return Err(CompileError::new(
                            ErrorKind::Codegen,
                            crate::ast::Span::point(0),
                            "SWITCH_KIND too many cases (max 255)",
                        ));
                    }
                    let table_end_pc = this_pc + 1 + (ncases as u32);
                    let def_pc = blk_pc(&block_start, *default)?;
                    let def_delta: u32 = ((def_pc as i32) - (table_end_pc as i32)) as u32;
                    vinsns.push(VInsn {
                        op: Op::SwitchKind as u8,
                        a: reg(*kind),
                        b: ncases as u32,
                        c: 0,
                        imm: def_delta,
                    });
                    infos.push(InstrInfo { uses: vec![VReg(kind.0)], defs: vec![] });
                    for (k, tgt) in cases {
                        let tgt_pc = blk_pc(&block_start, *tgt)?;
                        let d: u32 = ((tgt_pc as i32) - (table_end_pc as i32)) as u32;
                        vinsns.push(VInsn { op: Op::CaseKind as u8, a: *k as u32, b: 0, c: 0, imm: d });
                        infos.push(InstrInfo { uses: vec![], defs: vec![] });
                    }
                }
                IrTerminator::Unreachable => {
                    return Err(CompileError::new(
                        ErrorKind::Codegen,
                        crate::ast::Span::point(0),
                        "IR→bytecode bridge: unreachable terminator not supported",
                    ))
                }
            }
        }

        // Register allocation
        //
        // For now, we must preserve the VM call ABI: callee parameters live in regs 0..n-1.
        // Our typed-per-class allocator doesn't support pre-colored regs, so we only run it
        // for functions with no parameters (eg the program entry function).

        // Collect CallR callee vregs so we can avoid spilling them (spill/reload can corrupt
        // closure values when pop order is wrong or stack is shared).
        let call_callee_vregs: std::collections::HashSet<u32> = vinsns
            .iter()
            .filter(|vi| vi.op == Op::CallR as u8)
            .map(|vi| vi.b)
            .collect();

        // Sanity-check call windows: the IR relies on arg-window vregs having the exact
        // types described by the signature used to pin the window.
        for &(sig_id, arg_base_v, nargs) in &call_windows {
            let sig = ir
                .sigs
                .get(sig_id as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "bad fun sig id"))?;
            if sig.args.len() != (nargs as usize) {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "CALLR arg count does not match signature",
                ));
            }
            for i in 0..(nargs as u32) {
                let vi = (arg_base_v + i) as usize;
                if vi >= f.vreg_types.len() {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        crate::ast::Span::point(0),
                        "CALLR arg vreg out of range",
                    ));
                }
                let vt = f.vreg_types[vi];
                let st = sig.args[i as usize];
                if vt != st {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        crate::ast::Span::point(0),
                        format!("CALLR arg window type mismatch (vreg {}: {} != {})", vi, vt, st),
                    ));
                }
            }
        }

        let mut vreg_to_reg: Vec<u8> = vec![0; f.vreg_types.len()];
        let mut vreg_to_spill: Vec<Option<u32>> = vec![None; f.vreg_types.len()];
        let mut reg_types: Vec<u32> = Vec::new();
        let mut base: u16;
        let mut spill_reload_per_tid: Vec<(u32, [u8; 2])> = Vec::new();

        // Preserve ABI: params are fixed at 0..param_count-1.
        if (f.param_count as usize) > f.vreg_types.len() {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "bad param_count",
            ));
        }
        for i in 0..(f.param_count as usize) {
            vreg_to_reg[i] = i as u8;
        }
        reg_types.extend_from_slice(&f.vreg_types[..(f.param_count as usize)]);
        base = f.param_count as u16;

        // Add capture slots at the beginning (after params) so spill regs never overlap.
        let cap_start = if f.cap_vregs.is_empty() {
            0u32
        } else {
            let param_count = f.param_count as u32;
            if reg_types.len() + f.cap_vregs.len() > 256 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs",
                ));
            }
            for v in &f.cap_vregs {
                let tid = f.vreg_types.get(v.0 as usize).copied().unwrap_or(T_DYNAMIC);
                reg_types.push(tid);
            }
            for (i, v) in f.cap_vregs.iter().enumerate() {
                let idx = v.0 as usize;
                if idx >= vreg_to_reg.len() {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        crate::ast::Span::point(0),
                        "capture vreg out of range",
                    ));
                }
                vreg_to_reg[idx] = (param_count as u8)
                    .checked_add(i as u8)
                    .ok_or_else(|| {
                        CompileError::new(
                            ErrorKind::Internal,
                            crate::ast::Span::point(0),
                            "capture slot overflow",
                        )
                    })?;
            }
            base = (base as u32 + f.cap_vregs.len() as u32) as u16;
            param_count
        };

        let mut allow_multi_def_global: Vec<bool> = vec![false; f.vreg_types.len()];
        let mut def_counts: Vec<u32> = vec![0; f.vreg_types.len()];
        for ins in &infos {
            for &d in &ins.defs {
                if (d.0 as usize) < def_counts.len() {
                    def_counts[d.0 as usize] += 1;
                }
            }
        }
        for (i, &c) in def_counts.iter().enumerate() {
            if c > 1 {
                allow_multi_def_global[i] = true;
            }
        }

        // Mark vregs that are part of a call arg window; we'll pin them to the
        // per-signature arg blocks instead of allocating them normally.
        let mut is_arg_vreg: Vec<bool> = vec![false; f.vreg_types.len()];
        for &(_sig_id, arg_base_v, nargs) in &call_windows {
            if nargs == 0 {
                continue;
            }
            let end = arg_base_v
                .checked_add((nargs as u32).saturating_sub(1))
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "call arg window overflow"))?;
            if (end as usize) >= f.vreg_types.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "call arg window out of vreg range",
                ));
            }
            for v in arg_base_v..(arg_base_v + (nargs as u32)) {
                is_arg_vreg[v as usize] = true;
            }
        }
        // Also exclude closure capture-slot vregs from normal allocation; we pin them to the
        // end of the frame after allocating internal call arg blocks.
        for v in &f.cap_vregs {
            if (v.0 as usize) < is_arg_vreg.len() {
                is_arg_vreg[v.0 as usize] = true;
            }
        }

        // Only allocate vregs that appear in the instruction stream (opt passes may leave
        // orphan vregs in vreg_types that are never used/defined).
        let mut live_vregs: std::collections::HashSet<u32> = std::collections::HashSet::new();
        for ins in &infos {
            for &u in &ins.uses {
                live_vregs.insert(u.0);
            }
            for &d in &ins.defs {
                live_vregs.insert(d.0);
            }
        }

        let mut tids: Vec<TypeId> = f.vreg_types.iter().copied().collect();
        tids.sort();
        tids.dedup();
        // Allocate Dynamic last so other type classes reserve their slice first.
        tids.sort_by_key(|&tid| {
            let is_dyn = ir
                .types
                .get(tid as usize)
                .map_or(false, |te| te.kind == TypeKind::Dynamic);
            (is_dyn, tid)
        });

        for tid in tids {
            let mut globals: Vec<u32> = Vec::new();
            for (i, &t) in f.vreg_types.iter().enumerate() {
                if i >= (f.param_count as usize)
                    && !is_arg_vreg[i]
                    && t == tid
                    && live_vregs.contains(&(i as u32))
                {
                    globals.push(i as u32);
                }
            }
            if globals.is_empty() {
                continue;
            }

            let mut g2l: Vec<Option<u32>> = vec![None; f.vreg_types.len()];
            for (li, &gv) in globals.iter().enumerate() {
                g2l[gv as usize] = Some(li as u32);
            }

            let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
            for ins in &infos {
                let mut uses = Vec::new();
                let mut defs = Vec::new();
                for &u in &ins.uses {
                    if let Some(li) = g2l[u.0 as usize] {
                        uses.push(VReg(li));
                    }
                }
                for &d in &ins.defs {
                    if let Some(li) = g2l[d.0 as usize] {
                        defs.push(VReg(li));
                    }
                }
                cls_instrs.push(InstrInfo { uses, defs });
            }

            let mut allow_multi_def_local: Vec<bool> = vec![false; globals.len()];
            for (li, &gv) in globals.iter().enumerate() {
                allow_multi_def_local[li] = allow_multi_def_global[gv as usize];
            }

            // Reserve space for arg blocks + capture slots so spill reload regs never overlap.
            let arg_block_size: u16 = call_windows
                .iter()
                .map(|(sig_id, _, _)| *sig_id)
                .collect::<std::collections::HashSet<_>>()
                .iter()
                .filter_map(|&sig_id| ir.sigs.get(sig_id as usize))
                .map(|sig| sig.args.len() as u16)
                .sum();
            let cap_reserve = (f.cap_vregs.len() as u16) + arg_block_size;
            let remaining = 256u16
                .checked_sub(base)
                .and_then(|r| r.checked_sub(cap_reserve))
                .ok_or_else(|| {
                    CompileError::new(
                        ErrorKind::Codegen,
                        crate::ast::Span::point(0),
                        "register allocation exceeded 256 regs",
                    )
                })?;
            // Enable spilling for all types; VM SpillPush/Pop box/unbox via vm_box_from_typed/vm_store_from_boxed.
            // Never spill CallR callees (closures) - spill/reload can corrupt them.
            let spillable: Vec<bool> = globals
                .iter()
                .map(|&gv| !call_callee_vregs.contains(&gv))
                .collect();
            let (spillable, spill_policy, num_pregs) = if remaining <= 2 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs",
                ));
            } else {
                (spillable, SpillPolicy::Allow, remaining - 2)
            };
            let alloc = regalloc::lsra_allocate(
                num_pregs,
                globals.len() as u32,
                &cls_instrs,
                &spillable,
                spill_policy,
                Some(&allow_multi_def_local),
            )
            .map_err(|e| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("LSRA allocation failed: {:?}", e),
                )
            })?;

            let used = alloc.used_pregs;
            let spill_reload_base = base + used;
            let spill_reload_count = if alloc.used_spill_slots > 0 {
                spill_reload_per_tid.push((tid, [spill_reload_base as u8, spill_reload_base as u8 + 1]));
                2u16
            } else {
                0u16
            };
            if base as u32 + used as u32 + spill_reload_count as u32 > 256 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs",
                ));
            }

            let need = base as usize + used as usize + spill_reload_count as usize;
            if reg_types.len() < need {
                reg_types.resize(need, 0);
            }
            for i in 0..used {
                reg_types[base as usize + i as usize] = tid;
            }
            if spill_reload_count > 0 {
                for i in 0..spill_reload_count {
                    reg_types[spill_reload_base as usize + i as usize] = tid;
                }
            }
            for (li, &gv) in globals.iter().enumerate() {
                if let Some(p) = alloc.vreg_to_preg[li] {
                    vreg_to_reg[gv as usize] = (base + p.0) as u8;
                } else {
                    vreg_to_reg[gv as usize] = spill_reload_base as u8;
                    vreg_to_spill[gv as usize] = alloc.vreg_to_spill[li];
                }
            }
            base += used + spill_reload_count;
        }

        // Verify CallR callees were not spilled (spill/reload can corrupt closures).
        for &gv in &call_callee_vregs {
            if (gv as usize) < vreg_to_spill.len() && vreg_to_spill[gv as usize].is_some() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("CallR callee vreg {} was spilled (spillable should prevent this)", gv),
                ));
            }
        }

        // Allocate arg blocks per signature, and pin each call's arg-window vregs
        // to the corresponding contiguous physical register window.
        //
        // This removes the need for the old "expand CALLR into MOV* + CALLR" step.
        let mut arg_block_start: std::collections::HashMap<u32, u8> = std::collections::HashMap::new();
        if !call_windows.is_empty() {
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

            for &(sig_id, arg_base_v, nargs) in &call_windows {
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
        }

        // Fix: never map a non-Object vreg to a reg that holds an Object param.
        // (IR can produce AddF64 dst=param when param is incorrectly reused; this avoids VM validation failure.)
        let mut f64_escape_per_param: Vec<u8> = vec![0; f.param_count as usize];
        for param_idx in 0..(f.param_count as usize) {
            let param_tid = f.vreg_types.get(param_idx).copied().unwrap_or(0);
            let param_is_object = ir
                .types
                .get(param_tid as usize)
                .map_or(false, |te| te.kind == crate::jlyb::TypeKind::Object);
            if !param_is_object {
                continue;
            }
            let param_reg = vreg_to_reg.get(param_idx).copied().unwrap_or(0);
            for (v, &tid) in f.vreg_types.iter().enumerate() {
                if vreg_to_reg.get(v).copied() == Some(param_reg) && tid != param_tid {
                    if reg_types.len() >= 256 {
                        return Err(CompileError::new(
                            ErrorKind::Codegen,
                            crate::ast::Span::point(0),
                            "register allocation exceeded 256 regs (fixing param overlap)",
                        ));
                    }
                    reg_types.push(tid);
                    vreg_to_reg[v] = (reg_types.len() - 1) as u8;
                }
            }
        }
        // Handle AddF64 dst=param vreg when param is Object: use escape regs (Object can't hold F64).
        for v in 0..(f.param_count as usize) {
            let param_tid = f.vreg_types.get(v).copied().unwrap_or(0);
            let param_is_object = ir
                .types
                .get(param_tid as usize)
                .map_or(false, |te| te.kind == crate::jlyb::TypeKind::Object);
            if param_is_object && vinsns.iter().any(|vi| vi.op == Op::AddF64 as u8 && (vi.a as usize) == v) {
                if reg_types.len() >= 256 {
                    return Err(CompileError::new(
                        ErrorKind::Codegen,
                        crate::ast::Span::point(0),
                        "register allocation exceeded 256 regs (F64 escape)",
                    ));
                }
                reg_types.push(T_F64);
                f64_escape_per_param[v] = (reg_types.len() - 1) as u8;
            }
        }

        // Typed-slot invariant: a physical register never changes type.
        // Every vreg must map to a preg whose static type matches.
        for (i, &tid) in f.vreg_types.iter().enumerate() {
            if !live_vregs.contains(&(i as u32)) {
                continue; // dead vreg (eg after DCE), skip type check
            }
            let r = vreg_to_reg
                .get(i)
                .copied()
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, crate::ast::Span::point(0), "vreg_to_reg out of range"))?
                as usize;
            let rt = reg_types.get(r).copied().ok_or_else(|| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "vreg mapped to missing reg_types entry",
                )
            })?;
            if rt != tid {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "typed slot invariant violated: vreg {} (tid={}) mapped to reg {} (tid={})",
                        i, tid, r, rt
                    ),
                ));
            }
        }

        // Compute last-def PC per vreg (for spill pop order)
        let mut last_def_pc: Vec<u32> = vec![0; f.vreg_types.len()];
        for (pc, info) in infos.iter().enumerate() {
            for &d in &info.defs {
                if (d.0 as usize) < last_def_pc.len() {
                    last_def_pc[d.0 as usize] = pc as u32;
                }
            }
        }

        let insns: Vec<Insn> = if !spill_reload_per_tid.is_empty() {
            spill::insert_spill_ops(
                &vinsns,
                &infos,
                &vreg_to_reg,
                &vreg_to_spill,
                &f.vreg_types,
                &spill_reload_per_tid,
                &last_def_pc,
                Some(&f64_escape_per_param),
            )
        } else {
            let mut out: Vec<Insn> = Vec::with_capacity(vinsns.len());
            for vi in vinsns.iter() {
                // Map operands (vreg -> preg); vi.a, vi.b, vi.c are vreg indices (u32).
                let (a, b, c, imm) = match vi.op {
                    x if x == Op::ConstBool as u8
                        || x == Op::ConstI32 as u8
                        || x == Op::ConstI8Imm as u8
                        || x == Op::ConstF16 as u8
                        || x == Op::ConstI64 as u8
                        || x == Op::ConstBytes as u8
                        || x == Op::ConstF32 as u8
                        || x == Op::ConstF64 as u8
                        || x == Op::ConstNull as u8
                        || x == Op::ConstFun as u8 =>
                    {
                        (vreg_to_reg[vi.a as usize], 0, vi.c as u8, vi.imm)
                    }
                    x if x == Op::Ret as u8 || x == Op::Throw as u8 => {
                        (vreg_to_reg[vi.a as usize], 0, 0, vi.imm)
                    }
                    x if x == Op::Mov as u8
                        || x == Op::NegI32 as u8
                        || x == Op::NegI64 as u8
                        || x == Op::NegF32 as u8
                        || x == Op::NegF64 as u8
                        || x == Op::NotBool as u8
                        || x == Op::Kindof as u8
                        || x == Op::BytesNew as u8
                        || x == Op::BytesLen as u8
                        || x == Op::ListHead as u8
                        || x == Op::ListTail as u8
                        || x == Op::ListIsNil as u8
                        || x == Op::ArrayNew as u8
                        || x == Op::ArrayLen as u8
                        || x == Op::ToDyn as u8
                        || x == Op::FromDynI8 as u8
                        || x == Op::FromDynI16 as u8
                        || x == Op::FromDynI32 as u8
                        || x == Op::FromDynF16 as u8
                        || x == Op::FromDynF32 as u8
                        || x == Op::FromDynI64 as u8
                        || x == Op::FromDynF64 as u8
                        || x == Op::FromDynBool as u8
                        || x == Op::FromDynPtr as u8
                        || x == Op::SextI64 as u8
                        || x == Op::I32FromI64 as u8
                        || x == Op::F32FromI32 as u8
                        || x == Op::F64FromI32 as u8
                        || x == Op::F64FromI64 as u8
                        || x == Op::I64FromF64 as u8
                        || x == Op::I32FromF64 as u8
                        || x == Op::I32FromF32 as u8
                        || x == Op::F64FromF32 as u8
                        || x == Op::F32FromF64 as u8
                        || x == Op::F32FromI64 as u8
                        || x == Op::I64FromF32 as u8
                        || x == Op::SextI16 as u8
                        || x == Op::TruncI8 as u8
                        || x == Op::TruncI16 as u8
                        || x == Op::F16FromF32 as u8
                        || x == Op::F32FromF16 as u8 =>
                    {
                        (vreg_to_reg[vi.a as usize], vreg_to_reg[vi.b as usize], 0, vi.imm)
                    }
                    x if x == Op::AddI32 as u8
                        || x == Op::SubI32 as u8
                        || x == Op::MulI32 as u8
                        || x == Op::DivF32 as u8
                        || x == Op::DivF64 as u8
                        || x == Op::AddI64 as u8
                        || x == Op::SubI64 as u8
                        || x == Op::MulI64 as u8
                        || x == Op::AddF16 as u8
                        || x == Op::SubF16 as u8
                        || x == Op::MulF16 as u8
                        || x == Op::AddF32 as u8
                        || x == Op::SubF32 as u8
                        || x == Op::MulF32 as u8
                        || x == Op::AddF64 as u8
                        || x == Op::SubF64 as u8
                        || x == Op::MulF64 as u8
                        || x == Op::EqI32 as u8
                        || x == Op::EqI64 as u8
                        || x == Op::EqF32 as u8
                        || x == Op::EqF64 as u8
                        || x == Op::LtI32 as u8
                        || x == Op::LtI64 as u8
                        || x == Op::LtF32 as u8
                        || x == Op::LtF64 as u8
                        || x == Op::Physeq as u8
                        || x == Op::BytesConcat2 as u8
                        || x == Op::ListCons as u8
                        || x == Op::BytesGetU8 as u8
                        || x == Op::BytesSetU8 as u8
                        || x == Op::ArrayGet as u8
                        || x == Op::ArraySet as u8 =>
                    {
                        let dst_reg = if x == Op::AddF64 as u8
                            || x == Op::SubF64 as u8
                            || x == Op::MulF64 as u8
                            || x == Op::DivF64 as u8
                        {
                            let v = vi.a as usize;
                            if v < f64_escape_per_param.len() && f64_escape_per_param[v] != 0 {
                                f64_escape_per_param[v]
                            } else {
                                vreg_to_reg[vi.a as usize]
                            }
                        } else {
                            vreg_to_reg[vi.a as usize]
                        };
                        (
                            dst_reg,
                            vreg_to_reg[vi.b as usize],
                            vreg_to_reg[vi.c as usize],
                            vi.imm,
                        )
                    }
                    x if x == Op::BytesConcatMany as u8 => {
                        (vreg_to_reg[vi.a as usize], vreg_to_reg[vi.b as usize], 0, vi.imm)
                    }
                    x if x == Op::ObjNew as u8
                        || x == Op::ListNil as u8
                        || x == Op::ConstAtom as u8 =>
                    {
                        (vreg_to_reg[vi.a as usize], 0, 0, vi.imm)
                    }
                    x if x == Op::ObjGetAtom as u8
                        || x == Op::ObjHasAtom as u8
                        || x == Op::ObjSetAtom as u8 =>
                    {
                        (vreg_to_reg[vi.a as usize], vreg_to_reg[vi.b as usize], 0, vi.imm)
                    }
                    x if x == Op::ObjGet as u8 || x == Op::ObjSet as u8 => (
                        vreg_to_reg[vi.a as usize],
                        vreg_to_reg[vi.b as usize],
                        vreg_to_reg[vi.c as usize],
                        vi.imm,
                    ),
                    x if x == Op::Closure as u8 => (
                        vreg_to_reg[vi.a as usize],
                        vreg_to_reg[vi.b as usize],
                        vi.c as u8, /* ncaps */
                        vi.imm,
                    ),
                    x if x == Op::BindThis as u8 => (
                        vreg_to_reg[vi.a as usize],
                        vreg_to_reg[vi.b as usize],
                        vreg_to_reg[vi.c as usize],
                        vi.imm,
                    ),
                    x if x == Op::JmpIf as u8 || x == Op::Assert as u8 => {
                        (vreg_to_reg[vi.a as usize], 0, 0, vi.imm)
                    }
                    x if x == Op::Try as u8 => (vreg_to_reg[vi.a as usize], vi.b as u8, 0, vi.imm), // b = trap_only
                    x if x == Op::SwitchKind as u8 => (vreg_to_reg[vi.a as usize], vi.b as u8, 0, vi.imm), // b = ncases
                    x if x == Op::EndTry as u8 || x == Op::Jmp as u8 || x == Op::CaseKind as u8 => {
                        (vi.a as u8, vi.b as u8, vi.c as u8, vi.imm)
                    }
                    x if x == Op::CallR as u8 => {
                        let arg_base_v = vi.imm as usize;
                        if arg_base_v >= vreg_to_reg.len() {
                            return Err(CompileError::new(
                                ErrorKind::Internal,
                                crate::ast::Span::point(0),
                                "CALLR arg_base vreg out of range",
                            ));
                        }
                        (
                            vreg_to_reg[vi.a as usize],
                            vreg_to_reg[vi.b as usize],
                            vi.c as u8,
                            vreg_to_reg[arg_base_v] as u32,
                        )
                    }
                    x if x == Op::Call as u8 => {
                        (vreg_to_reg[vi.a as usize], vreg_to_reg[vi.b as usize], vi.c as u8, vi.imm)
                    }
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Internal,
                            crate::ast::Span::point(0),
                            format!("unexpected opcode in IR emission mapping: {}", vi.op),
                        ));
                    }
                };
                out.push(Insn {
                    op: vi.op,
                    a,
                    b,
                    c,
                    imm,
                });
            }
            out
        };

        let insns = crate::peephole::peephole(&insns);

        Ok(Function { reg_types, cap_start, insns })
    }

    let mut const_f64 = ir.const_f64.clone();

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
        entry: prelude_count + 1 + (ir.entry as u32),
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
        if let Err(e) = emit_ir_module(&ir) {
            panic!("emit failed: {}\nIR:\n{:#?}", e.render(src, None), ir.funcs[0]);
        }
    }

    #[test]
    fn emit_if_expression_ir_has_jumps() {
        let sp = Span::new(0, 1);
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(
                ExprKind::If {
                    cond: Box::new(Spanned::new(ExprKind::BoolLit(true), sp)),
                    then_br: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), sp)),
                    else_br: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), sp)),
                },
                sp,
            ),
        };
        let (hir, info) = crate::semantic::analyze_program(&prog).unwrap();
        let mut ir = lower_program_to_ir(&hir.program, &info).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
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
        let any_spill_push = m.funcs.iter().any(|f| f.insns.iter().any(|i| i.op == Op::SpillPush as u8));
        let any_spill_pop = m.funcs.iter().any(|f| f.insns.iter().any(|i| i.op == Op::SpillPop as u8));
        assert!(any_spill_push, "expected SpillPush in some emitted function");
        assert!(any_spill_pop, "expected SpillPop in some emitted function");
    }
}
