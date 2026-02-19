use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::jlyb::{self, Function, Insn, Module, Op};
use crate::regalloc::{self, InstrInfo, SpillPolicy, VReg};

fn reg(v: VRegId) -> u8 {
    v.0 as u8
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

    fn emit_function(ir: &IrModule, f: &crate::ir::IrFunction) -> Result<Function, CompileError> {
        if f.vreg_types.len() > 256 {
            return Err(CompileError::new(
                ErrorKind::Codegen,
                crate::ast::Span::point(0),
                "IR→bytecode bridge exceeded 256 regs",
            ));
        }

        // Precompute block start PCs in vinsn space.
        let mut block_start: Vec<u32> = vec![0; f.blocks.len()];
        let mut pc: u32 = 0;
        for (bi, blk) in f.blocks.iter().enumerate() {
            block_start[bi] = pc;
            pc += blk.insns.len() as u32;
            pc += term_size(&blk.term);
        }

        let mut vinsns: Vec<Insn> = Vec::new(); // operands are vregs for now
        let mut infos: Vec<InstrInfo> = Vec::new();
        // Track call arg windows so we can allocate/pin them contiguously.
        let mut call_windows: Vec<(u32, u32, u8)> = Vec::new(); // (sig_id, arg_base_vreg, nargs)

        for (bi, blk) in f.blocks.iter().enumerate() {
            for (ii, ins) in blk.insns.iter().enumerate() {
                let this_pc = block_start[bi] + (ii as u32);
                match &ins.op {
                    IrOp::ConstBytes { dst, pool_index } => vinsns.push(Insn {
                        op: Op::ConstBytes as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *pool_index,
                    }),
                    IrOp::ConstBool { dst, imm } => vinsns.push(Insn {
                        op: Op::ConstBool as u8,
                        a: reg(*dst),
                        b: 0,
                        c: if *imm { 1 } else { 0 },
                        imm: 0,
                    }),
                    IrOp::ConstI32 { dst, imm } => vinsns.push(Insn {
                        op: Op::ConstI32 as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *imm as u32,
                    }),
                    IrOp::ConstNull { dst } => vinsns.push(Insn { op: Op::ConstNull as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                    IrOp::ConstAtom { dst, atom_id } => vinsns.push(Insn { op: Op::ConstAtom as u8, a: reg(*dst), b: 0, c: 0, imm: *atom_id }),
                    IrOp::ConstFun { dst, func_index } => vinsns.push(Insn {
                        op: Op::ConstFun as u8,
                        a: reg(*dst),
                        b: 0,
                        c: 0,
                        imm: *func_index,
                    }),
                    IrOp::Mov { dst, src } => vinsns.push(Insn { op: Op::Mov as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::AddI32 { dst, a, b } => vinsns.push(Insn { op: Op::AddI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::SubI32 { dst, a, b } => vinsns.push(Insn { op: Op::SubI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NegI32 { dst, src } => vinsns.push(Insn { op: Op::NegI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::EqI32 { dst, a, b } => vinsns.push(Insn { op: Op::EqI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::LtI32 { dst, a, b } => vinsns.push(Insn { op: Op::LtI32 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::Physeq { dst, a, b } => vinsns.push(Insn { op: Op::Physeq as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                    IrOp::NotBool { dst, src } => vinsns.push(Insn { op: Op::NotBool as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::Kindof { dst, src } => vinsns.push(Insn { op: Op::Kindof as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::BytesNew { dst, len } => vinsns.push(Insn { op: Op::BytesNew as u8, a: reg(*dst), b: reg(*len), c: 0, imm: 0 }),
                    IrOp::BytesLen { dst, bytes } => vinsns.push(Insn { op: Op::BytesLen as u8, a: reg(*dst), b: reg(*bytes), c: 0, imm: 0 }),
                    IrOp::BytesGetU8 { dst, bytes, index } => vinsns.push(Insn {
                        op: Op::BytesGetU8 as u8,
                        a: reg(*dst),
                        b: reg(*bytes),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::BytesSetU8 { bytes, index, value } => vinsns.push(Insn {
                        op: Op::BytesSetU8 as u8,
                        a: reg(*value),
                        b: reg(*bytes),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::BytesConcat2 { dst, a, b } => vinsns.push(Insn {
                        op: Op::BytesConcat2 as u8,
                        a: reg(*dst),
                        b: reg(*a),
                        c: reg(*b),
                        imm: 0,
                    }),
                    IrOp::BytesConcatMany { dst, parts } => vinsns.push(Insn {
                        op: Op::BytesConcatMany as u8,
                        a: reg(*dst),
                        b: reg(*parts),
                        c: 0,
                        imm: 0,
                    }),
                    IrOp::ListNil { dst } => vinsns.push(Insn { op: Op::ListNil as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                    IrOp::ListCons { dst, head, tail } => vinsns.push(Insn {
                        op: Op::ListCons as u8,
                        a: reg(*dst),
                        b: reg(*head),
                        c: reg(*tail),
                        imm: 0,
                    }),
                    IrOp::ListHead { dst, list } => {
                        vinsns.push(Insn { op: Op::ListHead as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 })
                    }
                    IrOp::ListTail { dst, list } => {
                        vinsns.push(Insn { op: Op::ListTail as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 })
                    }
                    IrOp::ListIsNil { dst, list } => {
                        vinsns.push(Insn { op: Op::ListIsNil as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 })
                    }
                    IrOp::ArrayNew { dst, len } => vinsns.push(Insn { op: Op::ArrayNew as u8, a: reg(*dst), b: reg(*len), c: 0, imm: 0 }),
                    IrOp::ArrayLen { dst, arr } => vinsns.push(Insn { op: Op::ArrayLen as u8, a: reg(*dst), b: reg(*arr), c: 0, imm: 0 }),
                    IrOp::ArrayGet { dst, arr, index } => vinsns.push(Insn {
                        op: Op::ArrayGet as u8,
                        a: reg(*dst),
                        b: reg(*arr),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::ArraySet { arr, index, value } => vinsns.push(Insn {
                        op: Op::ArraySet as u8,
                        a: reg(*value),
                        b: reg(*arr),
                        c: reg(*index),
                        imm: 0,
                    }),
                    IrOp::ObjNew { dst } => vinsns.push(Insn { op: Op::ObjNew as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                    IrOp::ObjHasAtom { dst, obj, atom_id } => vinsns.push(Insn {
                        op: Op::ObjHasAtom as u8,
                        a: reg(*dst),
                        b: reg(*obj),
                        c: 0,
                        imm: *atom_id,
                    }),
                    IrOp::ObjGetAtom { dst, obj, atom_id } => vinsns.push(Insn {
                        op: Op::ObjGetAtom as u8,
                        a: reg(*dst),
                        b: reg(*obj),
                        c: 0,
                        imm: *atom_id,
                    }),
                    IrOp::ObjSetAtom { obj, atom_id, value } => vinsns.push(Insn {
                        op: Op::ObjSetAtom as u8,
                        a: reg(*value),
                        b: reg(*obj),
                        c: 0,
                        imm: *atom_id,
                    }),
                    IrOp::ObjGet { dst, obj, atom } => vinsns.push(Insn { op: Op::ObjGet as u8, a: reg(*dst), b: reg(*obj), c: reg(*atom), imm: 0 }),
                    IrOp::ObjSet { obj, atom, value } => vinsns.push(Insn { op: Op::ObjSet as u8, a: reg(*value), b: reg(*obj), c: reg(*atom), imm: 0 }),
                    IrOp::ToDyn { dst, src } => vinsns.push(Insn { op: Op::ToDyn as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::FromDynI32 { dst, src } => {
                        vinsns.push(Insn { op: Op::FromDynI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynBool { dst, src } => {
                        vinsns.push(Insn { op: Op::FromDynBool as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 })
                    }
                    IrOp::FromDynPtr { dst, src } => vinsns.push(Insn { op: Op::FromDynPtr as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                    IrOp::Throw { payload } => vinsns.push(Insn { op: Op::Throw as u8, a: reg(*payload), b: 0, c: 0, imm: 0 }),
                    IrOp::Try { catch_dst, catch_block } => {
                        let catch_pc = blk_pc(&block_start, *catch_block)?;
                        vinsns.push(Insn { op: Op::Try as u8, a: reg(*catch_dst), b: 0, c: 0, imm: delta(this_pc, catch_pc) });
                    }
                    IrOp::EndTry => vinsns.push(Insn { op: Op::EndTry as u8, a: 0, b: 0, c: 0, imm: 0 }),
                    IrOp::Call {
                        dst,
                        callee,
                        sig_id,
                        arg_base,
                        nargs,
                    } => {
                        // Encode arg_base vreg in imm for the virtual stream. We'll map it to
                        // a physical register index during final emission.
                        vinsns.push(Insn {
                            op: Op::CallR as u8,
                            a: reg(*dst),
                            b: reg(*callee),
                            c: *nargs,
                            imm: arg_base.0,
                        });
                        call_windows.push((*sig_id, arg_base.0, *nargs));
                    }
                    IrOp::Closure {
                        dst,
                        func_index,
                        cap_sig_id,
                        cap_base,
                        ncaps,
                    } => {
                        vinsns.push(Insn {
                            op: Op::Closure as u8,
                            a: reg(*dst),
                            b: reg(*cap_base),
                            c: *ncaps,
                            imm: *func_index,
                        });
                        call_windows.push((*cap_sig_id, cap_base.0, *ncaps));
                    }
                    IrOp::BindThis { dst, func, this } => {
                        vinsns.push(Insn { op: Op::BindThis as u8, a: reg(*dst), b: reg(*func), c: reg(*this), imm: 0 });
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
                    | IrOp::EqI32 { dst, a, b }
                    | IrOp::LtI32 { dst, a, b }
                    | IrOp::Physeq { dst, a, b } => {
                        uses.push(VReg(a.0));
                        uses.push(VReg(b.0));
                        defs.push(VReg(dst.0));
                    }
                    IrOp::NegI32 { dst, src }
                    | IrOp::NotBool { dst, src }
                    | IrOp::Kindof { dst, src }
                    | IrOp::ToDyn { dst, src }
                    | IrOp::FromDynI32 { dst, src }
                    | IrOp::FromDynBool { dst, src }
                    | IrOp::FromDynPtr { dst, src } => {
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
                        uses.push(VReg(callee.0));
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
            }

            // Terminator
            let this_pc = block_start[bi] + (blk.insns.len() as u32);
            match &blk.term {
                IrTerminator::Ret { value } => {
                    vinsns.push(Insn { op: Op::Ret as u8, a: reg(*value), b: 0, c: 0, imm: 0 });
                    infos.push(InstrInfo { uses: vec![VReg(value.0)], defs: vec![] });
                }
                IrTerminator::Jmp { target } => {
                    let to = blk_pc(&block_start, *target)?;
                    vinsns.push(Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: delta(this_pc, to) });
                    infos.push(InstrInfo { uses: vec![], defs: vec![] });
                }
                IrTerminator::JmpIf { cond, then_tgt, else_tgt } => {
                    let then_pc = blk_pc(&block_start, *then_tgt)?;
                    let else_pc = blk_pc(&block_start, *else_tgt)?;
                    vinsns.push(Insn { op: Op::JmpIf as u8, a: reg(*cond), b: 0, c: 0, imm: delta(this_pc, then_pc) });
                    vinsns.push(Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: delta(this_pc + 1, else_pc) });
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
                    vinsns.push(Insn {
                        op: Op::SwitchKind as u8,
                        a: reg(*kind),
                        b: ncases as u8,
                        c: 0,
                        imm: def_delta,
                    });
                    infos.push(InstrInfo { uses: vec![VReg(kind.0)], defs: vec![] });
                    for (k, tgt) in cases {
                        let tgt_pc = blk_pc(&block_start, *tgt)?;
                        let d: u32 = ((tgt_pc as i32) - (table_end_pc as i32)) as u32;
                        vinsns.push(Insn { op: Op::CaseKind as u8, a: *k, b: 0, c: 0, imm: d });
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
        // for functions with no parameters (e.g. the program entry function).

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
        let mut reg_types: Vec<u32> = Vec::new();
        let mut base: u16;

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

        let mut tids: Vec<TypeId> = f.vreg_types.iter().copied().collect();
        tids.sort();
        tids.dedup();

        for tid in tids {
            let mut globals: Vec<u32> = Vec::new();
            for (i, &t) in f.vreg_types.iter().enumerate() {
                if i >= (f.param_count as usize) && !is_arg_vreg[i] && t == tid {
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

            let spillable = vec![false; globals.len()];
            let alloc = regalloc::lsra_allocate(
                256,
                globals.len() as u32,
                &cls_instrs,
                &spillable,
                SpillPolicy::Forbid,
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
            if base as u32 + used as u32 > 256 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs",
                ));
            }

            let need = base as usize + used as usize;
            if reg_types.len() < need {
                reg_types.resize(need, 0);
            }
            for i in 0..used {
                reg_types[base as usize + i as usize] = tid;
            }
            for (li, &gv) in globals.iter().enumerate() {
                let p = alloc.vreg_to_preg[li].expect("unexpected spill");
                vreg_to_reg[gv as usize] = (base + p.0) as u8;
            }
            base += used;
        }

        // Allocate arg blocks per signature, and pin each call's arg-window vregs
        // to the corresponding contiguous physical register window.
        //
        // This removes the need for the old "expand CALLR into MOV* + CALLR" step.
        let mut arg_block_start: std::collections::HashMap<u32, u8> = std::collections::HashMap::new();
        if !call_windows.is_empty() {
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

        // Allocate capture slots at the end of the frame, and pin the capture-slot vregs to them.
        if !f.cap_vregs.is_empty() {
            if reg_types.len() + f.cap_vregs.len() > 256 {
                return Err(CompileError::new(
                    ErrorKind::Codegen,
                    crate::ast::Span::point(0),
                    "register allocation exceeded 256 regs",
                ));
            }
            let start = reg_types.len() as u8;
            for _ in 0..f.cap_vregs.len() {
                reg_types.push(3); // Dynamic
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
                vreg_to_reg[idx] = start + (i as u8);
            }
        }

        // Typed-slot invariant: a physical register never changes type.
        // Every vreg must map to a preg whose static type matches.
        for (i, &tid) in f.vreg_types.iter().enumerate() {
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
                    "typed slot invariant violated (preg type mismatch)",
                ));
            }
        }

        let mut insns: Vec<Insn> = Vec::with_capacity(vinsns.len());

        for vi in vinsns.iter() {
            // Map operands (vreg -> preg) and any vreg-encoded immediates.
            let mut out = *vi;
            match out.op {
                x if x == Op::ConstBool as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::ConstI32 as u8
                    || x == Op::ConstBytes as u8
                    || x == Op::ConstNull as u8
                    || x == Op::ConstFun as u8 =>
                {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::Ret as u8 || x == Op::Throw as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::Mov as u8
                    || x == Op::NegI32 as u8
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
                    || x == Op::FromDynI32 as u8
                    || x == Op::FromDynBool as u8
                    || x == Op::FromDynPtr as u8 =>
                {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                }
                x if x == Op::AddI32 as u8
                    || x == Op::SubI32 as u8
                    || x == Op::EqI32 as u8
                    || x == Op::LtI32 as u8
                    || x == Op::Physeq as u8
                    || x == Op::BytesConcat2 as u8
                    || x == Op::ListCons as u8
                    || x == Op::BytesGetU8 as u8
                    || x == Op::BytesSetU8 as u8
                    || x == Op::ArrayGet as u8
                    || x == Op::ArraySet as u8 =>
                {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                    out.c = vreg_to_reg[out.c as usize];
                }
                x if x == Op::BytesConcatMany as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                }
                x if x == Op::ObjNew as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::ListNil as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::ObjGetAtom as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                }
                x if x == Op::ObjHasAtom as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                }
                x if x == Op::ObjSetAtom as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                }
                x if x == Op::ConstAtom as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::ObjGet as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                    out.c = vreg_to_reg[out.c as usize];
                }
                x if x == Op::ObjSet as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                    out.c = vreg_to_reg[out.c as usize];
                }
                x if x == Op::Closure as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                    // out.c is ncaps immediate
                }
                x if x == Op::BindThis as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                    out.c = vreg_to_reg[out.c as usize];
                }
                x if x == Op::JmpIf as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::Try as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                }
                x if x == Op::EndTry as u8 || x == Op::Jmp as u8 => {}
                x if x == Op::SwitchKind as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    // out.b is ncases (u8 immediate), not a vreg.
                }
                x if x == Op::CaseKind as u8 => {
                    // data-only: out.a is kind_u8, out.imm is delta
                }
                x if x == Op::CallR as u8 => {
                    out.a = vreg_to_reg[out.a as usize];
                    out.b = vreg_to_reg[out.b as usize];
                    let arg_base_v = out.imm as usize;
                    if arg_base_v >= vreg_to_reg.len() {
                        return Err(CompileError::new(
                            ErrorKind::Internal,
                            crate::ast::Span::point(0),
                            "CALLR arg_base vreg out of range",
                        ));
                    }
                    out.imm = vreg_to_reg[arg_base_v] as u32;
                }
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Internal,
                        crate::ast::Span::point(0),
                        format!("unexpected opcode in IR emission mapping: {}", out.op),
                    ));
                }
            }
            insns.push(out);
        }

        Ok(Function { reg_types, insns })
    }

    let mut funcs: Vec<Function> = Vec::with_capacity((jlyb::PRELUDE_FUN_COUNT as usize) + ir.funcs.len());
    // Inject prelude at fixed indices 0..PRELUDE_FUN_COUNT.
    funcs.extend(jlyb::prelude_funcs_for_program());
    for f in &ir.funcs {
        funcs.push(emit_function(ir, f)?);
    }

    Ok(Module {
        types: ir.types.clone(),
        sigs: ir.sigs.clone(),
        atoms: ir.atoms.clone(),
        const_bytes: ir.const_bytes.clone(),
        funcs,
        entry: (jlyb::PRELUDE_FUN_COUNT + (ir.entry as u32)),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ExprKind, Program, Span, Spanned};
    use crate::P;
    use crate::lower::lower_program_to_ir;
    use crate::phi;

    #[test]
    fn emit_minimal_bytes_ir() {
        let prog = Program {
            stmts: vec![],
            expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(0, 2)),
        };
        let mut ir = lower_program_to_ir(&prog).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        let entry = m.entry as usize;
        assert_eq!(m.const_bytes.len(), 1);
        assert!(!m.funcs[entry].insns.is_empty());
        assert_eq!(m.funcs[entry].insns[0].op, Op::ConstBytes as u8);
        assert_eq!(m.funcs[entry].insns.last().unwrap().op, Op::Ret as u8);
    }

    #[test]
    fn emit_match_basic_ir() {
        let src = "let x = 2; match (x) { 1 => { \"bad\" }, 2 => { \"ok\" }, _ => { \"bad\" }, }";
        let mut p = P::new(src);
        let prog = p.parse_program().unwrap();
        let mut ir = lower_program_to_ir(&prog).unwrap();
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
        let mut ir = lower_program_to_ir(&prog).unwrap();
        phi::eliminate_phis(&mut ir).unwrap();
        let m = emit_ir_module(&ir).unwrap();
        let entry = m.entry as usize;
        assert!(m.funcs[entry].insns.iter().any(|i| i.op == Op::JmpIf as u8));
        assert!(m.funcs[entry].insns.iter().any(|i| i.op == Op::Jmp as u8));
    }
}

