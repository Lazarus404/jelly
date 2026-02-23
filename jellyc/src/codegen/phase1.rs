use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule, IrOp, IrTerminator};
use crate::jlyb::{Op, TypeKind};
use crate::regalloc::{InstrInfo, VReg};
use crate::regalloc::spill::VInsn;

use super::{blk_pc, block_order_rpo, delta, reg, term_size, VirtualStream};
use std::collections::HashSet;

pub(super) fn build_virtual_stream(
    ir: &IrModule,
    f: &IrFunction,
    map_func_index: &impl Fn(u32) -> u32,
) -> Result<VirtualStream, CompileError> {
    // Precompute block start PCs in vinsn space.
    //
    // NOTE: allocation is based on a linearized instruction stream. Emitting
    // blocks in reverse-postorder (RPO) avoids placing loop exits/returns "in
    // the middle" of the linear stream, which can otherwise break liveness
    // approximations and yield incorrect allocations.
    let order = block_order_rpo(f);
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
                IrOp::ConstBytes { dst, pool_index } => vinsns.push(VInsn { op: Op::ConstBytes as u8, a: reg(*dst), b: 0, c: 0, imm: *pool_index }),
                IrOp::ConstI64 { dst, pool_index } => vinsns.push(VInsn { op: Op::ConstI64 as u8, a: reg(*dst), b: 0, c: 0, imm: *pool_index }),
                IrOp::ConstBool { dst, imm } => vinsns.push(VInsn { op: Op::ConstBool as u8, a: reg(*dst), b: 0, c: if *imm { 1 } else { 0 }, imm: 0 }),
                IrOp::ConstI32 { dst, imm } => vinsns.push(VInsn { op: Op::ConstI32 as u8, a: reg(*dst), b: 0, c: 0, imm: *imm as u32 }),
                IrOp::ConstF32 { dst, bits } => vinsns.push(VInsn { op: Op::ConstF32 as u8, a: reg(*dst), b: 0, c: 0, imm: *bits }),
                IrOp::ConstI8Imm { dst, imm } => vinsns.push(VInsn { op: Op::ConstI8Imm as u8, a: reg(*dst), b: 0, c: *imm as u32, imm: 0 }),
                IrOp::ConstF16 { dst, bits } => vinsns.push(VInsn { op: Op::ConstF16 as u8, a: reg(*dst), b: 0, c: 0, imm: *bits as u32 }),
                IrOp::ConstF64 { dst, pool_index } => vinsns.push(VInsn { op: Op::ConstF64 as u8, a: reg(*dst), b: 0, c: 0, imm: *pool_index }),
                IrOp::ConstNull { dst } => vinsns.push(VInsn { op: Op::ConstNull as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                IrOp::ConstAtom { dst, atom_id } => vinsns.push(VInsn { op: Op::ConstAtom as u8, a: reg(*dst), b: 0, c: 0, imm: *atom_id }),
                IrOp::ConstFun { dst, func_index } => vinsns.push(VInsn { op: Op::ConstFun as u8, a: reg(*dst), b: 0, c: 0, imm: map_func_index(*func_index) }),
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
                                (Some(sk), Some(dk)) if sk != dk => match (sk, dk) {
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
                                },
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
                IrOp::BytesGetU8 { dst, bytes, index } => vinsns.push(VInsn { op: Op::BytesGetU8 as u8, a: reg(*dst), b: reg(*bytes), c: reg(*index), imm: 0 }),
                IrOp::BytesSetU8 { bytes, index, value } => vinsns.push(VInsn { op: Op::BytesSetU8 as u8, a: reg(*value), b: reg(*bytes), c: reg(*index), imm: 0 }),
                IrOp::BytesConcat2 { dst, a, b } => vinsns.push(VInsn { op: Op::BytesConcat2 as u8, a: reg(*dst), b: reg(*a), c: reg(*b), imm: 0 }),
                IrOp::BytesConcatMany { dst, parts } => vinsns.push(VInsn { op: Op::BytesConcatMany as u8, a: reg(*dst), b: reg(*parts), c: 0, imm: 0 }),
                IrOp::ListNil { dst } => vinsns.push(VInsn { op: Op::ListNil as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                IrOp::ListCons { dst, head, tail } => vinsns.push(VInsn { op: Op::ListCons as u8, a: reg(*dst), b: reg(*head), c: reg(*tail), imm: 0 }),
                IrOp::ListHead { dst, list } => vinsns.push(VInsn { op: Op::ListHead as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 }),
                IrOp::ListTail { dst, list } => vinsns.push(VInsn { op: Op::ListTail as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 }),
                IrOp::ListIsNil { dst, list } => vinsns.push(VInsn { op: Op::ListIsNil as u8, a: reg(*dst), b: reg(*list), c: 0, imm: 0 }),
                IrOp::ArrayNew { dst, len } => vinsns.push(VInsn { op: Op::ArrayNew as u8, a: reg(*dst), b: reg(*len), c: 0, imm: 0 }),
                IrOp::ArrayLen { dst, arr } => vinsns.push(VInsn { op: Op::ArrayLen as u8, a: reg(*dst), b: reg(*arr), c: 0, imm: 0 }),
                IrOp::ArrayGet { dst, arr, index } => vinsns.push(VInsn { op: Op::ArrayGet as u8, a: reg(*dst), b: reg(*arr), c: reg(*index), imm: 0 }),
                IrOp::ArraySet { arr, index, value } => vinsns.push(VInsn { op: Op::ArraySet as u8, a: reg(*value), b: reg(*arr), c: reg(*index), imm: 0 }),
                IrOp::ObjNew { dst } => vinsns.push(VInsn { op: Op::ObjNew as u8, a: reg(*dst), b: 0, c: 0, imm: 0 }),
                IrOp::ObjHasAtom { dst, obj, atom_id } => vinsns.push(VInsn { op: Op::ObjHasAtom as u8, a: reg(*dst), b: reg(*obj), c: 0, imm: *atom_id }),
                IrOp::ObjGetAtom { dst, obj, atom_id } => vinsns.push(VInsn { op: Op::ObjGetAtom as u8, a: reg(*dst), b: reg(*obj), c: 0, imm: *atom_id }),
                IrOp::ObjSetAtom { obj, atom_id, value } => vinsns.push(VInsn { op: Op::ObjSetAtom as u8, a: reg(*value), b: reg(*obj), c: 0, imm: *atom_id }),
                IrOp::ObjGet { dst, obj, atom } => vinsns.push(VInsn { op: Op::ObjGet as u8, a: reg(*dst), b: reg(*obj), c: reg(*atom), imm: 0 }),
                IrOp::ObjSet { obj, atom, value } => vinsns.push(VInsn { op: Op::ObjSet as u8, a: reg(*value), b: reg(*obj), c: reg(*atom), imm: 0 }),
                IrOp::ToDyn { dst, src } => vinsns.push(VInsn { op: Op::ToDyn as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynI8 { dst, src } => vinsns.push(VInsn { op: Op::FromDynI8 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynI16 { dst, src } => vinsns.push(VInsn { op: Op::FromDynI16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynI32 { dst, src } => vinsns.push(VInsn { op: Op::FromDynI32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynI64 { dst, src } => vinsns.push(VInsn { op: Op::FromDynI64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynF16 { dst, src } => vinsns.push(VInsn { op: Op::FromDynF16 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynF32 { dst, src } => vinsns.push(VInsn { op: Op::FromDynF32 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynF64 { dst, src } => vinsns.push(VInsn { op: Op::FromDynF64 as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynBool { dst, src } => vinsns.push(VInsn { op: Op::FromDynBool as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::FromDynPtr { dst, src } => vinsns.push(VInsn { op: Op::FromDynPtr as u8, a: reg(*dst), b: reg(*src), c: 0, imm: 0 }),
                IrOp::Throw { payload } => vinsns.push(VInsn { op: Op::Throw as u8, a: reg(*payload), b: 0, c: 0, imm: 0 }),
                IrOp::Try { catch_dst, catch_block, trap_only } => {
                    let catch_pc = blk_pc(&block_start, *catch_block)?;
                    vinsns.push(VInsn { op: Op::Try as u8, a: reg(*catch_dst), b: if *trap_only { 1 } else { 0 }, c: 0, imm: delta(this_pc, catch_pc) });
                }
                IrOp::EndTry => vinsns.push(VInsn { op: Op::EndTry as u8, a: 0, b: 0, c: 0, imm: 0 }),
                IrOp::Call { dst, callee, sig_id, arg_base, nargs } => {
                    if let Some(fi) = const_fun_of_vreg.get(callee.0 as usize).copied().flatten() {
                        is_direct_call = true;
                        vinsns.push(VInsn { op: Op::Call as u8, a: reg(*dst), b: reg(*arg_base), c: *nargs as u32, imm: fi });
                    } else {
                        vinsns.push(VInsn { op: Op::CallR as u8, a: reg(*dst), b: reg(*callee), c: *nargs as u32, imm: arg_base.0 });
                    }
                    call_windows.push((*sig_id, arg_base.0, *nargs));
                }
                IrOp::Closure { dst, func_index, cap_sig_id, cap_base, ncaps } => {
                    vinsns.push(VInsn { op: Op::Closure as u8, a: reg(*dst), b: reg(*cap_base), c: *ncaps as u32, imm: map_func_index(*func_index) });
                    call_windows.push((*cap_sig_id, cap_base.0, *ncaps));
                }
                IrOp::BindThis { dst, func, this } => vinsns.push(VInsn { op: Op::BindThis as u8, a: reg(*dst), b: reg(*func), c: reg(*this), imm: 0 }),
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Codegen,
                        ins.span,
                        "IR→bytecode bridge: unsupported instruction op",
                    ))
                }
            }

            // InstrInfo for regalloc:
            // derive from the authoritative `IrOp::uses/def`, with one special-case:
            // when we emit a direct `CALL`, the callee vreg is not read at runtime.
            if matches!(ins.op, IrOp::Phi { .. }) {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    ins.span,
                    "Phi survived into emission (run phi elimination first)",
                ));
            }
            let mut uses: Vec<VReg> = ins.op.uses().into_iter().map(|v| VReg(v.0)).collect();
            if is_direct_call {
                if let IrOp::Call { callee, .. } = &ins.op {
                    uses.retain(|u| u.0 != callee.0);
                }
            }
            let defs: Vec<VReg> = ins.op.def().into_iter().map(|d| VReg(d.0)).collect();
            infos.push(InstrInfo { uses, defs });

            // Update local constant-function tracking after this instruction.
            //
            // NOTE: use the op's explicit def (not the regalloc InstrInfo) so we never
            // accidentally keep a stale `ConstFun` fact for values like `Closure(...)`.
            if let Some(d) = ins.op.def() {
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
                vinsns.push(VInsn { op: Op::SwitchKind as u8, a: reg(*kind), b: ncases as u32, c: 0, imm: def_delta });
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

    // LSRA uses a linear instruction order. To remain correct in the presence of loops/backedges,
    // extend live ranges to block ends using CFG liveness (so loop-carried values aren't expired
    // before the backedge).
    //
    // We do this by computing block live-out sets and adding them as "uses" on the block's
    // terminator instruction info.
    {
        let nb = f.blocks.len();
        let mut use_set: Vec<HashSet<u32>> = vec![HashSet::new(); nb];
        let mut def_set: Vec<HashSet<u32>> = vec![HashSet::new(); nb];
        let mut succs: Vec<Vec<usize>> = vec![Vec::new(); nb];

        for (bi, blk) in f.blocks.iter().enumerate() {
            let start = block_start[bi] as usize;
            let end = start + blk.insns.len() + (term_size(&blk.term) as usize);
            let mut defs: HashSet<u32> = HashSet::new();
            let mut uses: HashSet<u32> = HashSet::new();
            for ii in start..end {
                if ii >= infos.len() {
                    break;
                }
                for &u in &infos[ii].uses {
                    if !defs.contains(&u.0) {
                        uses.insert(u.0);
                    }
                }
                for &d in &infos[ii].defs {
                    defs.insert(d.0);
                }
            }
            use_set[bi] = uses;
            def_set[bi] = defs;
            succs[bi] = super::block_successors(&blk.term)
                .into_iter()
                .map(|b| b.0 as usize)
                .filter(|&s| s < nb)
                .collect();
        }

        let mut live_in: Vec<HashSet<u32>> = vec![HashSet::new(); nb];
        let mut live_out: Vec<HashSet<u32>> = vec![HashSet::new(); nb];

        let mut changed = true;
        while changed {
            changed = false;
            for bi in (0..nb).rev() {
                let mut out: HashSet<u32> = HashSet::new();
                for &s in &succs[bi] {
                    out.extend(live_in[s].iter().copied());
                }

                let mut inb: HashSet<u32> = use_set[bi].clone();
                for v in out.iter().copied() {
                    if !def_set[bi].contains(&v) {
                        inb.insert(v);
                    }
                }

                if inb != live_in[bi] || out != live_out[bi] {
                    live_in[bi] = inb;
                    live_out[bi] = out;
                    changed = true;
                }
            }
        }

        for (bi, blk) in f.blocks.iter().enumerate() {
            // Forward edges are naturally handled by linear order (uses in successors appear later).
            // The only missing case for linear-scan is **backedges**, where successor uses occur at
            // an earlier PC than the edge source. Extend those values to the edge source's end.
            let from_pc = block_start[bi];
            let mut need: HashSet<u32> = HashSet::new();
            for &s in &succs[bi] {
                if block_start[s] < from_pc {
                    need.extend(live_in[s].iter().copied());
                }
            }
            if need.is_empty() {
                continue;
            }

            let start = block_start[bi] as usize;
            let term_begin = start + blk.insns.len();
            let term_info_idx = term_begin;
            if term_info_idx >= infos.len() {
                continue;
            }
            let info = &mut infos[term_info_idx];
            for v in need.into_iter() {
                if !info.uses.iter().any(|u| u.0 == v) {
                    info.uses.push(VReg(v));
                }
            }
        }
    }

    Ok(VirtualStream { vinsns, infos, call_windows })
}

