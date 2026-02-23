use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::Nop as u8 => Ok(true),
        x if x == Op::Ret as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("ret reg out of range"));
            }
            Ok(true)
        }
        x if x == Op::Mov as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("mov reg out of range"));
            }
            // Match VM validation: allow mov between different type IDs if either:
            // - kinds match (nominal subtypes), OR
            // - slot sizes match (raw copy).
            if ctx.reg_types[ins.a as usize] != ctx.reg_types[ins.b as usize] {
                let ka = ctx.rk(ins.a)?;
                let kb = ctx.rk(ins.b)?;
                let sa = ctx.slot_size_bytes(ka);
                let sb = ctx.slot_size_bytes(kb);
                if ka == kb {
                    // same kind, different type IDs - ok
                } else if sa == sb && sa > 0 {
                    // same slot size - ok
                } else {
                    return Err(ctx.err("mov type mismatch"));
                }
            }
            Ok(true)
        }
        x if x == Op::Jmp as u8 => {
            let d = ins.imm as i32;
            let tgt = (ctx.pc as i32 + 1) + d;
            if tgt < 0 || tgt > (ctx.m.funcs[ctx.func_i].insns.len() as i32) {
                return Err(ctx.err("jmp target out of range"));
            }
            Ok(true)
        }
        x if x == Op::JmpIf as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("jmp_if cond reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("jmp_if cond must be bool"));
            }
            let d = ins.imm as i32;
            let tgt = (ctx.pc as i32 + 1) + d;
            if tgt < 0 || tgt > (ctx.m.funcs[ctx.func_i].insns.len() as i32) {
                return Err(ctx.err("jmp_if target out of range"));
            }
            Ok(true)
        }
        x if x == Op::Assert as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("assert cond reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("assert cond must be bool"));
            }
            Ok(true)
        }
        x if x == Op::Try as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("try reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Dynamic {
                return Err(ctx.err("try dst must be Dynamic"));
            }
            if ins.b > 1 {
                return Err(ctx.err("try b must be 0/1 (trap_only flag)"));
            }
            let d = ins.imm as i32;
            let tgt = (ctx.pc as i32 + 1) + d;
            if tgt < 0 || tgt > (ctx.m.funcs[ctx.func_i].insns.len() as i32) {
                return Err(ctx.err("try catch target out of range"));
            }
            Ok(true)
        }
        x if x == Op::EndTry as u8 => Ok(true),
        x if x == Op::Throw as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("throw reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Dynamic {
                return Err(ctx.err("throw payload must be Dynamic"));
            }
            Ok(true)
        }
        x if x == Op::SwitchKind as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("switch_kind reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 {
                return Err(ctx.err("switch_kind src must be i32"));
            }
            let ncases = ins.b as usize;
            if ctx.pc + 1 + ncases > ctx.m.funcs[ctx.func_i].insns.len() {
                return Err(ctx.err("switch_kind case table out of range"));
            }
            Ok(true)
        }
        x if x == Op::CaseKind as u8 => Ok(true),
        _ => Ok(false),
    }
}
