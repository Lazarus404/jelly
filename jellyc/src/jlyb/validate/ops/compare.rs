use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::Physeq as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("physeq reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("physeq dst must be bool"));
            }
            Ok(true)
        }
        x if x == Op::EqI32 as u8 || x == Op::LtI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("eq/lt reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("eq/lt dst must be bool"));
            }
            if !ctx.is_i32ish(ctx.rk(ins.b)?) || !ctx.is_i32ish(ctx.rk(ins.c)?) {
                return Err(ctx.err("eq/lt_i32 operands must be i8/i16/i32"));
            }
            Ok(true)
        }
        x if x == Op::EqI32Imm as u8 || x == Op::LtI32Imm as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("eq/lt_imm reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("eq/lt_imm dst must be bool"));
            }
            if !ctx.is_i32ish(ctx.rk(ins.b)?) {
                return Err(ctx.err("eq/lt_imm src must be i8/i16/i32"));
            }
            Ok(true)
        }
        x if x == Op::EqI64 as u8 || x == Op::LtI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("eq/lt reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool
                || ctx.rk(ins.b)? != TypeKind::I64
                || ctx.rk(ins.c)? != TypeKind::I64
            {
                return Err(ctx.err("eq/lt_i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::EqF32 as u8 || x == Op::LtF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("eq/lt reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool
                || ctx.rk(ins.b)? != TypeKind::F32
                || ctx.rk(ins.c)? != TypeKind::F32
            {
                return Err(ctx.err("eq/lt_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::EqF64 as u8 || x == Op::LtF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("eq/lt reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool
                || ctx.rk(ins.b)? != TypeKind::F64
                || ctx.rk(ins.c)? != TypeKind::F64
            {
                return Err(ctx.err("eq/lt_f64 types required"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
