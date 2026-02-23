use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::ArrayNew as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("array new reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Array || ctx.rk(ins.b)? != TypeKind::I32 {
                return Err(ctx.err("array_new types required"));
            }
            Ok(true)
        }
        x if x == Op::ArrayLen as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("array len reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::I32 || ctx.rk(ins.b)? != TypeKind::Array {
                return Err(ctx.err("array_len types required"));
            }
            Ok(true)
        }
        x if x == Op::ArrayGet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("array get reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Array || ctx.rk(ins.c)? != TypeKind::I32 {
                return Err(ctx.err("array_get types required"));
            }
            let arr_tid = ctx.reg_types[ins.b as usize];
            let elem_tid = ctx
                .m
                .types
                .get(arr_tid as usize)
                .map(|te| te.p0)
                .ok_or_else(|| ctx.err("array_get array type entry out of range"))?;
            if elem_tid != ctx.reg_types[ins.a as usize] {
                return Err(ctx.err("array_get dst type mismatch"));
            }
            Ok(true)
        }
        x if x == Op::ArraySet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("array set reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Array || ctx.rk(ins.c)? != TypeKind::I32 {
                return Err(ctx.err("array_set types required"));
            }
            let arr_tid = ctx.reg_types[ins.b as usize];
            let elem_tid = ctx
                .m
                .types
                .get(arr_tid as usize)
                .map(|te| te.p0)
                .ok_or_else(|| ctx.err("array_set array type entry out of range"))?;
            if elem_tid != ctx.reg_types[ins.a as usize] {
                return Err(ctx.err("array_set src type mismatch"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
