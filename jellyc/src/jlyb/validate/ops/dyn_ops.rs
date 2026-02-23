use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::ToDyn as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("to_dyn reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Dynamic {
                return Err(ctx.err("to_dyn dst must be Dynamic"));
            }
            Ok(true)
        }
        x if x == Op::FromDynI8 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::I8 {
                return Err(ctx.err("from_dyn_i8 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynI16 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::I16 {
                return Err(ctx.err("from_dyn_i16 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynI32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::I32 {
                return Err(ctx.err("from_dyn_i32 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynI64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::I64 {
                return Err(ctx.err("from_dyn_i64 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynF16 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::F16 {
                return Err(ctx.err("from_dyn_f16 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynF32 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::F32 {
                return Err(ctx.err("from_dyn_f32 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynF64 as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::F64 {
                return Err(ctx.err("from_dyn_f64 types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynBool as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::Bool {
                return Err(ctx.err("from_dyn_bool types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic || ctx.rk(ins.a)? != TypeKind::Atom {
                return Err(ctx.err("from_dyn_atom types required"));
            }
            Ok(true)
        }
        x if x == Op::FromDynPtr as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("dyn conv reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Dynamic {
                return Err(ctx.err("from_dyn_ptr src must be Dynamic"));
            }
            let dk = ctx.rk(ins.a)?;
            if !ctx.is_ptr_kind(dk) {
                return Err(ctx.err("from_dyn_ptr dst must be pointer-kind"));
            }
            Ok(true)
        }
        x if x == Op::SpillPush as u8 || x == Op::SpillPop as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("spill reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Dynamic {
                return Err(ctx.err("spill ops require Dynamic reg"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
