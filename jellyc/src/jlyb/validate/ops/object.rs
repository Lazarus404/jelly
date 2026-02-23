use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::ObjNew as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("obj new reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Object {
                return Err(ctx.err("obj_new dst must be object"));
            }
            Ok(true)
        }
        x if x == Op::ObjHasAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("obj has reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool || ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_has types required"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("obj_has atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ObjGetAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("obj get reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_get src must be object"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("obj_get atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ObjSetAtom as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("obj set reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_set dst must be object"));
            }
            if (ins.imm as usize) >= ctx.m.atoms.len() {
                return Err(ctx.err("obj_set atom id out of range"));
            }
            Ok(true)
        }
        x if x == Op::ObjGet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("obj get reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_get src must be object"));
            }
            if ctx.rk(ins.c)? != TypeKind::Atom {
                return Err(ctx.err("obj_get key must be atom"));
            }
            Ok(true)
        }
        x if x == Op::ObjSet as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("obj set reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::Object {
                return Err(ctx.err("obj_set dst must be object"));
            }
            if ctx.rk(ins.c)? != TypeKind::Atom {
                return Err(ctx.err("obj_set key must be atom"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
