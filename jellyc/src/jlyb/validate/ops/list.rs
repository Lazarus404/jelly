use crate::jlyb::{Insn, Op, TypeKind};

use super::super::ctx::ValidateCtx;

pub(super) fn validate(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<bool, String> {
    let op = ins.op;
    match op {
        x if x == Op::ListNil as u8 => {
            if (ins.a as u32) >= ctx.nregs {
                return Err(ctx.err("list nil reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::List {
                return Err(ctx.err("list_nil dst must be list"));
            }
            Ok(true)
        }
        x if x == Op::ListCons as u8 => {
            if (ins.a as u32) >= ctx.nregs
                || (ins.b as u32) >= ctx.nregs
                || (ins.c as u32) >= ctx.nregs
            {
                return Err(ctx.err("list cons reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::List {
                return Err(ctx.err("list_cons dst must be list"));
            }
            if ctx.rk(ins.c)? != TypeKind::List {
                return Err(ctx.err("list_cons tail must be list"));
            }
            if ctx.reg_types[ins.a as usize] != ctx.reg_types[ins.c as usize] {
                return Err(ctx.err("list_cons list type mismatch"));
            }
            let list_tid = ctx.reg_types[ins.a as usize];
            let elem_tid = ctx.unary_elem_tid(list_tid, TypeKind::List, "list_cons")?;
            if elem_tid != ctx.reg_types[ins.b as usize] {
                return Err(ctx.err("list_cons head type mismatch"));
            }
            Ok(true)
        }
        x if x == Op::ListHead as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("list head reg out of range"));
            }
            if ctx.rk(ins.b)? != TypeKind::List {
                return Err(ctx.err("list_head src must be list"));
            }
            let list_tid = ctx.reg_types[ins.b as usize];
            let elem_tid = ctx.unary_elem_tid(list_tid, TypeKind::List, "list_head")?;
            if elem_tid != ctx.reg_types[ins.a as usize] {
                return Err(ctx.err("list_head dst type mismatch"));
            }
            Ok(true)
        }
        x if x == Op::ListTail as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("list tail reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::List || ctx.rk(ins.b)? != TypeKind::List {
                return Err(ctx.err("list_tail types required"));
            }
            if ctx.reg_types[ins.a as usize] != ctx.reg_types[ins.b as usize] {
                return Err(ctx.err("list_tail list type mismatch"));
            }
            Ok(true)
        }
        x if x == Op::ListIsNil as u8 => {
            if (ins.a as u32) >= ctx.nregs || (ins.b as u32) >= ctx.nregs {
                return Err(ctx.err("list is_nil reg out of range"));
            }
            if ctx.rk(ins.a)? != TypeKind::Bool || ctx.rk(ins.b)? != TypeKind::List {
                return Err(ctx.err("list_is_nil types required"));
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
