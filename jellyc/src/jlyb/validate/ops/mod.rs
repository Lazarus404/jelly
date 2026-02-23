mod arith;
mod array;
mod bytes;
mod calls;
mod compare;
mod consts;
mod control;
mod conv;
mod dyn_ops;
mod list;
mod object;

use crate::jlyb::Insn;

use super::ctx::ValidateCtx;

pub(super) fn validate_insn(ctx: &ValidateCtx<'_>, ins: &Insn) -> Result<(), String> {
    if control::validate(ctx, ins)? {
        return Ok(());
    }
    if calls::validate(ctx, ins)? {
        return Ok(());
    }
    if consts::validate(ctx, ins)? {
        return Ok(());
    }
    if dyn_ops::validate(ctx, ins)? {
        return Ok(());
    }
    if arith::validate(ctx, ins)? {
        return Ok(());
    }
    if compare::validate(ctx, ins)? {
        return Ok(());
    }
    if conv::validate(ctx, ins)? {
        return Ok(());
    }
    if list::validate(ctx, ins)? {
        return Ok(());
    }
    if array::validate(ctx, ins)? {
        return Ok(());
    }
    if bytes::validate(ctx, ins)? {
        return Ok(());
    }
    if object::validate(ctx, ins)? {
        return Ok(());
    }

    // If an opcode isn't explicitly validated above, fail fast.
    //
    // This keeps the validator in sync with the VM's rules as opcodes are added/changed,
    // and prevents accidental "accept by default" behavior.
    if ins.op > 117 {
        return Err(ctx.err(format!("unknown opcode: {}", ins.op)));
    }
    Err(ctx.err(format!("opcode not yet validated: {}", ins.op)))
}
