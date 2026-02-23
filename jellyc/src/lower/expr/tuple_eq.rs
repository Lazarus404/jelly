use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use super::super::{intern_atom, LowerCtx, T_BOOL, T_BYTES, T_I32};

pub(super) fn emit_bytes_eq(
    span: Span,
    dst: VRegId,
    a: VRegId,
    b2: VRegId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(), CompileError> {
    let sig_args = [T_BYTES, T_BYTES];
    let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);
    let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args);

    let vcallee = b.new_vreg(fun_tid);
    b.emit(
        span,
        IrOp::ConstFun {
            dst: vcallee,
            func_index: crate::jlyb::PRELUDE_BYTES_EQ,
        },
    );

    // Marshal args into a contiguous vreg window.
    let arg0 = b.new_vreg(T_BYTES);
    b.emit(span, IrOp::Mov { dst: arg0, src: a });
    let arg1 = b.new_vreg(T_BYTES);
    b.emit(span, IrOp::Mov { dst: arg1, src: b2 });

    b.emit(
        span,
        IrOp::Call {
            dst,
            callee: vcallee,
            sig_id,
            arg_base: arg0,
            nargs: 2,
        },
    );
    Ok(())
}

pub(super) fn lower_tuple_eq(
    span: Span,
    a: VRegId,
    b2: VRegId,
    tup_tid: TypeId,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<VRegId, CompileError> {
    super::super::ensure_open_block(b);

    let elems = ctx
        .type_ctx
        .tuple_elems(tup_tid)
        .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad tuple type"))?
        .to_vec();

    let out = b.new_vreg(T_BOOL);
    let fail_b = b.new_block(Some("tup_eq_fail".to_string()));
    let ok_b = b.new_block(Some("tup_eq_ok".to_string()));
    let join_b = b.new_block(Some("tup_eq_join".to_string()));

    if elems.is_empty() {
        // () == () is true.
        b.emit(
            span,
            IrOp::ConstBool {
                dst: out,
                imm: true,
            },
        );
        return Ok(out);
    } else {
        // Compare each element with early-exit on mismatch.
        for (i, &et) in elems.iter().enumerate() {
            let atom_id = intern_atom(&i.to_string(), ctx);
            let va = b.new_vreg(et);
            let vb = b.new_vreg(et);
            b.emit(
                span,
                IrOp::ObjGetAtom {
                    dst: va,
                    obj: a,
                    atom_id,
                },
            );
            b.emit(
                span,
                IrOp::ObjGetAtom {
                    dst: vb,
                    obj: b2,
                    atom_id,
                },
            );

            let v_eq = b.new_vreg(T_BOOL);
            match et {
                T_I32 => b.emit(
                    span,
                    IrOp::EqI32 {
                        dst: v_eq,
                        a: va,
                        b: vb,
                    },
                ),
                T_BOOL => b.emit(
                    span,
                    IrOp::Physeq {
                        dst: v_eq,
                        a: va,
                        b: vb,
                    },
                ),
                T_BYTES => emit_bytes_eq(span, v_eq, va, vb, ctx, b)?,
                _ if ctx.type_ctx.is_tuple_type(et) => {
                    // Nested tuples.
                    let nested = lower_tuple_eq(span, va, vb, et, ctx, b)?;
                    b.emit(
                        span,
                        IrOp::Mov {
                            dst: v_eq,
                            src: nested,
                        },
                    );
                }
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        span,
                        "tuple element equality not supported for this type yet",
                    ))
                }
            }

            let then_tgt = if i + 1 == elems.len() {
                ok_b
            } else {
                b.new_block(Some(format!("tup_eq_next{}", i)))
            };
            b.term(IrTerminator::JmpIf {
                cond: v_eq,
                then_tgt,
                else_tgt: fail_b,
            });
            b.set_block(then_tgt);
        }
    }

    // ok: true
    b.set_block(ok_b);
    b.emit(
        span,
        IrOp::ConstBool {
            dst: out,
            imm: true,
        },
    );
    b.term(IrTerminator::Jmp { target: join_b });

    // fail: false
    b.set_block(fail_b);
    b.emit(
        span,
        IrOp::ConstBool {
            dst: out,
            imm: false,
        },
    );
    b.term(IrTerminator::Jmp { target: join_b });

    // join: value in `out`
    b.set_block(join_b);
    Ok(out)
}
