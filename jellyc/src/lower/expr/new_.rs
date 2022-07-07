/**
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

// `new` expression lowering (object construction with optional init call).

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrBuilder, IrOp, IrTerminator, TypeId, VRegId};

use super::{is_object_kind, lower_expr, LowerCtx};
use super::{T_BOOL, T_OBJECT};

pub fn lower_new_expr(
    e: &crate::ast::Expr,
    proto: &crate::ast::Expr,
    args: &[crate::ast::Expr],
    expect: Option<TypeId>,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
) -> Result<(VRegId, TypeId), CompileError> {
    let (v_proto, t_proto) = lower_expr(proto, ctx, b)?;
    if !is_object_kind(&ctx.type_ctx, t_proto) {
        return Err(CompileError::new(
            ErrorKind::Type,
            proto.span,
            "new expects an Object prototype",
        ));
    }

    // Type of the allocated instance:
    // - default: use the prototype's (possibly nominal) type
    // - allow erasure to plain Object if context expects Object
    // - otherwise, require the expected nominal type to match the prototype's nominal type
    let self_tid = match expect {
        Some(et) if et == T_OBJECT => T_OBJECT,
        Some(et) if is_object_kind(&ctx.type_ctx, et) => {
            if et != t_proto {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    e.span,
                    "new: expected object type does not match prototype type",
                ));
            }
            et
        }
        _ => t_proto,
    };
    let v_self = b.new_vreg(self_tid);
    b.emit(e.span, IrOp::ObjNew { dst: v_self });
    b.emit(
        e.span,
        IrOp::ObjSetAtom {
            obj: v_self,
            atom_id: crate::jlyb::ATOM___PROTO__,
            value: v_proto,
        },
    );

    // If proto has init, call it.
    let v_has = b.new_vreg(T_BOOL);
    b.emit(
        e.span,
        IrOp::ObjHasAtom {
            dst: v_has,
            obj: v_proto,
            atom_id: crate::jlyb::ATOM_INIT,
        },
    );

    let call_b = b.new_block(Some("new_init".to_string()));
    let join_b = b.new_block(Some("new_join".to_string()));
    b.term(IrTerminator::JmpIf {
        cond: v_has,
        then_tgt: call_b,
        else_tgt: join_b,
    });

    // call_b
    b.set_block(call_b);

    // Type the init call by the actual argument types.
    let mut lowered_args: Vec<(VRegId, TypeId, Span)> = Vec::with_capacity(args.len());
    for a in args {
        let (v, t) = lower_expr(a, ctx, b)?;
        lowered_args.push((v, t, a.span));
    }

    let sig_args: Vec<TypeId> = std::iter::once(T_OBJECT)
        .chain(lowered_args.iter().map(|(_v, t, _sp)| *t))
        .collect();
    let sig_id = ctx.type_ctx.intern_sig(T_OBJECT, &sig_args);
    let fun_tid = ctx.type_ctx.intern_fun_type(T_OBJECT, &sig_args);

    let v_init = b.new_vreg(fun_tid);
    b.emit(
        e.span,
        IrOp::ObjGetAtom {
            dst: v_init,
            obj: v_proto,
            atom_id: crate::jlyb::ATOM_INIT,
        },
    );

    // Marshal args into a contiguous vreg window.
    let arg_base = b.new_vreg(T_OBJECT);
    b.emit(e.span, IrOp::Mov { dst: arg_base, src: v_self });
    for (v, t, sp) in lowered_args {
        let slot = b.new_vreg(t);
        b.emit(sp, IrOp::Mov { dst: slot, src: v });
    }

    let v_tmp_ret = b.new_vreg(T_OBJECT);
    b.emit(
        e.span,
        IrOp::Call {
            dst: v_tmp_ret,
            callee: v_init,
            sig_id,
            arg_base,
            nargs: (1 + sig_args.len() - 1) as u8,
        },
    );
    b.term(IrTerminator::Jmp { target: join_b });

    b.set_block(join_b);
    Ok((v_self, self_tid))
}
