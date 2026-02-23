use crate::ast::Span;
use crate::ir::{IrBuilder, IrOp, TypeId, VRegId};

/// Marshal args into a contiguous window. When `tail_call` is true and args are params
/// in order, returns (VRegId(0), nargs) to use the param range directly (avoids Mov +
/// arg-block pinning that can corrupt closure tail calls).
pub(super) fn marshal_args_window(
    span: Span,
    arg_tids: &[TypeId],
    arg_vals: &[VRegId],
    b: &mut IrBuilder,
    tail_call: bool,
) -> (VRegId, u8) {
    debug_assert_eq!(
        arg_tids.len(),
        arg_vals.len(),
        "argument types/values must match"
    );

    let nargs = arg_vals.len() as u8;
    if nargs == 0 {
        return (VRegId(0), 0);
    }

    if tail_call
        && arg_vals
            .iter()
            .enumerate()
            .all(|(i, &v)| v.0 == i as u32)
    {
        return (VRegId(0), nargs);
    }

    let base = b.new_vreg(arg_tids[0]);
    b.emit(
        span,
        IrOp::Mov {
            dst: base,
            src: arg_vals[0],
        },
    );

    let mut prev = base;
    for i in 1..(nargs as usize) {
        let v = b.new_vreg(arg_tids[i]);
        debug_assert_eq!(v.0, prev.0 + 1, "arg window must be contiguous vregs");
        b.emit(
            span,
            IrOp::Mov {
                dst: v,
                src: arg_vals[i],
            },
        );
        prev = v;
    }

    (base, nargs)
}
