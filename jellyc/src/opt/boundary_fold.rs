// Local Dynamic-boundary folding.
//
// `boundary_simplify` is a CFG dataflow pass that can eliminate `ToDyn`/`FromDyn*` pairs when
// provenance is stable across blocks. Post-phi, however, the IR is no longer strictly SSA-like and
// vregs may be multi-defined; the global pass intentionally becomes conservative in that case.
//
// This pass is a **block-local** peephole that remains safe even with multi-def typed origins by
// tracking a per-vreg "def epoch" and only folding an unbox when the typed origin has not been
// redefined since it was boxed.

use crate::ir::{IrModule, IrOp, VRegId};
use crate::typectx::T_DYNAMIC;
use std::collections::HashMap;

/// Fold redundant `ToDyn`/`FromDyn*` within basic blocks.
/// Returns true if any change was made.
pub fn fold_local_dynamic_boundaries(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let mut def_epoch: Vec<u32> = vec![0; func.vreg_types.len()];

        for block in &mut func.blocks {
            // dyn vreg -> (typed origin vreg, epoch_at_box)
            let mut dyn_origin: HashMap<VRegId, (VRegId, u32)> = HashMap::new();

            for ins in &mut block.insns {
                // Note: treat "def happens after uses" within an instruction by applying any
                // epoch bump at the end of this loop iteration.
                match ins.op {
                    IrOp::ToDyn { dst, src } => {
                        let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                        let src_tid = func.vreg_types.get(src.0 as usize).copied().unwrap_or(T_DYNAMIC);

                        // `ToDyn` from an already-Dynamic src is just a copy.
                        if dst_tid == T_DYNAMIC && src_tid == T_DYNAMIC {
                            ins.op = IrOp::Mov { dst, src };
                            changed = true;
                            if let Some(&(o, e)) = dyn_origin.get(&src) {
                                dyn_origin.insert(dst, (o, e));
                            } else {
                                dyn_origin.remove(&dst);
                            }
                        } else if dst_tid == T_DYNAMIC && src_tid != T_DYNAMIC {
                            let epoch = def_epoch.get(src.0 as usize).copied().unwrap_or(0);
                            dyn_origin.insert(dst, (src, epoch));
                        } else {
                            dyn_origin.remove(&dst);
                        }
                    }
                    IrOp::Mov { dst, src } => {
                        let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                        let src_tid = func.vreg_types.get(src.0 as usize).copied().unwrap_or(T_DYNAMIC);
                        if dst_tid == T_DYNAMIC && src_tid == T_DYNAMIC {
                            if let Some(&(o, e)) = dyn_origin.get(&src) {
                                dyn_origin.insert(dst, (o, e));
                            } else {
                                dyn_origin.remove(&dst);
                            }
                        } else {
                            dyn_origin.remove(&dst);
                        }
                    }
                    IrOp::FromDynI8 { dst, src }
                    | IrOp::FromDynI16 { dst, src }
                    | IrOp::FromDynI32 { dst, src }
                    | IrOp::FromDynI64 { dst, src }
                    | IrOp::FromDynF16 { dst, src }
                    | IrOp::FromDynF32 { dst, src }
                    | IrOp::FromDynF64 { dst, src }
                    | IrOp::FromDynBool { dst, src }
                    | IrOp::FromDynPtr { dst, src } => {
                        let Some(&(origin, epoch_at_box)) = dyn_origin.get(&src) else {
                            // no local provenance
                            // (global boundary_simplify might still handle it)
                            // continue to epoch bump below
                            if let Some(d) = ins.op.def() {
                                dyn_origin.remove(&d);
                            }
                            if let Some(d) = ins.op.def() {
                                if (d.0 as usize) < def_epoch.len() {
                                    def_epoch[d.0 as usize] = def_epoch[d.0 as usize].saturating_add(1);
                                }
                            }
                            continue;
                        };

                        let dst_tid = func.vreg_types.get(dst.0 as usize).copied().unwrap_or(T_DYNAMIC);
                        let origin_tid = func
                            .vreg_types
                            .get(origin.0 as usize)
                            .copied()
                            .unwrap_or(T_DYNAMIC);

                        // Only eliminate when the unbox is a no-op w.r.t static types, and the
                        // origin has not been redefined since we boxed it.
                        let cur_epoch = def_epoch.get(origin.0 as usize).copied().unwrap_or(0);
                        if dst_tid == origin_tid && dst_tid != T_DYNAMIC && cur_epoch == epoch_at_box {
                            ins.op = IrOp::Mov { dst, src: origin };
                            changed = true;
                        }
                    }
                    _ => {
                        // Any other def invalidates local provenance for that vreg.
                        if let Some(d) = ins.op.def() {
                            dyn_origin.remove(&d);
                        }
                    }
                }

                // Bump epoch for defs after processing.
                if let Some(d) = ins.op.def() {
                    let di = d.0 as usize;
                    if di < def_epoch.len() {
                        def_epoch[di] = def_epoch[di].saturating_add(1);
                    }
                }
            }
        }
    }

    changed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::ir::{IrBuilder, IrModule, IrTerminator};
    use crate::typectx::{T_DYNAMIC, T_I32};

    #[test]
    fn folds_from_dyn_when_origin_not_redefined() {
        // v0 = 1
        // v1 = to_dyn v0
        // v2 = from_dyn_i32 v1   ; fold to mov v2, v0
        // v0 = 2                 ; redefinition after fold point is fine
        let mut b = IrBuilder::new(Some("t".to_string()));
        let v0 = b.new_vreg(T_I32);
        let v1 = b.new_vreg(T_DYNAMIC);
        let v2 = b.new_vreg(T_I32);

        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 1 });
        b.emit(Span::point(0), IrOp::ToDyn { dst: v1, src: v0 });
        b.emit(Span::point(0), IrOp::FromDynI32 { dst: v2, src: v1 });
        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 2 });
        b.term(IrTerminator::Ret { value: v2 });

        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![b.func],
            entry: 0,
        };

        let changed = fold_local_dynamic_boundaries(&mut m);
        assert!(changed);
        let ins = &m.funcs[0].blocks[0].insns[2].op;
        assert!(matches!(ins, IrOp::Mov { dst, src } if *dst == v2 && *src == v0));
    }

    #[test]
    fn does_not_fold_if_origin_redefined_between_box_and_unbox() {
        // v0 = 1
        // v1 = to_dyn v0
        // v0 = 2                 ; redefinition breaks mov-substitution
        // v2 = from_dyn_i32 v1   ; must NOT fold
        let mut b = IrBuilder::new(Some("t".to_string()));
        let v0 = b.new_vreg(T_I32);
        let v1 = b.new_vreg(T_DYNAMIC);
        let v2 = b.new_vreg(T_I32);

        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 1 });
        b.emit(Span::point(0), IrOp::ToDyn { dst: v1, src: v0 });
        b.emit(Span::point(0), IrOp::ConstI32 { dst: v0, imm: 2 });
        b.emit(Span::point(0), IrOp::FromDynI32 { dst: v2, src: v1 });
        b.term(IrTerminator::Ret { value: v2 });

        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![b.func],
            entry: 0,
        };

        let _changed = fold_local_dynamic_boundaries(&mut m);
        let ins = &m.funcs[0].blocks[0].insns[3].op;
        assert!(matches!(ins, IrOp::FromDynI32 { .. }), "expected FromDynI32 to remain");
    }
}

