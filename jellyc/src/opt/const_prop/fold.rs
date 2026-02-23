use crate::ir::{IrOp, TypeId, VRegId};
use crate::typectx::{T_I32, T_I8};

pub(super) fn fold_int_result(dst: VRegId, imm: i32, vreg_types: &[TypeId]) -> IrOp {
    let dst_tid = vreg_types.get(dst.0 as usize).copied().unwrap_or(T_I32);
    if dst_tid == T_I8 && imm >= -128 && imm <= 127 {
        IrOp::ConstI8Imm {
            dst,
            imm: imm as i8 as u8,
        }
    } else {
        IrOp::ConstI32 { dst, imm }
    }
}

pub(super) fn same_rewriteable_op(a: &IrOp, b: &IrOp) -> bool {
    use IrOp::*;
    match (a, b) {
        (ConstI32 { dst: ad, imm: ai }, ConstI32 { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (ConstI8Imm { dst: ad, imm: ai }, ConstI8Imm { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (ConstF16 { dst: ad, bits: ab }, ConstF16 { dst: bd, bits: bb }) => ad == bd && ab == bb,
        (ConstBool { dst: ad, imm: ai }, ConstBool { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (Mov { dst: ad, src: asrc }, Mov { dst: bd, src: bsrc }) => ad == bd && asrc == bsrc,
        _ => false,
    }
}
