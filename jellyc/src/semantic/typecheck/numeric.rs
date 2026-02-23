use crate::ir::TypeId;
use crate::typectx::{T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

pub(super) fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

fn numeric_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        T_F16 => 4,
        T_F32 => 5,
        T_F64 => 6,
        _ => 255,
    }
}

pub(super) fn join_numeric(a: TypeId, b: TypeId) -> TypeId {
    if numeric_rank(a) >= numeric_rank(b) {
        a
    } else {
        b
    }
}
