use crate::jlyb::{Module, TypeKind};

pub(super) fn type_kind(m: &Module, tid: u32) -> Result<TypeKind, String> {
    m.types.get(tid as usize).map(|te| te.kind).ok_or_else(|| {
        format!(
            "type id out of range: tid={tid} (types_len={})",
            m.types.len()
        )
    })
}

pub(super) fn rk(m: &Module, reg_types: &[u32], r: u8) -> Result<TypeKind, String> {
    let tid = *reg_types
        .get(r as usize)
        .ok_or_else(|| format!("reg out of range: r={r} (nregs={})", reg_types.len()))?;
    type_kind(m, tid)
}

pub(super) fn slot_size_bytes(k: TypeKind) -> usize {
    match k {
        TypeKind::I8
        | TypeKind::I16
        | TypeKind::I32
        | TypeKind::F32
        | TypeKind::F16
        | TypeKind::Bool
        | TypeKind::Atom => 4,
        TypeKind::I64 | TypeKind::F64 => 8,
        TypeKind::Dynamic
        | TypeKind::Bytes
        | TypeKind::Function
        | TypeKind::List
        | TypeKind::Array
        | TypeKind::Object
        | TypeKind::Abstract => std::mem::size_of::<usize>(),
    }
}

pub(super) fn is_i32ish(k: TypeKind) -> bool {
    matches!(k, TypeKind::I8 | TypeKind::I16 | TypeKind::I32)
}

pub(super) fn is_ptr_kind(k: TypeKind) -> bool {
    matches!(
        k,
        TypeKind::Bytes
            | TypeKind::Function
            | TypeKind::List
            | TypeKind::Array
            | TypeKind::Object
            | TypeKind::Abstract
    )
}

pub(super) fn unary_elem_tid(
    m: &Module,
    container_tid: u32,
    want: TypeKind,
    why: &'static str,
) -> Result<u32, String> {
    let te = m
        .types
        .get(container_tid as usize)
        .ok_or_else(|| format!("{why}: type id out of range"))?;
    if te.kind != want {
        return Err(format!(
            "{why}: expected {want:?} container kind, got {:?}",
            te.kind
        ));
    }
    Ok(te.p0)
}

pub(super) fn err(func_i: usize, pc: usize, msg: impl Into<String>) -> String {
    format!("func[{func_i}] pc={pc}: {}", msg.into())
}
