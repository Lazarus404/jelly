use crate::ir::TypeId;

// Base program-module type IDs. Must match `TypeCtx::new_program_base()`.
pub(crate) const T_BOOL: TypeId = crate::typectx::T_BOOL; // 0
pub(crate) const T_ATOM: TypeId = crate::typectx::T_ATOM; // 1
pub(crate) const T_I8: TypeId = crate::typectx::T_I8; // 2
pub(crate) const T_I16: TypeId = crate::typectx::T_I16; // 3
pub(crate) const T_I32: TypeId = crate::typectx::T_I32; // 4
pub(crate) const T_I64: TypeId = crate::typectx::T_I64; // 5
pub(crate) const T_F16: TypeId = crate::typectx::T_F16; // 6
pub(crate) const T_F32: TypeId = crate::typectx::T_F32; // 7
pub(crate) const T_F64: TypeId = crate::typectx::T_F64; // 8
pub(crate) const T_BYTES: TypeId = crate::typectx::T_BYTES; // 9
pub(crate) const T_DYNAMIC: TypeId = crate::typectx::T_DYNAMIC; // 10
pub(crate) const T_OBJECT: TypeId = crate::typectx::T_OBJECT; // 11
pub(crate) const T_ARRAY_I32: TypeId = crate::typectx::T_ARRAY_I32; // 12
pub(crate) const T_ARRAY_BYTES: TypeId = crate::typectx::T_ARRAY_BYTES; // 13
pub(crate) const T_LIST_I32: TypeId = crate::typectx::T_LIST_I32; // 14
pub(crate) const T_LIST_BYTES: TypeId = crate::typectx::T_LIST_BYTES; // 15
