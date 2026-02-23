use crate::ir::IrBuilder;
use crate::ir::TypeId;
use crate::typectx::TypeCtx;

pub(crate) fn is_object_kind(tc: &TypeCtx, tid: TypeId) -> bool {
    tc.types
        .get(tid as usize)
        .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
}

pub(crate) fn ensure_open_block(b: &mut IrBuilder) {
    if !b.is_open() {
        let nb = b.new_block(Some("cont".to_string()));
        b.set_block(nb);
    }
}
