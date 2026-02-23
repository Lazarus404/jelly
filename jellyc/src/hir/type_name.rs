use crate::ir::TypeId;
use crate::typectx::TypeCtx;
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16,
    T_I32, T_I64, T_I8, T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

pub fn type_name(tid: TypeId, tc: &TypeCtx) -> String {
    if let Some(te) = tc.types.get(tid as usize) {
        match te.kind {
            crate::jlyb::TypeKind::Function => {
                if let Some(sig) = tc.sigs.get(te.p0 as usize) {
                    let mut out = "(".to_string();
                    for (i, a) in sig.args.iter().enumerate() {
                        if i != 0 {
                            out.push_str(", ");
                        }
                        out.push_str(&type_name(*a, tc));
                    }
                    out.push_str(") -> ");
                    out.push_str(&type_name(sig.ret_type, tc));
                    return out;
                }
            }
            crate::jlyb::TypeKind::Object => {
                const TUPLE_TAG: u32 = 0x8000_0000;
                if (te.p0 & TUPLE_TAG) != 0 {
                    let sig_id = te.p0 & !TUPLE_TAG;
                    if let Some(sig) = tc.sigs.get(sig_id as usize) {
                        let mut out = "Tuple<".to_string();
                        for (i, a) in sig.args.iter().enumerate() {
                            if i != 0 {
                                out.push_str(", ");
                            }
                            out.push_str(&type_name(*a, tc));
                        }
                        out.push('>');
                        return out;
                    }
                }
                // Nominal object kinds: use stable tag (hash of nominal key).
                if te.p0 != 0 {
                    return format!("Object#{:08x}", te.p0);
                }
            }
            _ => {}
        }
    }
    match tid {
        T_BOOL => "Bool".to_string(),
        T_ATOM => "Atom".to_string(),
        T_I8 => "I8".to_string(),
        T_I16 => "I16".to_string(),
        T_I32 => "I32".to_string(),
        T_I64 => "I64".to_string(),
        T_F16 => "F16".to_string(),
        T_F32 => "F32".to_string(),
        T_F64 => "F64".to_string(),
        T_BYTES => "Bytes".to_string(),
        T_DYNAMIC => "Dynamic".to_string(),
        T_OBJECT => "Object".to_string(),
        T_ARRAY_I32 => "Array<I32>".to_string(),
        T_ARRAY_BYTES => "Array<Bytes>".to_string(),
        T_LIST_I32 => "List<I32>".to_string(),
        T_LIST_BYTES => "List<Bytes>".to_string(),
        _ => format!("t{tid}"),
    }
}
