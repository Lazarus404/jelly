use crate::ast::Expr;
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

use super::TypeChecker;

pub(super) fn type_new(
    tc: &mut TypeChecker,
    e: &Expr,
    proto: &Expr,
    args: &[Expr],
    expect: Option<TypeId>,
) -> Result<TypeId, CompileError> {
    let t_proto0 = tc.check_expr(proto, None)?;
    let t_proto = if t_proto0 == T_DYNAMIC {
        tc.check_expr(proto, Some(T_OBJECT))?
    } else {
        t_proto0
    };
    if !tc.is_object_kind(t_proto) {
        return Err(CompileError::new(
            ErrorKind::Type,
            proto.span,
            "new expects an Object prototype",
        ));
    }
    for a in args {
        let _ = tc.check_expr(a, None)?;
    }
    // Type of the allocated instance:
    // - default: use the prototype's (possibly nominal) type
    // - allow erasure to plain Object if context expects Object
    // - otherwise, require the expected nominal type to match the prototype's nominal type
    let self_tid = match expect {
        Some(et) if et == T_OBJECT => T_OBJECT,
        Some(et) if tc.is_object_kind(et) => {
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
    Ok(self_tid)
}
