use super::*;
use crate::ast::{Span, Spanned};
use crate::ast::{Ty, TyKind};

fn ty_named(name: &str) -> Ty {
    Spanned::new(TyKind::Named(name.to_string()), Span::point(0))
}

#[test]
fn resolve_ty_with_subst_named_typevar() {
    let mut tc = TypeCtx::new_program_base();
    let subst = HashMap::from([("T".to_string(), T_I32)]);
    assert_eq!(
        tc.resolve_ty_with_subst(&ty_named("T"), &subst).unwrap(),
        T_I32
    );
}

#[test]
fn resolve_ty_with_subst_generic_array() {
    let mut tc = TypeCtx::new_program_base();
    let t = Spanned::new(
        TyKind::Generic {
            base: "Array".to_string(),
            args: vec![ty_named("T")],
        },
        Span::point(0),
    );
    let subst = HashMap::from([("T".to_string(), T_I32)]);
    assert_eq!(tc.resolve_ty_with_subst(&t, &subst).unwrap(), T_ARRAY_I32);
}

#[test]
fn resolve_ty_with_subst_unknown_still_errors() {
    let mut tc = TypeCtx::new_program_base();
    let subst: HashMap<String, u32> = HashMap::new();
    let err = tc
        .resolve_ty_with_subst(&ty_named("T"), &subst)
        .unwrap_err();
    assert!(err.message.contains("unknown type"));
}
