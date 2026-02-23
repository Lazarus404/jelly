use crate::ast::{Ty, TyKind};
use crate::error::{CompileError, ErrorKind};

/// Get the key for a type.
pub(super) fn type_key(t: &Ty) -> Result<String, CompileError> {
    fn enc(s: &str) -> String {
        // Identifier-safe fragment: [A-Za-z0-9_]+
        s.chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect()
    }

    match &t.node {
        TyKind::Named(n) => Ok(enc(n)),
        TyKind::Generic { base, args } => {
            let mut out = enc(base);
            for a in args {
                out.push('_');
                out.push_str(&type_key(a)?);
            }
            Ok(out)
        }
        TyKind::Tuple(elems) => {
            let mut out = "Tuple".to_string();
            out.push('_');
            out.push_str(&elems.len().to_string());
            for el in elems {
                out.push('_');
                out.push_str(&type_key(el)?);
            }
            Ok(out)
        }
        TyKind::Fun { .. } => Err(CompileError::new(
            ErrorKind::Type,
            t.span,
            "function types are not supported as template arguments yet",
        )),
    }
}

/// Get the name for a specialization.
pub(super) fn spec_name(base: &str, type_args: &[Ty]) -> Result<String, CompileError> {
    let mut out = base.to_string();
    for a in type_args {
        out.push_str("__");
        out.push_str(&type_key(a)?);
    }
    Ok(out)
}
