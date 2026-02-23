use std::collections::HashSet;

use crate::error::{CompileError, ErrorKind};
use crate::hir::{ConstInit, ConstValue};

fn resolve_alias_value(
    name: &str,
    span: crate::ast::Span,
    lookup_const: &mut dyn FnMut(&str) -> Option<ConstInit>,
    visiting: &mut HashSet<String>,
) -> Result<ConstValue, CompileError> {
    if !visiting.insert(name.to_string()) {
        return Err(CompileError::new(
            ErrorKind::Type,
            span,
            "cycle in const initializer aliases",
        ));
    }
    let init = lookup_const(name).ok_or_else(|| {
        CompileError::new(
            ErrorKind::Type,
            span,
            format!("unknown const '{name}' in const initializer"),
        )
    })?;
    match init {
        ConstInit::Value(v) => Ok(v),
        ConstInit::Alias(n2) => resolve_alias_value(&n2, span, lookup_const, visiting),
    }
}

pub(super) fn force_value(
    init: ConstInit,
    span: crate::ast::Span,
    lookup_const: &mut dyn FnMut(&str) -> Option<ConstInit>,
) -> Result<ConstValue, CompileError> {
    match init {
        ConstInit::Value(v) => Ok(v),
        ConstInit::Alias(n) => resolve_alias_value(&n, span, lookup_const, &mut HashSet::new()),
    }
}
