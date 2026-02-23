use std::collections::HashMap;

use crate::ast::Span;

pub(super) const BUILTIN_NAMESPACES: &[&str] = &[
    "System", "Bytes", "Integer", "Float", "Math", "Atom", "Object", "Array", "List",
];

pub(super) fn builtin_namespace_spans() -> HashMap<String, Span> {
    let mut builtins = HashMap::new();
    for &name in BUILTIN_NAMESPACES {
        builtins.insert(name.to_string(), Span::point(0));
    }
    builtins
}

pub(super) fn is_valid_builtin_member(ns: &str, name: &str) -> bool {
    match ns {
        "Bytes" => matches!(name, "new" | "len" | "get_u8" | "set_u8" | "slice" | "eq"),
        "Array" => matches!(name, "new" | "len" | "get" | "set"),
        "List" => matches!(name, "nil" | "cons" | "head" | "tail" | "is_nil"),
        "Atom" => matches!(name, "intern"),
        "Object" => matches!(name, "get" | "set"),
        "System" => matches!(name, "assert" | "exit"),
        "Integer" => matches!(name, "to_i8" | "to_i16" | "to_i32" | "to_i64"),
        "Float" => matches!(name, "to_f16" | "to_f32" | "to_f64"),
        "Math" => matches!(name, "sqrt"),
        _ => true,
    }
}
