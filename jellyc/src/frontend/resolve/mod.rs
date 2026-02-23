use std::collections::HashMap;

use crate::ast::Span;

mod builtins;
mod ctx;
mod entry;
mod visit;

pub(crate) use entry::resolve_program;

/// Resolver for the compiler.
///
/// Enforces:
/// - names are defined before use (with capture exception inside fn literals)
/// - statement placement rules (imports/export at top-level, return/break/continue placement)
/// - builtin namespace member names (when not shadowed)
struct Resolver {
    builtins: HashMap<String, Span>,
    scopes: Vec<HashMap<String, Span>>,
    loop_depth: usize,
    fn_depth: usize,
    // When resolving a function literal, we intentionally hide outer scopes (no captures yet).
    // However, we still want to detect and report captures cleanly.
    outer_scopes_stack: Vec<Vec<HashMap<String, Span>>>,
}
