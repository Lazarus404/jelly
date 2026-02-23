use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, Span, Stmt, Ty};

pub(super) mod infer;
pub(super) mod instantiate;
pub(super) mod meta;
pub(super) mod rewrite;
pub(super) mod spans;

#[derive(Clone)]
pub(super) struct TemplateDef {
    #[allow(dead_code)]
    pub(super) span: Span,
    #[allow(dead_code)]
    pub(super) name: String,
    pub(super) type_params: Vec<String>,
    pub(super) ty: Ty,
    pub(super) expr: Expr,
}

/// Expander for templates.
/// This is used to expand templates into concrete specialized types and expressions.
pub(super) struct Expander {
    pub(super) templates: HashMap<String, TemplateDef>,
    pub(super) emitted: HashSet<String>, // spec name
    pub(super) stack: Vec<String>,       // spec name
    pub(super) out_specs: Vec<Stmt>,
    pub(super) known_vars: HashMap<String, Ty>, // top-level typed lets seen so far (MVP inference aid)
    pub(super) tmp_counter: u32,
}
