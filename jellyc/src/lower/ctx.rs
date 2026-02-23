use std::collections::HashMap;

use crate::error::{CompileError, CompileWarning};
use crate::hir::{ConstInit, NodeId};
use crate::ir::{BlockId, TypeId, VRegId};
use crate::typectx::TypeCtx;

use super::env::Binding;

/// (nominal_type_id, atom_id) -> func_index for prototype methods known at compile time.
pub(crate) type MethodTable = HashMap<(TypeId, u32), u32>;

pub(crate) struct LowerCtx {
    pub type_ctx: TypeCtx,
    pub const_bytes: Vec<Vec<u8>>,
    pub const_i64: Vec<i64>,
    pub const_f64: Vec<f64>,
    pub atoms: Vec<Vec<u8>>,
    pub atom_ids: HashMap<String, u32>,
    pub env_stack: Vec<HashMap<String, Binding>>,
    pub loop_stack: Vec<LoopTargets>,
    pub fn_stack: Vec<FnCtx>,
    pub nested_funcs: Vec<crate::ir::IrFunction>,
    /// Monotonic allocator for nested (lambda) function indices.
    ///
    /// This must be independent of `nested_funcs.len()` because we can lower nested lambdas
    /// while their parent lambda is still being built (and thus not yet pushed into
    /// `nested_funcs`), which would otherwise cause duplicate function indices.
    pub next_nested_fun_index: u32,
    pub pending_fn_self: Option<(String, TypeId)>,
    pub user_top_level_fun_count: u32,
    pub exports_obj: Option<VRegId>,
    pub module_alias_exports: HashMap<String, HashMap<String, TypeId>>,
    pub module_key_to_alias: HashMap<String, String>,
    pub sem_binding_types: HashMap<NodeId, TypeId>,
    pub sem_expr_types: HashMap<NodeId, TypeId>,
    pub sem_const_inits: HashMap<NodeId, ConstInit>,
    pub warnings: Vec<CompileWarning>,
    /// Method table: (nominal_tid, atom_id) -> func_index. Populated when lowering prototype inits.
    pub method_table: MethodTable,
    /// When lowering an object literal that is a prototype init, holds the nominal type id.
    pub proto_for_nominal: Option<TypeId>,
    /// When set, fn literal lowering records (nom_tid, atom_id) -> func_index for ConstFun.
    pub recording_method: Option<(TypeId, u32)>,
    /// After lowering a fn literal that emits ConstFun, holds the func_index for binding.
    pub last_const_fun_index: Option<u32>,
    /// When true (REPL), emit ObjSetAtom for all top-level lets so they are visible to the next line.
    pub export_all_lets: bool,
}

impl LowerCtx {
    pub(crate) fn with_env_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        self.env_stack.push(HashMap::new());
        let out = f(self);
        self.env_stack.pop();
        out
    }

    pub(crate) fn with_env<T>(
        &mut self,
        env: HashMap<String, Binding>,
        f: impl FnOnce(&mut Self) -> Result<T, CompileError>,
    ) -> Result<T, CompileError> {
        self.env_stack.push(env);
        let out = f(self);
        self.env_stack.pop();
        out
    }
}

#[derive(Clone)]
pub(crate) struct FnCtx {
    pub ret_tid: TypeId,
    pub outer_env: Option<Vec<HashMap<String, Binding>>>,
    pub captures: HashMap<String, (Binding, VRegId)>,
    pub capture_order: Vec<String>,
    pub self_name: Option<String>,
    pub self_fun_tid: Option<TypeId>,
    pub self_loop_head: BlockId,
}

#[derive(Clone, Copy)]
pub(crate) struct LoopTargets {
    pub break_tgt: BlockId,
    pub continue_tgt: BlockId,
}
