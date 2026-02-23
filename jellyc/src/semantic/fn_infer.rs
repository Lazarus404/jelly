use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Stmt, StmtKind, Ty};
use crate::error::{CompileError, ErrorKind};
use crate::ir::TypeId;
use crate::typectx::{
    TypeCtx, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8,
    T_OBJECT,
};

use crate::builtin_constraints::{
    array_constraints_from_arr_tid, builtin_constraints, list_constraints_from_list_tid,
    ArgConstraint,
};

#[derive(Clone, Copy, Debug)]
enum ITy {
    Known(TypeId),
    Var(usize),
}

#[derive(Debug)]
struct Dsu {
    parent: Vec<usize>,
    value: Vec<Option<TypeId>>,
    numeric_hint: Vec<bool>,
}

impl Dsu {
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            value: vec![None; n],
            numeric_hint: vec![false; n],
        }
    }

    fn find(&mut self, x: usize) -> usize {
        let p = self.parent[x];
        if p == x {
            return x;
        }
        let r = self.find(p);
        self.parent[x] = r;
        r
    }

    fn union(&mut self, a: usize, b: usize) -> Result<usize, CompileError> {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return Ok(ra);
        }
        self.parent[rb] = ra;
        self.numeric_hint[ra] |= self.numeric_hint[rb];
        match (self.value[ra], self.value[rb]) {
            (Some(x), Some(y)) if x != y => {
                if is_numeric(x) && is_numeric(y) {
                    self.value[ra] = Some(join_numeric(x, y));
                    Ok(ra)
                } else {
                    Err(CompileError::new(ErrorKind::Type, crate::ast::Span::point(0), "inferred type conflict"))
                }
            }
            (None, Some(y)) => {
                self.value[ra] = Some(y);
                Ok(ra)
            }
            _ => Ok(ra),
        }
    }

    fn constrain(&mut self, v: usize, tid: TypeId) -> Result<(), CompileError> {
        let r = self.find(v);
        if let Some(cur) = self.value[r] {
            if cur != tid {
                if is_numeric(cur) && is_numeric(tid) {
                    self.value[r] = Some(join_numeric(cur, tid));
                    return Ok(());
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "inferred type conflict",
                ));
            }
        } else {
            self.value[r] = Some(tid);
        }
        Ok(())
    }

    fn mark_numeric(&mut self, v: usize) {
        let r = self.find(v);
        self.numeric_hint[r] = true;
    }

    fn resolve_or_default(&mut self, v: usize) -> TypeId {
        let r = self.find(v);
        if let Some(t) = self.value[r] {
            return t;
        }
        if self.numeric_hint[r] {
            return T_I32;
        }
        T_DYNAMIC
    }
}

fn is_numeric(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64 | T_F16 | T_F32 | T_F64)
}

fn numeric_rank(t: TypeId) -> u8 {
    match t {
        T_I8 => 0,
        T_I16 => 1,
        T_I32 => 2,
        T_I64 => 3,
        T_F16 => 4,
        T_F32 => 5,
        T_F64 => 6,
        _ => 255,
    }
}

fn join_numeric(a: TypeId, b: TypeId) -> TypeId {
    if numeric_rank(a) >= numeric_rank(b) {
        a
    } else {
        b
    }
}

fn unify(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    match (a, b) {
        (ITy::Known(x), ITy::Known(y)) => {
            if x != y {
                if is_numeric(x) && is_numeric(y) {
                    return Ok(ITy::Known(join_numeric(x, y)));
                }
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "inferred type conflict",
                ));
            }
            Ok(ITy::Known(x))
        }
        (ITy::Var(v), ITy::Known(t)) | (ITy::Known(t), ITy::Var(v)) => {
            dsu.constrain(v, t)?;
            Ok(ITy::Known(t))
        }
        (ITy::Var(a), ITy::Var(b)) => {
            let r = dsu.union(a, b)?;
            Ok(ITy::Var(r))
        }
    }
}

fn infer_numeric_bin(dsu: &mut Dsu, a: ITy, b: ITy) -> Result<ITy, CompileError> {
    match (a, b) {
        (ITy::Known(ta), ITy::Known(tb)) => {
            if !is_numeric(ta) || !is_numeric(tb) {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    crate::ast::Span::point(0),
                    "numeric operator expects numeric operands",
                ));
            }
            Ok(ITy::Known(join_numeric(ta, tb)))
        }
        (ITy::Known(ta), ITy::Var(vb)) if is_numeric(ta) => {
            dsu.mark_numeric(vb);
            dsu.constrain(vb, ta)?;
            Ok(ITy::Known(ta))
        }
        (ITy::Var(va), ITy::Known(tb)) if is_numeric(tb) => {
            dsu.mark_numeric(va);
            dsu.constrain(va, tb)?;
            Ok(ITy::Known(tb))
        }
        (ITy::Var(va), ITy::Var(vb)) => {
            dsu.mark_numeric(va);
            dsu.mark_numeric(vb);
            let r = dsu.union(va, vb)?;
            Ok(ITy::Var(r))
        }
        _ => Err(CompileError::new(
            ErrorKind::Type,
            crate::ast::Span::point(0),
            "numeric operator expects numeric operands",
        )),
    }
}

pub fn infer_fn_type_for_let(
    self_name: &str,
    params: &[(String, Option<Ty>)],
    body: &[Stmt],
    tail: &Option<Box<Expr>>,
    type_ctx: &mut TypeCtx,
    env_stack: &[HashMap<String, TypeId>],
    module_alias_exports: &HashMap<String, HashMap<String, TypeId>>,
) -> Result<(TypeId, Vec<TypeId>, TypeId), CompileError> {
    struct Infer<'a> {
        type_ctx: &'a mut TypeCtx,
        self_name: &'a str,
        env_stack: &'a [HashMap<String, TypeId>],
        module_alias_exports: &'a HashMap<String, HashMap<String, TypeId>>,
        param_vars: Vec<usize>,
        ret_var: usize,
        dsu: Dsu,
        index_int_constraints: Vec<(usize, crate::ast::Span)>,
        scopes: Vec<HashMap<String, ITy>>,
    }

    impl<'a> Infer<'a> {
        fn new(
            type_ctx: &'a mut TypeCtx,
            self_name: &'a str,
            nparams: usize,
            env_stack: &'a [HashMap<String, TypeId>],
            module_alias_exports: &'a HashMap<String, HashMap<String, TypeId>>,
        ) -> Self {
            Self {
                type_ctx,
                self_name,
                env_stack,
                module_alias_exports,
                param_vars: (0..nparams).collect(),
                ret_var: nparams,
                dsu: Dsu::new(nparams + 1),
                index_int_constraints: Vec::new(),
                scopes: vec![HashMap::new()],
            }
        }

        fn bind(&mut self, name: &str, ty: ITy) {
            self.scopes.last_mut().expect("scope").insert(name.to_string(), ty);
        }

        fn lookup(&self, name: &str) -> Option<ITy> {
            for s in self.scopes.iter().rev() {
                if let Some(t) = s.get(name) {
                    return Some(*t);
                }
            }
            self.env_stack
                .iter()
                .rev()
                .find_map(|m| m.get(name))
                .copied()
                .map(ITy::Known)
        }

        fn resolve_ty_ann(&mut self, t: &Ty) -> Result<TypeId, CompileError> {
            self.type_ctx.resolve_ty(t)
        }

        fn resolve_known_tid(&mut self, t: ITy) -> Option<TypeId> {
            match t {
                ITy::Known(tid) => Some(tid),
                ITy::Var(v) => {
                    let r = self.dsu.find(v);
                    self.dsu.value[r]
                }
            }
        }

        fn is_object_kind_tid(&self, tid: TypeId) -> bool {
            self.type_ctx
                .types
                .get(tid as usize)
                .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Object)
        }

        fn require_int_index(&mut self, t: ITy, span: crate::ast::Span) -> Result<(), CompileError> {
            fn ok_index_tid(tid: TypeId) -> bool {
                matches!(tid, T_DYNAMIC | T_I8 | T_I16 | T_I32 | T_I64)
            }
            if let Some(tid) = self.resolve_known_tid(t) {
                if !ok_index_tid(tid) {
                    return Err(CompileError::new(ErrorKind::Type, span, "index must be an integer"));
                }
                return Ok(());
            }
            if let ITy::Var(v) = t {
                self.index_int_constraints.push((v, span));
            }
            Ok(())
        }

        fn check_int_index_constraints(&mut self) -> Result<(), CompileError> {
            for (v, span) in std::mem::take(&mut self.index_int_constraints) {
                let tid = self.dsu.resolve_or_default(v);
                if !matches!(tid, T_DYNAMIC | T_I8 | T_I16 | T_I32 | T_I64) {
                    return Err(CompileError::new(ErrorKind::Type, span, "index must be an integer"));
                }
            }
            Ok(())
        }

        fn infer_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
            match &s.node {
                StmtKind::Let { name, ty, expr, .. } => {
                    let et = if let Some(t) = ty {
                        Some(self.resolve_ty_ann(t)?)
                    } else {
                        None
                    };
                    let mut t = self.infer_expr(expr)?;
                    // `fn_infer` is a lightweight pass used only to infer the outer function's
                    // signature. Nested function literals are treated as `Dynamic` by `infer_expr`
                    // (we don't infer their signatures here). When there *is* an explicit
                    // annotation on the binding, trust it to avoid spurious conflicts.
                    if let (Some(et), ExprKind::Fn { .. }) = (et, &expr.node) {
                        let is_fun = self
                            .type_ctx
                            .types
                            .get(et as usize)
                            .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Function);
                        if is_fun {
                            t = ITy::Known(et);
                        }
                    }
                    let t = if let Some(et) = et {
                        unify(&mut self.dsu, t, ITy::Known(et))?
                    } else {
                        t
                    };
                    self.bind(name, t);
                    Ok(())
                }
                StmtKind::Assign { name, expr } => {
                    if let Some(dst) = self.lookup(name) {
                        let mut t = self.infer_expr(expr)?;
                        // If assigning a `fn {..}` to a variable that is already known to be a
                        // function type, trust the destination type (this pass doesn't infer
                        // nested function signatures).
                        if let ExprKind::Fn { .. } = &expr.node {
                            if let Some(dst_tid) = self.resolve_known_tid(dst) {
                                let is_fun = self
                                    .type_ctx
                                    .types
                                    .get(dst_tid as usize)
                                    .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Function);
                                if is_fun {
                                    t = ITy::Known(dst_tid);
                                }
                            }
                        }
                        let _ = unify(&mut self.dsu, dst, t)?;
                    }
                    Ok(())
                }
                StmtKind::Return { expr } => {
                    if let Some(e) = expr {
                        let mut t = self.infer_expr(e)?;
                        // If returning a `fn {..}` and the return type is already constrained to
                        // a function type, trust the constrained return type.
                        if let ExprKind::Fn { .. } = &e.node {
                            let cur = self.resolve_known_tid(ITy::Var(self.ret_var));
                            if let Some(ret_tid) = cur {
                                let is_fun = self
                                    .type_ctx
                                    .types
                                    .get(ret_tid as usize)
                                    .is_some_and(|te| te.kind == crate::jlyb::TypeKind::Function);
                                if is_fun {
                                    t = ITy::Known(ret_tid);
                                }
                            }
                        }
                        let _ = unify(&mut self.dsu, ITy::Var(self.ret_var), t)?;
                    }
                    Ok(())
                }
                StmtKind::Expr { expr } => {
                    let _ = self.infer_expr(expr)?;
                    Ok(())
                }
                StmtKind::While { cond, body } => {
                    let tc = self.infer_expr(cond)?;
                    // Jelly truthiness: loop conditions need not be `Bool`.
                    let _ = tc;
                    self.scopes.push(HashMap::new());
                    for st in body {
                        self.infer_stmt(st)?;
                    }
                    self.scopes.pop();
                    Ok(())
                }
                StmtKind::Break | StmtKind::Continue => Ok(()),
                StmtKind::Throw { expr } => {
                    let _ = self.infer_expr(expr)?;
                    Ok(())
                }
                StmtKind::MemberAssign { base, expr, .. } => {
                    let _ = self.infer_expr(base)?;
                    let _ = self.infer_expr(expr)?;
                    Ok(())
                }
                StmtKind::IndexAssign { base, index, expr } => {
                    let tb = self.infer_expr(base)?;
                    let ti = self.infer_expr(index)?;
                    self.require_int_index(ti, index.span)?;
                    let te = self.infer_expr(expr)?;
                    if let Some(base_tid) = self.resolve_known_tid(tb) {
                        match base_tid {
                            crate::typectx::T_ARRAY_I32 => {
                                let _ = unify(&mut self.dsu, te, ITy::Known(T_I32))?;
                            }
                            crate::typectx::T_ARRAY_BYTES => {
                                let _ = unify(&mut self.dsu, te, ITy::Known(T_BYTES))?;
                            }
                            T_BYTES => {
                                let _ = unify(&mut self.dsu, te, ITy::Known(T_I32))?;
                            }
                            _ => {}
                        }
                    }
                    Ok(())
                }
                StmtKind::ImportModule { .. } | StmtKind::ImportFrom { .. } | StmtKind::Prototype { .. } => Ok(()),
            }
        }

        fn infer_expr(&mut self, e: &Expr) -> Result<ITy, CompileError> {
            match &e.node {
                ExprKind::I32Lit(_) => Ok(ITy::Known(T_I32)),
                ExprKind::I8Lit(_) => Ok(ITy::Known(T_I8)),
                ExprKind::I16Lit(_) => Ok(ITy::Known(T_I16)),
                ExprKind::I64Lit(_) => Ok(ITy::Known(T_I64)),
                ExprKind::F16Lit(_) => Ok(ITy::Known(T_F16)),
                ExprKind::F64Lit(_) => Ok(ITy::Known(T_F64)),
                ExprKind::BoolLit(_) => Ok(ITy::Known(T_BOOL)),
                ExprKind::BytesLit(_) => Ok(ITy::Known(T_BYTES)),
                ExprKind::AtomLit(_) => Ok(ITy::Known(T_ATOM)),
                ExprKind::Null => Ok(ITy::Known(T_DYNAMIC)),
                ExprKind::Var(n) => Ok(self.lookup(n).unwrap_or(ITy::Known(T_DYNAMIC))),
                ExprKind::Neg(x) => {
                    let t = self.infer_expr(x)?;
                    if let ITy::Var(v) = t {
                        self.dsu.mark_numeric(v);
                    }
                    Ok(t)
                }
                ExprKind::Not(x) => {
                    let t = self.infer_expr(x)?;
                    let _ = unify(&mut self.dsu, t, ITy::Known(T_BOOL))?;
                    Ok(ITy::Known(T_BOOL))
                }
                ExprKind::Truthy(x) => {
                    let _ = self.infer_expr(x)?;
                    Ok(ITy::Known(T_BOOL))
                }
                ExprKind::Add(a, b) => {
                    let ta = self.infer_expr(a)?;
                    let tb = self.infer_expr(b)?;
                    if matches!(ta, ITy::Known(T_BYTES)) || matches!(tb, ITy::Known(T_BYTES)) {
                        let _ = unify(&mut self.dsu, ta, ITy::Known(T_BYTES))?;
                        let _ = unify(&mut self.dsu, tb, ITy::Known(T_BYTES))?;
                        Ok(ITy::Known(T_BYTES))
                    } else {
                        infer_numeric_bin(&mut self.dsu, ta, tb)
                    }
                }
                ExprKind::Sub(a, b) | ExprKind::Mul(a, b) | ExprKind::Div(a, b) => {
                    let ta = self.infer_expr(a)?;
                    let tb = self.infer_expr(b)?;
                    infer_numeric_bin(&mut self.dsu, ta, tb)
                }
                ExprKind::Eq(a, b) | ExprKind::Ne(a, b) => {
                    let ta = self.infer_expr(a)?;
                    let tb = self.infer_expr(b)?;
                    let _ = match (ta, tb) {
                        (ITy::Known(x), ITy::Known(y)) if is_numeric(x) && is_numeric(y) => Ok(ITy::Known(join_numeric(x, y))),
                        // Bool comparisons are truthiness-based; don't constrain the other side.
                        (ITy::Known(T_BOOL), _) | (_, ITy::Known(T_BOOL)) => Ok(ITy::Known(T_BOOL)),
                        // `Dynamic` can flow through unknown calls/values while inferring; don't
                        // reject the whole function type just because we can't pin it here.
                        (ITy::Known(T_DYNAMIC), _) | (_, ITy::Known(T_DYNAMIC)) => Ok(ITy::Known(T_DYNAMIC)),
                        (x, y) => unify(&mut self.dsu, x, y),
                    }?;
                    Ok(ITy::Known(T_BOOL))
                }
                ExprKind::Lt(a, b) | ExprKind::Le(a, b) | ExprKind::Gt(a, b) | ExprKind::Ge(a, b) => {
                    let ta = self.infer_expr(a)?;
                    let tb = self.infer_expr(b)?;
                    let _ = infer_numeric_bin(&mut self.dsu, ta, tb)?;
                    Ok(ITy::Known(T_BOOL))
                }
                ExprKind::And(a, b) | ExprKind::Or(a, b) => {
                    let ta = self.infer_expr(a)?;
                    let tb = self.infer_expr(b)?;
                    // Jelly truthiness: `&&`/`||` are boolean operators, but their operands are
                    // not required to be `Bool` (everything except `null` and `false` is truthy).
                    // Don't constrain operand types during inference.
                    let _ = ta;
                    let _ = tb;
                    Ok(ITy::Known(T_BOOL))
                }
                ExprKind::If { cond, then_br, else_br } => {
                    let tc = self.infer_expr(cond)?;
                    let _ = tc;
                    let tt = self.infer_expr(then_br)?;
                    let te = self.infer_expr(else_br)?;
                    unify(&mut self.dsu, tt, te)
                }
                ExprKind::Block { stmts, expr } => {
                    self.scopes.push(HashMap::new());
                    for s in stmts {
                        self.infer_stmt(s)?;
                    }
                    let t = self.infer_expr(expr)?;
                    self.scopes.pop();
                    Ok(t)
                }
                ExprKind::Call { callee, type_args, args } => {
                    // Keep return-type-dependent builtin behavior centralized.
                    if let ExprKind::Member { base, name } = &callee.node {
                        if let ExprKind::Var(ns) = &base.node {
                            if ns == "Array" && matches!(name.as_str(), "len" | "get" | "set") {
                                if !type_args.is_empty() {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        format!("Array.{} does not take type arguments", name),
                                    ));
                                }
                                // Always infer args for constraints/numeric hints.
                                if let Some(a0) = args.get(0) {
                                    let t0 = self.infer_expr(a0)?;
                                    if let Some(arr_tid) = self.resolve_known_tid(t0) {
                                        if let Some(cs) = array_constraints_from_arr_tid(
                                            name,
                                            args.len(),
                                            arr_tid,
                                            e.span,
                                        )? {
                                            for (i, a) in args.iter().enumerate() {
                                                let ta = self.infer_expr(a)?;
                                                match cs.args.get(i).copied().unwrap_or(ArgConstraint::Any) {
                                                    ArgConstraint::Exact(tid) => {
                                                        if tid != T_DYNAMIC {
                                                            let _ = unify(&mut self.dsu, ta, ITy::Known(tid))?;
                                                        }
                                                    }
                                                    ArgConstraint::ObjectKind => match ta {
                                                        ITy::Known(tid) => {
                                                            if tid != T_DYNAMIC && !self.is_object_kind_tid(tid) {
                                                                return Err(CompileError::new(
                                                                    ErrorKind::Type,
                                                                    e.span,
                                                                    "builtin expects an Object",
                                                                ));
                                                            }
                                                        }
                                                        ITy::Var(v) => {
                                                            let _ = unify(&mut self.dsu, ITy::Var(v), ITy::Known(T_OBJECT))?;
                                                        }
                                                    },
                                                    ArgConstraint::Numeric => match ta {
                                                        ITy::Known(tid) => {
                                                            if !is_numeric(tid) {
                                                                return Err(CompileError::new(ErrorKind::Type, e.span, "builtin expects numeric"));
                                                            }
                                                        }
                                                        ITy::Var(v) => self.dsu.mark_numeric(v),
                                                    },
                                                    ArgConstraint::Any => {}
                                                }
                                            }
                                            return Ok(ITy::Known(cs.ret));
                                        }
                                    }
                                }
                                // Unknown array type → ambiguous for inference.
                                for a in args {
                                    let _ = self.infer_expr(a)?;
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }

                            if ns == "List" && matches!(name.as_str(), "head" | "tail" | "is_nil") {
                                if !type_args.is_empty() {
                                    return Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        format!("List.{} does not take type arguments", name),
                                    ));
                                }
                                if let Some(a0) = args.get(0) {
                                    let t0 = self.infer_expr(a0)?;
                                    if let Some(list_tid) = self.resolve_known_tid(t0) {
                                        if let Some(cs) =
                                            list_constraints_from_list_tid(name, args.len(), list_tid, e.span)?
                                        {
                                            for (i, a) in args.iter().enumerate() {
                                                let ta = self.infer_expr(a)?;
                                                match cs.args.get(i).copied().unwrap_or(ArgConstraint::Any) {
                                                    ArgConstraint::Exact(tid) => {
                                                        if tid != T_DYNAMIC {
                                                            let _ = unify(&mut self.dsu, ta, ITy::Known(tid))?;
                                                        }
                                                    }
                                                    ArgConstraint::ObjectKind => match ta {
                                                        ITy::Known(tid) => {
                                                            if tid != T_DYNAMIC && !self.is_object_kind_tid(tid) {
                                                                return Err(CompileError::new(
                                                                    ErrorKind::Type,
                                                                    e.span,
                                                                    "builtin expects an Object",
                                                                ));
                                                            }
                                                        }
                                                        ITy::Var(v) => {
                                                            let _ = unify(&mut self.dsu, ITy::Var(v), ITy::Known(T_OBJECT))?;
                                                        }
                                                    },
                                                    ArgConstraint::Numeric => match ta {
                                                        ITy::Known(tid) => {
                                                            if !is_numeric(tid) {
                                                                return Err(CompileError::new(
                                                                    ErrorKind::Type,
                                                                    e.span,
                                                                    "builtin expects numeric",
                                                                ));
                                                            }
                                                        }
                                                        ITy::Var(v) => self.dsu.mark_numeric(v),
                                                    },
                                                    ArgConstraint::Any => {}
                                                }
                                            }
                                            return Ok(ITy::Known(cs.ret));
                                        }
                                    }
                                }
                                for a in args {
                                    let _ = self.infer_expr(a)?;
                                }
                                return Ok(ITy::Known(T_DYNAMIC));
                            }
                        }
                    }

                    if let Some(cs) = builtin_constraints(callee, type_args, args.len(), None, self.type_ctx, true, e.span)? {
                        for (i, a) in args.iter().enumerate() {
                            let ta = self.infer_expr(a)?;
                            match cs.args.get(i).copied().unwrap_or(ArgConstraint::Any) {
                                ArgConstraint::Exact(tid) => {
                                    if tid != T_DYNAMIC {
                                        let _ = unify(&mut self.dsu, ta, ITy::Known(tid))?;
                                    }
                                }
                                ArgConstraint::ObjectKind => match ta {
                                    ITy::Known(tid) => {
                                        if tid != T_DYNAMIC && !self.is_object_kind_tid(tid) {
                                            return Err(CompileError::new(ErrorKind::Type, e.span, "builtin expects an Object"));
                                        }
                                    }
                                    ITy::Var(v) => {
                                        let _ = unify(&mut self.dsu, ITy::Var(v), ITy::Known(T_OBJECT))?;
                                    }
                                },
                                ArgConstraint::Numeric => match ta {
                                    ITy::Known(tid) => {
                                        if !is_numeric(tid) {
                                            return Err(CompileError::new(ErrorKind::Type, e.span, "builtin expects numeric"));
                                        }
                                    }
                                    ITy::Var(v) => self.dsu.mark_numeric(v),
                                },
                                ArgConstraint::Any => {}
                            }
                        }
                        return Ok(ITy::Known(cs.ret));
                    }

                    if let ExprKind::Var(n) = &callee.node {
                        if n == self.self_name {
                            if args.len() != self.param_vars.len() {
                                return Err(CompileError::new(ErrorKind::Type, e.span, "call arity mismatch"));
                            }
                            for (i, a) in args.iter().enumerate() {
                                let ta = self.infer_expr(a)?;
                                let _ = unify(&mut self.dsu, ITy::Var(self.param_vars[i]), ta)?;
                            }
                            return Ok(ITy::Var(self.ret_var));
                        }
                    }

                    // Module export call: `Mod.f(...)` with known export type.
                    if let ExprKind::Member { base, name } = &callee.node {
                        if let ExprKind::Var(alias) = &base.node {
                            if let Some(exports) = self.module_alias_exports.get(alias) {
                                if let Some(&fun_tid) = exports.get(name) {
                                    return self.infer_call_known_fun(fun_tid, args, e.span);
                                }
                            }
                        }
                    }

                    if let ExprKind::Var(n) = &callee.node {
                        if let Some(ITy::Known(fun_tid)) = self.lookup(n) {
                            return self.infer_call_known_fun(fun_tid, args, e.span);
                        }
                    }

                    Ok(ITy::Known(T_DYNAMIC))
                }
                ExprKind::Index { base, index } => {
                    let _ = self.infer_expr(index)?;
                    let tb = self.infer_expr(base)?;
                    if let Some(base_tid) = self.resolve_known_tid(tb) {
                        match base_tid {
                            crate::typectx::T_ARRAY_I32 => Ok(ITy::Known(T_I32)),
                            crate::typectx::T_ARRAY_BYTES => Ok(ITy::Known(T_BYTES)),
                            T_BYTES => Ok(ITy::Known(T_I32)),
                            _ => Ok(ITy::Known(T_DYNAMIC)),
                        }
                    } else {
                        Ok(ITy::Known(T_DYNAMIC))
                    }
                }
                ExprKind::Member { .. }
                | ExprKind::TypeApp { .. }
                | ExprKind::ArrayLit(_)
                | ExprKind::TupleLit(_)
                | ExprKind::ObjLit(_)
                | ExprKind::Fn { .. }
                | ExprKind::Try { .. }
                | ExprKind::Match { .. }
                | ExprKind::New { .. } => Ok(ITy::Known(T_DYNAMIC)),
            }
        }

        fn infer_call_known_fun(&mut self, fun_tid: TypeId, args: &[Expr], span: crate::ast::Span) -> Result<ITy, CompileError> {
            let te = self
                .type_ctx
                .types
                .get(fun_tid as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad function type id"))?;
            if te.kind != crate::jlyb::TypeKind::Function {
                return Ok(ITy::Known(T_DYNAMIC));
            }
            let sig = self
                .type_ctx
                .sigs
                .get(te.p0 as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad fun sig id"))?
                .clone();
            if sig.args.len() != args.len() {
                return Err(CompileError::new(ErrorKind::Type, span, "call arity mismatch"));
            }
            for (i, a) in args.iter().enumerate() {
                let ta = self.infer_expr(a)?;
                let _ = unify(&mut self.dsu, ta, ITy::Known(sig.args[i]))?;
            }
            Ok(ITy::Known(sig.ret_type))
        }
    }

    let mut inf = Infer::new(type_ctx, self_name, params.len(), env_stack, module_alias_exports);
    for (i, (name, ann)) in params.iter().enumerate() {
        inf.bind(name, ITy::Var(inf.param_vars[i]));
        if let Some(t) = ann {
            let tid = inf.resolve_ty_ann(t)?;
            inf.dsu.constrain(inf.param_vars[i], tid)?;
        }
    }
    for s in body {
        inf.infer_stmt(s)?;
    }
    if let Some(t) = tail {
        let tt = inf.infer_expr(t)?;
        let _ = unify(&mut inf.dsu, ITy::Var(inf.ret_var), tt)?;
    }
    inf.check_int_index_constraints()?;

    let arg_tids: Vec<TypeId> = inf.param_vars.iter().map(|&v| inf.dsu.resolve_or_default(v)).collect();
    let ret_tid = inf.dsu.resolve_or_default(inf.ret_var);
    let fun_tid = inf.type_ctx.intern_fun_type(ret_tid, &arg_tids);
    Ok((fun_tid, arg_tids, ret_tid))
}

