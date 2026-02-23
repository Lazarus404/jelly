use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Program, Span, Stmt, StmtKind};
use crate::error::CompileError;
use crate::hir::{Capture, HirProgram, NodeId, SemanticInfo};
use crate::typectx::TypeCtx;
use crate::typectx::{T_DYNAMIC, T_OBJECT};

mod fn_infer;
mod const_eval;
mod typecheck;

pub fn analyze_program(p: &Program) -> Result<(HirProgram, SemanticInfo), CompileError> {
    let mut prog = p.clone();
    normalize_truthiness_program(&mut prog);

    let mut info = typecheck::typecheck_program(&prog)?;
    info.captures = capture_analysis(&prog, &info);

    Ok((HirProgram { program: prog }, info))
}

pub fn analyze_module_init(
    module_name: &str,
    p: &Program,
    is_entry: bool,
    import_exports: &HashMap<String, HashMap<String, crate::typectx::TypeRepr>>,
) -> Result<(HirProgram, SemanticInfo), CompileError> {
    let mut prog = p.clone();
    normalize_truthiness_program(&mut prog);

    // module_name/is_entry are reserved for future semantic differences (entry program constraints).
    let _ = module_name;
    let _ = is_entry;
    let mut info = typecheck::typecheck_module_init(&prog, import_exports)?;
    info.captures = capture_analysis(&prog, &info);

    Ok((HirProgram { program: prog }, info))
}

fn capture_analysis(p: &Program, info: &SemanticInfo) -> HashMap<NodeId, Vec<Capture>> {
    #[derive(Clone)]
    struct Analyzer<'a> {
        info: &'a SemanticInfo,
        scopes: Vec<HashMap<String, u32>>,
        outer_scopes_stack: Vec<Vec<HashMap<String, u32>>>,
        fn_span_stack: Vec<Span>,
        out: HashMap<NodeId, Vec<Capture>>,
    }

    impl<'a> Analyzer<'a> {
        fn new(info: &'a SemanticInfo) -> Self {
            Self {
                info,
                scopes: vec![HashMap::new()],
                outer_scopes_stack: Vec::new(),
                fn_span_stack: Vec::new(),
                out: HashMap::new(),
            }
        }

        fn lookup_local(&self, name: &str) -> Option<u32> {
            self.scopes.iter().rev().find_map(|s| s.get(name)).copied()
        }

        fn lookup_outer(&self, name: &str) -> Option<u32> {
            self.outer_scopes_stack
                .last()
                .and_then(|outer| outer.iter().rev().find_map(|s| s.get(name)).copied())
        }

        fn bind(&mut self, name: &str, tid: u32) {
            self.scopes.last_mut().expect("scopes").insert(name.to_string(), tid);
        }

        fn push_scope(&mut self) {
            self.scopes.push(HashMap::new());
        }

        fn pop_scope(&mut self) {
            self.scopes.pop();
            if self.scopes.is_empty() {
                self.scopes.push(HashMap::new());
            }
        }

        fn analyze_program(mut self, p: &Program) -> HashMap<NodeId, Vec<Capture>> {
            for s in &p.stmts {
                self.analyze_stmt(s);
            }
            self.analyze_expr(&p.expr);
            self.out
        }

        fn analyze_stmt(&mut self, s: &Stmt) {
            match &s.node {
                StmtKind::ImportModule { alias, .. } => {
                    self.bind(alias, T_OBJECT);
                }
                StmtKind::ImportFrom { items, .. } => {
                    for (name, alias) in items {
                        let bind = alias.as_deref().unwrap_or(name.as_str());
                        // We don't have a first-class type environment here yet; default to Dynamic.
                        self.bind(bind, T_DYNAMIC);
                    }
                }
                StmtKind::Let { name, expr, .. } => {
                    // Mirror resolver behavior: allow recursion sugar by binding the name
                    // before analyzing the fn literal initializer.
                    let is_discard = name == "_";
                    if !is_discard && matches!(&expr.node, ExprKind::Fn { .. }) {
                        let tid = self
                            .info
                            .binding_types
                            .get(&NodeId(s.span))
                            .copied()
                            .unwrap_or(T_DYNAMIC);
                        self.bind(name, tid);
                        self.analyze_expr(expr);
                    } else {
                        self.analyze_expr(expr);
                        if !is_discard {
                            let tid = self
                                .info
                                .binding_types
                                .get(&NodeId(s.span))
                                .copied()
                                .unwrap_or(T_DYNAMIC);
                            self.bind(name, tid);
                        }
                    }
                }
                StmtKind::Assign { expr, .. } => self.analyze_expr(expr),
                StmtKind::MemberAssign { base, expr, .. } => {
                    self.analyze_expr(base);
                    self.analyze_expr(expr);
                }
                StmtKind::IndexAssign { base, index, expr } => {
                    self.analyze_expr(base);
                    self.analyze_expr(index);
                    self.analyze_expr(expr);
                }
                StmtKind::While { cond, body } => {
                    self.analyze_expr(cond);
                    self.push_scope();
                    for st in body {
                        self.analyze_stmt(st);
                    }
                    self.pop_scope();
                }
                StmtKind::Throw { expr } => self.analyze_expr(expr),
                StmtKind::Return { expr } => {
                    if let Some(e) = expr {
                        self.analyze_expr(e);
                    }
                }
                StmtKind::Expr { expr } => self.analyze_expr(expr),
                StmtKind::Prototype { .. } | StmtKind::Break | StmtKind::Continue => {}
            }
        }

        fn analyze_expr(&mut self, e: &Expr) {
            match &e.node {
                ExprKind::Var(n) => {
                    if self.lookup_local(n).is_some() {
                        return;
                    }
                    let Some(cur_fn) = self.fn_span_stack.last().copied() else {
                        return;
                    };
                    let Some(tid) = self.lookup_outer(n) else {
                        return;
                    };
                    let caps = self.out.entry(NodeId(cur_fn)).or_default();
                    if !caps.iter().any(|c| c.name == *n) {
                        caps.push(Capture {
                            name: n.clone(),
                            tid,
                        });
                    }
                }
                ExprKind::Member { base, .. } => self.analyze_expr(base),
                ExprKind::Call { callee, args, .. } => {
                    self.analyze_expr(callee);
                    for a in args {
                        self.analyze_expr(a);
                    }
                }
                ExprKind::TypeApp { base, .. } => self.analyze_expr(base),
                ExprKind::ArrayLit(elems) | ExprKind::TupleLit(elems) => {
                    for x in elems {
                        self.analyze_expr(x);
                    }
                }
                ExprKind::ObjLit(fields) => {
                    for (_, v) in fields {
                        self.analyze_expr(v);
                    }
                }
                ExprKind::Index { base, index } => {
                    self.analyze_expr(base);
                    self.analyze_expr(index);
                }
                ExprKind::Fn { params, body, tail } => {
                    self.fn_span_stack.push(e.span);
                    let saved_scopes = std::mem::take(&mut self.scopes);
                    self.outer_scopes_stack.push(saved_scopes.clone());
                    self.scopes = vec![HashMap::new()];

                    // Bind params in local scope (use annotations if present, else Dynamic).
                    let mut tc = TypeCtx::new_program_base();
                    for (pn, pty) in params {
                        let tid = pty.as_ref().and_then(|t| tc.resolve_ty(t).ok()).unwrap_or(T_DYNAMIC);
                        self.bind(pn, tid);
                    }

                    // Establish capture list for this fn literal.
                    self.out.entry(NodeId(e.span)).or_default();
                    for st in body {
                        self.analyze_stmt(st);
                    }
                    if let Some(t) = tail {
                        self.analyze_expr(t);
                    }

                    self.scopes = saved_scopes;
                    self.outer_scopes_stack.pop();
                    self.fn_span_stack.pop();
                }
                ExprKind::Truthy(x) | ExprKind::Not(x) | ExprKind::Neg(x) => self.analyze_expr(x),
                ExprKind::Add(a, b)
                | ExprKind::Sub(a, b)
                | ExprKind::Mul(a, b)
                | ExprKind::Div(a, b)
                | ExprKind::Eq(a, b)
                | ExprKind::Ne(a, b)
                | ExprKind::Lt(a, b)
                | ExprKind::Le(a, b)
                | ExprKind::Gt(a, b)
                | ExprKind::Ge(a, b)
                | ExprKind::And(a, b)
                | ExprKind::Or(a, b) => {
                    self.analyze_expr(a);
                    self.analyze_expr(b);
                }
                ExprKind::If { cond, then_br, else_br } => {
                    self.analyze_expr(cond);
                    self.analyze_expr(then_br);
                    self.analyze_expr(else_br);
                }
                ExprKind::Block { stmts, expr } => {
                    self.push_scope();
                    for st in stmts {
                        self.analyze_stmt(st);
                    }
                    self.analyze_expr(expr);
                    self.pop_scope();
                }
                ExprKind::Try { body, catch_name, catch_body } => {
                    self.analyze_expr(body);
                    self.push_scope();
                    if let Some(n) = catch_name {
                        self.bind(n, T_DYNAMIC);
                    }
                    self.analyze_expr(catch_body);
                    self.pop_scope();
                }
                ExprKind::Match { subject, arms } => {
                    self.analyze_expr(subject);
                    for a in arms {
                        self.push_scope();
                        // Pattern bindings aren't typed yet; bind as Dynamic so nested fn capture analysis sees them.
                        self.bind_pattern(&a.pat);
                        if let Some(w) = &a.when {
                            self.analyze_expr(w);
                        }
                        for st in &a.body {
                            self.analyze_stmt(st);
                        }
                        if let Some(t) = &a.tail {
                            self.analyze_expr(t);
                        }
                        self.pop_scope();
                    }
                }
                ExprKind::New { proto, args } => {
                    self.analyze_expr(proto);
                    for a in args {
                        self.analyze_expr(a);
                    }
                }
                ExprKind::BytesLit(_)
                | ExprKind::BoolLit(_)
                | ExprKind::I32Lit(_)
                | ExprKind::I8Lit(_)
                | ExprKind::I16Lit(_)
                | ExprKind::I64Lit(_)
                | ExprKind::F64Lit(_)
                | ExprKind::F16Lit(_)
                | ExprKind::AtomLit(_)
                | ExprKind::Null => {}
            }
        }

        fn bind_pattern(&mut self, p: &crate::ast::Pattern) {
            use crate::ast::PatternKind;
            match &p.node {
                PatternKind::Bind(n) => self.bind(n, T_DYNAMIC),
                PatternKind::Obj(fields) => {
                    for (_k, v) in fields {
                        self.bind_pattern(v);
                    }
                }
                PatternKind::TupleExact(elems) | PatternKind::ArrayExact(elems) => {
                    for el in elems {
                        self.bind_pattern(el);
                    }
                }
                PatternKind::ArrayHeadTail { head, rest } => {
                    self.bind_pattern(head);
                    self.bind(rest, T_DYNAMIC);
                }
                PatternKind::ArrayPrefixRest { prefix, rest } => {
                    for el in prefix {
                        self.bind_pattern(el);
                    }
                    self.bind(rest, T_DYNAMIC);
                }
                PatternKind::Wildcard
                | PatternKind::BoolLit(_)
                | PatternKind::I8Lit(_)
                | PatternKind::I16Lit(_)
                | PatternKind::I32Lit(_)
                | PatternKind::Pin(_) => {}
            }
        }

    }

    Analyzer::new(info).analyze_program(p)
}

fn normalize_truthiness_program(p: &mut Program) {
    for s in &mut p.stmts {
        normalize_truthiness_stmt(s);
    }
    p.expr = normalize_truthiness_expr(&p.expr);
}

fn normalize_truthiness_stmt(s: &mut Stmt) {
    match &mut s.node {
        StmtKind::Let { expr, .. } => {
            *expr = normalize_truthiness_expr(expr);
        }
        StmtKind::Assign { expr, .. } => {
            *expr = normalize_truthiness_expr(expr);
        }
        StmtKind::MemberAssign { base, expr, .. } => {
            *base = normalize_truthiness_expr(base);
            *expr = normalize_truthiness_expr(expr);
        }
        StmtKind::IndexAssign { base, index, expr } => {
            *base = normalize_truthiness_expr(base);
            *index = normalize_truthiness_expr(index);
            *expr = normalize_truthiness_expr(expr);
        }
        StmtKind::While { cond, body } => {
            *cond = normalize_truthiness_expr(cond);
            for st in body {
                normalize_truthiness_stmt(st);
            }
        }
        StmtKind::Throw { expr } => *expr = normalize_truthiness_expr(expr),
        StmtKind::Return { expr } => {
            if let Some(e) = expr {
                *e = normalize_truthiness_expr(e);
            }
        }
        StmtKind::Expr { expr } => *expr = normalize_truthiness_expr(expr),
        StmtKind::ImportModule { .. }
        | StmtKind::ImportFrom { .. }
        | StmtKind::Prototype { .. }
        | StmtKind::Break
        | StmtKind::Continue => {}
    }
}

fn normalize_truthiness_expr(e: &Expr) -> Expr {
    let span = e.span;
    let node = match &e.node {
        ExprKind::BytesLit(b) => ExprKind::BytesLit(b.clone()),
        ExprKind::BoolLit(x) => ExprKind::BoolLit(*x),
        ExprKind::I32Lit(x) => ExprKind::I32Lit(*x),
        ExprKind::I8Lit(x) => ExprKind::I8Lit(*x),
        ExprKind::I16Lit(x) => ExprKind::I16Lit(*x),
        ExprKind::I64Lit(x) => ExprKind::I64Lit(*x),
        ExprKind::F16Lit(x) => ExprKind::F16Lit(*x),
        ExprKind::F64Lit(x) => ExprKind::F64Lit(*x),
        ExprKind::AtomLit(n) => ExprKind::AtomLit(n.clone()),
        ExprKind::Null => ExprKind::Null,
        ExprKind::Var(n) => ExprKind::Var(n.clone()),
        ExprKind::Member { base, name } => ExprKind::Member {
            base: Box::new(normalize_truthiness_expr(base)),
            name: name.clone(),
        },
        ExprKind::Call { callee, type_args, args } => ExprKind::Call {
            callee: Box::new(normalize_truthiness_expr(callee)),
            type_args: type_args.clone(),
            args: args.iter().map(normalize_truthiness_expr).collect(),
        },
        ExprKind::TypeApp { base, type_args } => ExprKind::TypeApp {
            base: Box::new(normalize_truthiness_expr(base)),
            type_args: type_args.clone(),
        },
        ExprKind::ArrayLit(elems) => ExprKind::ArrayLit(elems.iter().map(normalize_truthiness_expr).collect()),
        ExprKind::TupleLit(elems) => ExprKind::TupleLit(elems.iter().map(normalize_truthiness_expr).collect()),
        ExprKind::ObjLit(fields) => ExprKind::ObjLit(
            fields
                .iter()
                .map(|(k, v)| (k.clone(), normalize_truthiness_expr(v)))
                .collect(),
        ),
        ExprKind::Index { base, index } => ExprKind::Index {
            base: Box::new(normalize_truthiness_expr(base)),
            index: Box::new(normalize_truthiness_expr(index)),
        },
        ExprKind::Fn { params, body, tail } => ExprKind::Fn {
            params: params.clone(),
            body: body
                .iter()
                .map(|st| {
                    let mut st2 = st.clone();
                    normalize_truthiness_stmt(&mut st2);
                    st2
                })
                .collect(),
            tail: tail.as_ref().map(|t| Box::new(normalize_truthiness_expr(t))),
        },
        ExprKind::Truthy(x) => ExprKind::Truthy(Box::new(normalize_truthiness_expr(x))),
        ExprKind::Not(x) => ExprKind::Not(Box::new(normalize_truthiness_expr(x))),
        ExprKind::Neg(x) => ExprKind::Neg(Box::new(normalize_truthiness_expr(x))),
        ExprKind::Add(a, b) => ExprKind::Add(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Sub(a, b) => ExprKind::Sub(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Mul(a, b) => ExprKind::Mul(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Div(a, b) => ExprKind::Div(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Eq(a, b) => normalize_truthy_cmp(span, true, a, b),
        ExprKind::Ne(a, b) => normalize_truthy_cmp(span, false, a, b),
        ExprKind::Lt(a, b) => ExprKind::Lt(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Le(a, b) => ExprKind::Le(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Gt(a, b) => ExprKind::Gt(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Ge(a, b) => ExprKind::Ge(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::And(a, b) => ExprKind::And(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::Or(a, b) => ExprKind::Or(
            Box::new(normalize_truthiness_expr(a)),
            Box::new(normalize_truthiness_expr(b)),
        ),
        ExprKind::If { cond, then_br, else_br } => ExprKind::If {
            cond: Box::new(normalize_truthiness_expr(cond)),
            then_br: Box::new(normalize_truthiness_expr(then_br)),
            else_br: Box::new(normalize_truthiness_expr(else_br)),
        },
        ExprKind::Block { stmts, expr } => {
            let mut out_stmts: Vec<Stmt> = Vec::with_capacity(stmts.len());
            for st in stmts {
                let mut st2 = st.clone();
                normalize_truthiness_stmt(&mut st2);
                out_stmts.push(st2);
            }
            ExprKind::Block {
                stmts: out_stmts,
                expr: Box::new(normalize_truthiness_expr(expr)),
            }
        }
        ExprKind::Try { body, catch_name, catch_body } => ExprKind::Try {
            body: Box::new(normalize_truthiness_expr(body)),
            catch_name: catch_name.clone(),
            catch_body: Box::new(normalize_truthiness_expr(catch_body)),
        },
        ExprKind::Match { subject, arms } => ExprKind::Match {
            subject: Box::new(normalize_truthiness_expr(subject)),
            arms: arms
                .iter()
                .map(|a| {
                    let mut a2 = a.clone();
                    if let Some(w) = &a2.when {
                        a2.when = Some(normalize_truthiness_expr(w));
                    }
                    for st in &mut a2.body {
                        normalize_truthiness_stmt(st);
                    }
                    if let Some(t) = &a2.tail {
                        a2.tail = Some(normalize_truthiness_expr(t));
                    }
                    a2
                })
                .collect(),
        },
        ExprKind::New { proto, args } => ExprKind::New {
            proto: Box::new(normalize_truthiness_expr(proto)),
            args: args.iter().map(normalize_truthiness_expr).collect(),
        },
    };
    Expr::new(node, span)
}

fn normalize_truthy_cmp(span: Span, is_eq: bool, a: &Expr, b: &Expr) -> ExprKind {
    let a2 = normalize_truthiness_expr(a);
    let b2 = normalize_truthiness_expr(b);

    // Rewrite comparisons against boolean literals into truthiness checks.
    //
    // - x == true  => Truthy(x)
    // - x == false => !Truthy(x)
    // - x != true  => !Truthy(x)
    // - x != false => Truthy(x)
    let mk = |bv: bool, other: Expr| -> ExprKind {
        let truthy = Expr::new(ExprKind::Truthy(Box::new(other)), span);
        if (is_eq && bv) || (!is_eq && !bv) {
            truthy.node
        } else {
            ExprKind::Not(Box::new(truthy))
        }
    };

    if let ExprKind::BoolLit(bv) = a2.node {
        return mk(bv, b2);
    }
    if let ExprKind::BoolLit(bv) = b2.node {
        return mk(bv, a2);
    }

    if is_eq {
        ExprKind::Eq(Box::new(a2), Box::new(b2))
    } else {
        ExprKind::Ne(Box::new(a2), Box::new(b2))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_eq_true_to_truthy() {
        let src = "let x = 1; if (x == true) { \"ok\" } else { \"bad\" }";
        let mut p = crate::parse::parse_program(src).unwrap();
        crate::templates::expand_templates(&mut p).unwrap();
        crate::resolve::resolve_program(&p).unwrap();
        let (hir, _info) = analyze_program(&p).unwrap();
        let dumped = crate::hir::render_hir(&hir, &SemanticInfo::default());
        assert!(dumped.contains("Truthy"), "dumped:\n{dumped}");
    }
}

