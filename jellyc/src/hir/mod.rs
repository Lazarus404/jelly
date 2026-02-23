use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, MatchArm, Program, Span, Stmt, StmtKind};
use crate::ir::TypeId;
use crate::typectx::TypeCtx;
use crate::typectx::{
    T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8,
    T_LIST_BYTES, T_LIST_I32, T_OBJECT,
};

#[derive(Clone, Debug)]
pub struct HirProgram {
    /// For now, HIR is represented as an AST-shaped tree with semantic-normalized nodes
    /// (eg. `ExprKind::Truthy`), plus side tables in `SemanticInfo`.
    pub program: Program,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub Span);

#[derive(Clone, Debug)]
pub struct SemanticInfo {
    pub expr_types: HashMap<NodeId, TypeId>,
    pub binding_types: HashMap<NodeId, TypeId>,
    pub captures: HashMap<NodeId, Vec<Capture>>,
    pub type_ctx: TypeCtx,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Capture {
    pub name: String,
    pub tid: TypeId,
}

impl Default for SemanticInfo {
    fn default() -> Self {
        Self {
            expr_types: HashMap::new(),
            binding_types: HashMap::new(),
            captures: HashMap::new(),
            type_ctx: TypeCtx::new_program_base(),
        }
    }
}

pub fn type_name(tid: TypeId, tc: &TypeCtx) -> String {
    if let Some(te) = tc.types.get(tid as usize) {
        match te.kind {
            crate::jlyb::TypeKind::Function => {
                if let Some(sig) = tc.sigs.get(te.p0 as usize) {
                    let mut out = "(".to_string();
                    for (i, a) in sig.args.iter().enumerate() {
                        if i != 0 {
                            out.push_str(", ");
                        }
                        out.push_str(&type_name(*a, tc));
                    }
                    out.push_str(") -> ");
                    out.push_str(&type_name(sig.ret_type, tc));
                    return out;
                }
            }
            crate::jlyb::TypeKind::Object => {
                const TUPLE_TAG: u32 = 0x8000_0000;
                if (te.p0 & TUPLE_TAG) != 0 {
                    let sig_id = te.p0 & !TUPLE_TAG;
                    if let Some(sig) = tc.sigs.get(sig_id as usize) {
                        let mut out = "Tuple<".to_string();
                        for (i, a) in sig.args.iter().enumerate() {
                            if i != 0 {
                                out.push_str(", ");
                            }
                            out.push_str(&type_name(*a, tc));
                        }
                        out.push('>');
                        return out;
                    }
                }
            }
            _ => {}
        }
    }
    match tid {
        T_BOOL => "Bool".to_string(),
        T_ATOM => "Atom".to_string(),
        T_I8 => "I8".to_string(),
        T_I16 => "I16".to_string(),
        T_I32 => "I32".to_string(),
        T_I64 => "I64".to_string(),
        T_F16 => "F16".to_string(),
        T_F32 => "F32".to_string(),
        T_F64 => "F64".to_string(),
        T_BYTES => "Bytes".to_string(),
        T_DYNAMIC => "Dynamic".to_string(),
        T_OBJECT => "Object".to_string(),
        T_ARRAY_I32 => "Array<I32>".to_string(),
        T_ARRAY_BYTES => "Array<Bytes>".to_string(),
        T_LIST_I32 => "List<I32>".to_string(),
        T_LIST_BYTES => "List<Bytes>".to_string(),
        _ => format!("t{tid}"),
    }
}

pub fn render_hir(p: &HirProgram, info: &SemanticInfo) -> String {
    let mut out = String::new();
    out.push_str("hir {\n");
    for s in &p.program.stmts {
        render_stmt(s, info, 1, &mut out);
    }
    out.push_str("  expr:\n");
    render_expr(&p.program.expr, info, 2, &mut out);
    out.push_str("}\n");
    out
}

fn render_stmt(s: &Stmt, info: &SemanticInfo, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    let span = s.span;
    let bind_t = info.binding_types.get(&NodeId(span)).copied();
    match &s.node {
        StmtKind::Let { exported, name, ty, expr, .. } => {
            out.push_str(&format!(
                "{pad}let{} {}{} @{}..{}{}\n",
                if *exported { " export" } else { "" },
                name,
                ty.as_ref()
                    .map(|t| format!(": {:?}", t.node))
                    .unwrap_or_default(),
                span.start,
                span.end,
                bind_t
                    .map(|t| format!(" : {}", type_name(t, &info.type_ctx)))
                    .unwrap_or_default()
            ));
            render_expr(expr, info, indent + 1, out);
        }
        StmtKind::Assign { name, expr } => {
            out.push_str(&format!("{pad}assign {name} @{}..{}\n", span.start, span.end));
            render_expr(expr, info, indent + 1, out);
        }
        StmtKind::Expr { expr } => {
            out.push_str(&format!("{pad}stmt_expr @{}..{}\n", span.start, span.end));
            render_expr(expr, info, indent + 1, out);
        }
        StmtKind::While { cond, body } => {
            out.push_str(&format!("{pad}while @{}..{}\n", span.start, span.end));
            out.push_str(&format!("{pad}  cond:\n"));
            render_expr(cond, info, indent + 2, out);
            out.push_str(&format!("{pad}  body:\n"));
            for st in body {
                render_stmt(st, info, indent + 2, out);
            }
        }
        StmtKind::Return { expr } => {
            out.push_str(&format!("{pad}return @{}..{}\n", span.start, span.end));
            if let Some(e) = expr {
                render_expr(e, info, indent + 1, out);
            }
        }
        StmtKind::Break => out.push_str(&format!("{pad}break @{}..{}\n", span.start, span.end)),
        StmtKind::Continue => out.push_str(&format!("{pad}continue @{}..{}\n", span.start, span.end)),
        StmtKind::Throw { expr } => {
            out.push_str(&format!("{pad}throw @{}..{}\n", span.start, span.end));
            render_expr(expr, info, indent + 1, out);
        }
        StmtKind::MemberAssign { base, name, expr } => {
            out.push_str(&format!("{pad}member_assign .{name} @{}..{}\n", span.start, span.end));
            render_expr(base, info, indent + 1, out);
            render_expr(expr, info, indent + 1, out);
        }
        StmtKind::IndexAssign { base, index, expr } => {
            out.push_str(&format!("{pad}index_assign @{}..{}\n", span.start, span.end));
            render_expr(base, info, indent + 1, out);
            render_expr(index, info, indent + 1, out);
            render_expr(expr, info, indent + 1, out);
        }
        StmtKind::ImportModule { path, alias } => {
            out.push_str(&format!("{pad}import {} as {alias} @{}..{}\n", path.join("."), span.start, span.end));
        }
        StmtKind::ImportFrom { from, items, .. } => {
            out.push_str(&format!("{pad}import {{...}} from {} @{}..{}\n", from.join("."), span.start, span.end));
            for (n, a) in items {
                out.push_str(&format!("{pad}  item {}{}\n", n, a.as_ref().map(|x| format!(" as {x}")).unwrap_or_default()));
            }
        }
        StmtKind::Prototype { .. } => out.push_str(&format!("{pad}prototype @{}..{}\n", span.start, span.end)),
    }
}

fn render_expr(e: &Expr, info: &SemanticInfo, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    let span = e.span;
    let ty = info.expr_types.get(&NodeId(span)).copied();
    let ty_s = ty
        .map(|t| type_name(t, &info.type_ctx))
        .unwrap_or_else(|| "?".to_string());

    match &e.node {
        ExprKind::BytesLit(_) => out.push_str(&format!("{pad}BytesLit : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::BoolLit(v) => out.push_str(&format!("{pad}BoolLit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::I32Lit(v) => out.push_str(&format!("{pad}I32Lit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::I8Lit(v) => out.push_str(&format!("{pad}I8Lit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::I16Lit(v) => out.push_str(&format!("{pad}I16Lit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::I64Lit(v) => out.push_str(&format!("{pad}I64Lit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::F16Lit(v) => out.push_str(&format!("{pad}F16Lit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::F64Lit(v) => out.push_str(&format!("{pad}F64Lit({v}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::AtomLit(n) => out.push_str(&format!("{pad}AtomLit({n}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::Null => out.push_str(&format!("{pad}Null : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::Var(n) => out.push_str(&format!("{pad}Var({n}) : {ty_s} @{}..{}\n", span.start, span.end)),
        ExprKind::Member { base, name } => {
            out.push_str(&format!("{pad}Member(.{name}) : {ty_s} @{}..{}\n", span.start, span.end));
            render_expr(base, info, indent + 1, out);
        }
        ExprKind::Call { callee, type_args, args } => {
            out.push_str(&format!(
                "{pad}Call{} : {ty_s} @{}..{}\n",
                if type_args.is_empty() { "".to_string() } else { format!("<{}>", type_args.len()) },
                span.start,
                span.end
            ));
            render_expr(callee, info, indent + 1, out);
            for a in args {
                render_expr(a, info, indent + 1, out);
            }
        }
        ExprKind::TypeApp { base, type_args } => {
            out.push_str(&format!("{pad}TypeApp<{}> : {ty_s} @{}..{}\n", type_args.len(), span.start, span.end));
            render_expr(base, info, indent + 1, out);
        }
        ExprKind::ArrayLit(elems) => {
            out.push_str(&format!("{pad}ArrayLit({}) : {ty_s} @{}..{}\n", elems.len(), span.start, span.end));
            for el in elems {
                render_expr(el, info, indent + 1, out);
            }
        }
        ExprKind::TupleLit(elems) => {
            out.push_str(&format!("{pad}TupleLit({}) : {ty_s} @{}..{}\n", elems.len(), span.start, span.end));
            for el in elems {
                render_expr(el, info, indent + 1, out);
            }
        }
        ExprKind::ObjLit(fields) => {
            out.push_str(&format!("{pad}ObjLit({}) : {ty_s} @{}..{}\n", fields.len(), span.start, span.end));
            for (k, v) in fields {
                out.push_str(&format!("{pad}  field {k}:\n"));
                render_expr(v, info, indent + 2, out);
            }
        }
        ExprKind::Index { base, index } => {
            out.push_str(&format!("{pad}Index : {ty_s} @{}..{}\n", span.start, span.end));
            render_expr(base, info, indent + 1, out);
            render_expr(index, info, indent + 1, out);
        }
        ExprKind::Fn { params, body, tail } => {
            out.push_str(&format!("{pad}Fn(params={}) : {ty_s} @{}..{}\n", params.len(), span.start, span.end));
            if let Some(caps) = info.captures.get(&NodeId(span)) {
                if !caps.is_empty() {
                    out.push_str(&format!("{pad}  captures:\n"));
                    for c in caps {
                        out.push_str(&format!(
                            "{pad}    {} : {}\n",
                            c.name,
                            type_name(c.tid, &info.type_ctx)
                        ));
                    }
                }
            }
            for st in body {
                render_stmt(st, info, indent + 1, out);
            }
            if let Some(t) = tail {
                out.push_str(&format!("{pad}  tail:\n"));
                render_expr(t, info, indent + 2, out);
            }
        }
        ExprKind::Truthy(x) => {
            out.push_str(&format!("{pad}Truthy : {ty_s} @{}..{}\n", span.start, span.end));
            render_expr(x, info, indent + 1, out);
        }
        ExprKind::Not(x) => {
            out.push_str(&format!("{pad}Not : {ty_s} @{}..{}\n", span.start, span.end));
            render_expr(x, info, indent + 1, out);
        }
        ExprKind::Neg(x) => {
            out.push_str(&format!("{pad}Neg : {ty_s} @{}..{}\n", span.start, span.end));
            render_expr(x, info, indent + 1, out);
        }
        ExprKind::Add(a, b) => render_bin("Add", a, b, span, ty_s, info, indent, out),
        ExprKind::Sub(a, b) => render_bin("Sub", a, b, span, ty_s, info, indent, out),
        ExprKind::Mul(a, b) => render_bin("Mul", a, b, span, ty_s, info, indent, out),
        ExprKind::Div(a, b) => render_bin("Div", a, b, span, ty_s, info, indent, out),
        ExprKind::Eq(a, b) => render_bin("Eq", a, b, span, ty_s, info, indent, out),
        ExprKind::Ne(a, b) => render_bin("Ne", a, b, span, ty_s, info, indent, out),
        ExprKind::Lt(a, b) => render_bin("Lt", a, b, span, ty_s, info, indent, out),
        ExprKind::Le(a, b) => render_bin("Le", a, b, span, ty_s, info, indent, out),
        ExprKind::Gt(a, b) => render_bin("Gt", a, b, span, ty_s, info, indent, out),
        ExprKind::Ge(a, b) => render_bin("Ge", a, b, span, ty_s, info, indent, out),
        ExprKind::And(a, b) => render_bin("And", a, b, span, ty_s, info, indent, out),
        ExprKind::Or(a, b) => render_bin("Or", a, b, span, ty_s, info, indent, out),
        ExprKind::If { cond, then_br, else_br } => {
            out.push_str(&format!("{pad}If : {ty_s} @{}..{}\n", span.start, span.end));
            out.push_str(&format!("{pad}  cond:\n"));
            render_expr(cond, info, indent + 2, out);
            out.push_str(&format!("{pad}  then:\n"));
            render_expr(then_br, info, indent + 2, out);
            out.push_str(&format!("{pad}  else:\n"));
            render_expr(else_br, info, indent + 2, out);
        }
        ExprKind::Block { stmts, expr } => {
            out.push_str(&format!("{pad}Block : {ty_s} @{}..{}\n", span.start, span.end));
            for st in stmts {
                render_stmt(st, info, indent + 1, out);
            }
            render_expr(expr, info, indent + 1, out);
        }
        ExprKind::Try { body, catch_name, catch_body } => {
            out.push_str(&format!("{pad}Try(catch={}) : {ty_s} @{}..{}\n", catch_name.as_deref().unwrap_or("_"), span.start, span.end));
            render_expr(body, info, indent + 1, out);
            render_expr(catch_body, info, indent + 1, out);
        }
        ExprKind::Match { subject, arms } => {
            out.push_str(&format!("{pad}Match(arms={}) : {ty_s} @{}..{}\n", arms.len(), span.start, span.end));
            render_expr(subject, info, indent + 1, out);
            for a in arms {
                render_arm(a, info, indent + 1, out);
            }
        }
        ExprKind::New { proto, args } => {
            out.push_str(&format!("{pad}New(args={}) : {ty_s} @{}..{}\n", args.len(), span.start, span.end));
            render_expr(proto, info, indent + 1, out);
            for a in args {
                render_expr(a, info, indent + 1, out);
            }
        }
    }
}

fn render_arm(a: &MatchArm, info: &SemanticInfo, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!("{pad}arm:\n"));
    if let Some(w) = &a.when {
        out.push_str(&format!("{pad}  when:\n"));
        render_expr(w, info, indent + 2, out);
    }
    for st in &a.body {
        render_stmt(st, info, indent + 1, out);
    }
    if let Some(t) = &a.tail {
        out.push_str(&format!("{pad}  tail:\n"));
        render_expr(t, info, indent + 2, out);
    }
}

fn render_bin(
    op: &str,
    a: &Expr,
    b: &Expr,
    span: Span,
    ty_s: String,
    info: &SemanticInfo,
    indent: usize,
    out: &mut String,
) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!("{pad}{op} : {ty_s} @{}..{}\n", span.start, span.end));
    render_expr(a, info, indent + 1, out);
    render_expr(b, info, indent + 1, out);
}

