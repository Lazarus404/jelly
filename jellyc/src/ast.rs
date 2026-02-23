/**
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize, // byte offset (inclusive)
    pub end: usize,   // byte offset (exclusive)
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn point(at: usize) -> Self {
        Self { start: at, end: at }
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

pub type Ty = Spanned<TyKind>;

#[derive(Clone, Debug)]
pub enum TyKind {
    Named(String),
    Generic { base: String, args: Vec<Ty> },
    Fun { args: Vec<Ty>, ret: Box<Ty> },
    Tuple(Vec<Ty>),
}

pub type Expr = Spanned<ExprKind>;

pub type Pattern = Spanned<PatternKind>;

#[derive(Clone, Debug)]
pub enum PatternKind {
    Wildcard,
    BoolLit(bool),
    I8Lit(i32),
    I16Lit(i32),
    I32Lit(i32),
    /// Pattern binding (introduces a new name in the arm scope).
    Bind(String),
    /// Pin operator: match against an existing name's value (does not bind).
    Pin(String),
    /// Object pattern: `{a: <pat>, b: <pat>, ...}`.
    Obj(Vec<(String, Pattern)>),
    /// Tuple pattern: exact length match `(p0, p1, ...)`.
    TupleExact(Vec<Pattern>),
    /// Array pattern: exact length match `[p0, p1, ...]`.
    ArrayExact(Vec<Pattern>),
    /// Array head/tail match: `[head | rest]`.
    ArrayHeadTail { head: Box<Pattern>, rest: String },
    /// Array prefix/rest match: `[p0, p1, ..., ...rest]`.
    ArrayPrefixRest { prefix: Vec<Pattern>, rest: String },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pat: Pattern,
    pub when: Option<Expr>,
    /// Arm body statements (introduces a scope).
    pub body: Vec<Stmt>,
    /// Optional tail expression. If `None`, the arm falls through to the next arm.
    pub tail: Option<Expr>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    BytesLit(Vec<u8>),
    BoolLit(bool),
    I32Lit(i32),
    I8Lit(i32),
    I16Lit(i32),
    I64Lit(i64),
    F64Lit(f64),
    F16Lit(f32),
    AtomLit(String),
    Null,
    Var(String),
    /// Member access (currently used for namespaced builtins, eg `Bytes.get_u8`).
    Member { base: Box<Expr>, name: String },
    /// Function call; `type_args` are optional generic type parameters (eg `Array.new<I32>(3)`).
    Call {
        callee: Box<Expr>,
        type_args: Vec<Ty>,
        args: Vec<Expr>,
    },
    /// Type application / specialization: `Name<T1, T2>`.
    /// This is compile-time only today (used for template expansion).
    TypeApp { base: Box<Expr>, type_args: Vec<Ty> },
    ArrayLit(Vec<Expr>),
    TupleLit(Vec<Expr>),
    ObjLit(Vec<(String, Expr)>),
    Index { base: Box<Expr>, index: Box<Expr> },
    Fn {
        params: Vec<(String, Option<Ty>)>,
        body: Vec<Stmt>,
        tail: Option<Box<Expr>>,
    },
    /// Truthiness check.
    ///
    /// Semantic normalization can introduce this node to encode:
    /// - falsey = `null` or `false`
    /// - truthy = everything else
    ///
    /// The parser does not produce this directly.
    Truthy(Box<Expr>),
    Not(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    If {
        cond: Box<Expr>,
        then_br: Box<Expr>,
        else_br: Box<Expr>,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Box<Expr>,
    },
    Try {
        body: Box<Expr>,
        catch_name: Option<String>,
        catch_body: Box<Expr>,
    },
    Match {
        subject: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    /// Prototypal instantiation (MVP): `new <proto>(args...)`.
    /// Lowers to: allocate fresh object, attach `__proto__`, optionally call `init(self, ...args)`.
    New {
        proto: Box<Expr>,
        args: Vec<Expr>,
    },
}

pub type Stmt = Spanned<StmtKind>;

#[derive(Clone, Debug)]
pub enum StmtKind {
    Let {
        exported: bool,
        name: String,
        /// Compile-time template parameters: `let Name<T, U> = ...;`
        /// These are expanded (monomorphized) before lowering.
        type_params: Vec<String>,
        ty: Option<Ty>,
        expr: Expr,
    },
    /// Prototype declaration sugar:
    /// `prototype Name<T> { a: expr; b: expr; }`
    /// Desugars (before resolution/lowering) to:
    /// `let Name<T>: Object = {a: expr, b: expr};`
    Prototype {
        exported: bool,
        name: String,
        type_params: Vec<String>,
        fields: Vec<(String, Expr)>,
    },
    ImportModule { path: Vec<String>, alias: String },
    ImportFrom {
        /// If true, this imports types only (reserved; user-defined types not implemented yet).
        type_only: bool,
        items: Vec<(String, Option<String>)>, // (name, alias)
        from: Vec<String>,                    // dotted module path
    },
    Assign { name: String, expr: Expr },
    MemberAssign { base: Expr, name: String, expr: Expr },
    IndexAssign { base: Expr, index: Expr, expr: Expr },
    While { cond: Expr, body: Vec<Stmt> },
    Break,
    Continue,
    Throw { expr: Expr },
    Return { expr: Option<Expr> },
    /// Expression statement: `expr;` (value discarded).
    Expr { expr: Expr },
}

#[derive(Clone, Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    pub expr: Expr,
}

