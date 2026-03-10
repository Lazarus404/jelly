/*
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

use std::collections::HashMap;

use super::*;
use crate::ast::{ExprKind, Program, Span, Spanned, StmtKind, TyKind};
use crate::typectx::TypeRepr;

#[test]
fn lower_module_named_imports_binds_locals() {
    let mut i: usize = 0;
    let mut sp = || {
        let s = Span::new(i, i + 1);
        i += 1;
        s
    };
    let import_add = Spanned::new(
        StmtKind::ImportFrom {
            type_only: false,
            items: vec![("add".to_string(), None)],
            from: vec!["math".to_string()],
        },
        sp(),
    );
    let import_y = Spanned::new(
        StmtKind::ImportFrom {
            type_only: false,
            items: vec![("x".to_string(), Some("y".to_string()))],
            from: vec!["consts".to_string()],
        },
        sp(),
    );
    let call_add = Spanned::new(
        ExprKind::Call {
            callee: Box::new(Spanned::new(ExprKind::Var("add".to_string()), sp())),
            type_args: vec![],
            args: vec![
                Spanned::new(ExprKind::Var("y".to_string()), sp()),
                Spanned::new(ExprKind::I32Lit(3), sp()),
            ],
        },
        sp(),
    );
    let let_r = Spanned::new(
        StmtKind::Let {
            is_const: false,
            exported: false,
            name: "r".to_string(),
            type_params: Vec::new(),
            ty: Some(Spanned::new(TyKind::Named("I32".to_string()), sp())),
            expr: call_add,
        },
        sp(),
    );
    let mut prog = Program {
        stmts: vec![import_add, import_y, let_r],
        expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), sp()),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();

    let mut import_exports: HashMap<String, HashMap<String, TypeRepr>> = HashMap::new();
    import_exports.insert(
        "math".to_string(),
        HashMap::from([(
            "add".to_string(),
            TypeRepr::Fun {
                args: vec![TypeRepr::I32, TypeRepr::I32],
                ret: Box::new(TypeRepr::I32),
            },
        )]),
    );
    import_exports.insert(
        "consts".to_string(),
        HashMap::from([("x".to_string(), TypeRepr::I32)]),
    );

    let (hir, info) =
        crate::semantic::analyze_prepared_module_init("entry", prepared, true, false, &import_exports)
            .unwrap();
    let lowered =
        lower_module_init_to_ir("entry", &hir.program, &info, true, false, &import_exports).unwrap();
    assert!(lowered.ir.funcs.len() >= 1);
}

#[test]
fn lower_minimal_bytes_program() {
    let mut prog = Program {
        stmts: vec![],
        expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(0, 2)),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let m = lower_program_to_ir(&hir.program, &info).unwrap();
    assert_eq!(m.entry, 0);
    assert_eq!(m.funcs.len(), 1);
    assert_eq!(m.const_bytes.len(), 1);
}

#[test]
fn lower_if_expression_builds_blocks_and_join() {
    let if_sp = Span::new(0, 1);
    let cond_sp = Span::new(2, 3);
    let then_sp = Span::new(4, 5);
    let else_sp = Span::new(6, 7);
    let mut prog = Program {
        stmts: vec![],
        expr: Spanned::new(
            ExprKind::If {
                cond: Box::new(Spanned::new(ExprKind::BoolLit(true), cond_sp)),
                then_br: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), then_sp)),
                else_br: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), else_sp)),
            },
            if_sp,
        ),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let m = lower_program_to_ir(&hir.program, &info).unwrap();
    let f = &m.funcs[0];
    assert!(f.blocks.len() >= 4, "expected entry+then+else+join");
}

#[test]
fn lower_while_statement_builds_cond_body_exit_blocks() {
    let sp = Span::new(0, 1);
    let mut prog = Program {
        stmts: vec![Spanned::new(
            StmtKind::While {
                cond: Spanned::new(ExprKind::BoolLit(false), sp),
                body: vec![],
            },
            sp,
        )],
        expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), Span::new(2, 4)),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let m = lower_program_to_ir(&hir.program, &info).unwrap();
    let f = &m.funcs[0];
    assert!(
        f.blocks
            .iter()
            .any(|b| b.label.as_deref() == Some("while_cond")),
        "missing while_cond block"
    );
    assert!(
        f.blocks
            .iter()
            .any(|b| b.label.as_deref() == Some("while_body")),
        "missing while_body block"
    );
    assert!(
        f.blocks
            .iter()
            .any(|b| b.label.as_deref() == Some("while_exit")),
        "missing while_exit block"
    );
}

#[test]
fn lower_try_expression_builds_catch_and_join_blocks() {
    let sp = Span::new(0, 1);
    let mut prog = Program {
        stmts: vec![],
        expr: Spanned::new(
            ExprKind::Try {
                body: Box::new(Spanned::new(ExprKind::BytesLit(b"a".to_vec()), sp)),
                catch_name: Some("e".to_string()),
                catch_body: Box::new(Spanned::new(ExprKind::BytesLit(b"b".to_vec()), sp)),
            },
            sp,
        ),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    let m = lower_program_to_ir(&hir.program, &info).unwrap();
    let f = &m.funcs[0];
    assert!(
        f.blocks.iter().any(|b| b.label.as_deref() == Some("catch")),
        "missing catch block"
    );
    assert!(
        f.blocks
            .iter()
            .any(|b| b.label.as_deref() == Some("try_join")),
        "missing try_join block"
    );
}

#[test]
fn lower_self_recursive_fn_literal() {
    let mut i: usize = 0;
    let mut sp = || {
        let s = Span::new(i, i + 1);
        i += 1;
        s
    };
    let ty_i32 = Spanned::new(TyKind::Named("I32".to_string()), sp());
    let ty_fun = Spanned::new(
        TyKind::Fun {
            args: vec![ty_i32.clone()],
            ret: Box::new(ty_i32.clone()),
        },
        sp(),
    );

    let call_fib = Spanned::new(
        ExprKind::Call {
            callee: Box::new(Spanned::new(ExprKind::Var("fib".to_string()), sp())),
            type_args: vec![],
            args: vec![Spanned::new(ExprKind::Var("n".to_string()), sp())],
        },
        sp(),
    );
    let fn_lit = Spanned::new(
        ExprKind::Fn {
            params: vec![("n".to_string(), None)],
            body: vec![Spanned::new(
                StmtKind::Return {
                    expr: Some(call_fib),
                },
                sp(),
            )],
            tail: None,
        },
        sp(),
    );
    let st_let = Spanned::new(
        StmtKind::Let {
            is_const: false,
            exported: false,
            name: "fib".to_string(),
            type_params: Vec::new(),
            ty: Some(ty_fun),
            expr: fn_lit,
        },
        sp(),
    );

    let mut prog = Program {
        stmts: vec![st_let],
        expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), sp()),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    lower_program_to_ir(&hir.program, &info).unwrap();
}

#[test]
fn lower_self_recursive_fn_literal_through_if() {
    let mut i: usize = 0;
    let mut sp = || {
        let s = Span::new(i, i + 1);
        i += 1;
        s
    };
    let ty_i32 = Spanned::new(TyKind::Named("I32".to_string()), sp());
    let ty_fun = Spanned::new(
        TyKind::Fun {
            args: vec![ty_i32.clone()],
            ret: Box::new(ty_i32.clone()),
        },
        sp(),
    );

    let call_fib_1 = Spanned::new(
        ExprKind::Call {
            callee: Box::new(Spanned::new(ExprKind::Var("fib".to_string()), sp())),
            type_args: vec![],
            args: vec![Spanned::new(
                ExprKind::Sub(
                    Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp())),
                    Box::new(Spanned::new(ExprKind::I32Lit(1), sp())),
                ),
                sp(),
            )],
        },
        sp(),
    );
    let call_fib_2 = Spanned::new(
        ExprKind::Call {
            callee: Box::new(Spanned::new(ExprKind::Var("fib".to_string()), sp())),
            type_args: vec![],
            args: vec![Spanned::new(
                ExprKind::Sub(
                    Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp())),
                    Box::new(Spanned::new(ExprKind::I32Lit(2), sp())),
                ),
                sp(),
            )],
        },
        sp(),
    );
    let else_expr = Spanned::new(
        ExprKind::Add(Box::new(call_fib_1), Box::new(call_fib_2)),
        sp(),
    );
    let if_expr = Spanned::new(
        ExprKind::If {
            cond: Box::new(Spanned::new(
                ExprKind::Lt(
                    Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp())),
                    Box::new(Spanned::new(ExprKind::I32Lit(2), sp())),
                ),
                sp(),
            )),
            then_br: Box::new(Spanned::new(ExprKind::Var("n".to_string()), sp())),
            else_br: Box::new(else_expr),
        },
        sp(),
    );
    let fn_lit = Spanned::new(
        ExprKind::Fn {
            params: vec![("n".to_string(), None)],
            body: vec![Spanned::new(
                StmtKind::Return {
                    expr: Some(if_expr),
                },
                sp(),
            )],
            tail: None,
        },
        sp(),
    );
    let st_let = Spanned::new(
        StmtKind::Let {
            is_const: false,
            exported: false,
            name: "fib".to_string(),
            type_params: Vec::new(),
            ty: Some(ty_fun),
            expr: fn_lit,
        },
        sp(),
    );

    let mut prog = Program {
        stmts: vec![st_let],
        expr: Spanned::new(ExprKind::BytesLit(b"ok".to_vec()), sp()),
    };
    let prepared = crate::frontend::prepare_program(&mut prog).unwrap();
    let (hir, info) = crate::semantic::analyze_prepared_program(prepared).unwrap();
    lower_program_to_ir(&hir.program, &info).unwrap();
}
