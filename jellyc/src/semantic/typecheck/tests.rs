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

use super::*;

use crate::hir::{ConstInit, NodeId};
use crate::typectx::T_BYTES;

#[test]
fn typecheck_minimal_bytes_program() {
    let src = "\"ok\"";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let info = typecheck_program(&p).unwrap();
    assert_eq!(
        info.expr_types.get(&NodeId(p.expr.span)).copied(),
        Some(T_BYTES)
    );
}

#[test]
fn const_inits_are_recorded_and_folded() {
    let src = "const x = 1 + 2 * 3; \"ok\"";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let info = typecheck_program(&p).unwrap();

    let s0 = &p.stmts[0];
    let init = info.const_inits.get(&NodeId(s0.span)).expect("const init");
    match init {
        ConstInit::Value(crate::hir::ConstValue::Int(7)) => {}
        other => panic!("unexpected const init: {other:?}"),
    }
}

#[test]
fn const_bytes_var_init_records_alias() {
    let src = "const a = \"A\"; const b = a; \"ok\"";
    let mut p = crate::parse::parse_program(src).unwrap();
    let _prepared = crate::frontend::prepare_program(&mut p).unwrap();
    let info = typecheck_program(&p).unwrap();

    let s1 = &p.stmts[1];
    let init = info.const_inits.get(&NodeId(s1.span)).expect("const init");
    assert_eq!(init, &ConstInit::Alias("a".to_string()));
}
