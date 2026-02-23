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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
