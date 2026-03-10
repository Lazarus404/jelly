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

use crate::ast::Span;
use crate::error::{CompileError, ErrorKind};
use crate::typectx::{T_ATOM, T_BOOL, T_BYTES, T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

use super::{ArgConstraint, BuiltinConstraints};

pub(super) fn err(span: Span, msg: impl Into<String>) -> CompileError {
    CompileError::new(ErrorKind::Type, span, msg)
}

pub(super) fn namespace_constraints(
    ns: &str,
    name: &str,
    type_args: &[crate::ast::Ty],
    args_len: usize,
    expect: Option<crate::ir::TypeId>,
    span: Span,
) -> Result<Option<BuiltinConstraints>, CompileError> {
    let no_targs = |what: &str| {
        if !type_args.is_empty() {
            return Err(err(span, format!("{what} does not take type arguments")));
        }
        Ok(())
    };

    match (ns, name) {
        ("System", "assert") => {
            no_targs("assert")?;
            if args_len != 1 {
                return Err(err(span, "assert expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BOOL)],
                ret: T_BOOL,
            }))
        }
        ("System", "exit") => {
            no_targs("exit")?;
            if args_len != 0 {
                return Err(err(span, "exit expects 0 args"));
            }
            // Never returns; use expect to satisfy module init (e.g. T_BYTES for REPL)
            Ok(Some(BuiltinConstraints {
                args: vec![],
                ret: expect.unwrap_or(T_BOOL),
            }))
        }
        ("Integer", "to_i8") => {
            no_targs("Integer.to_i8")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_I8,
            }))
        }
        ("Integer", "to_i16") => {
            no_targs("Integer.to_i16")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_I16,
            }))
        }
        ("Integer", "to_i32") => {
            no_targs("Integer.to_i32")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_I32,
            }))
        }
        ("Integer", "to_i64") => {
            no_targs("Integer.to_i64")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_I64,
            }))
        }
        ("Float", "to_f16") => {
            no_targs("Float.to_f16")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_F16,
            }))
        }
        ("Float", "to_f32") => {
            no_targs("Float.to_f32")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_F32,
            }))
        }
        ("Float", "to_f64") => {
            no_targs("Float.to_f64")?;
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_F64,
            }))
        }
        ("Integer", "to_bytes") => {
            no_targs("Integer.to_bytes")?;
            if args_len != 1 {
                return Err(err(span, "Integer.to_bytes expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_BYTES,
            }))
        }
        ("Float", "to_bytes") => {
            no_targs("Float.to_bytes")?;
            if args_len != 1 {
                return Err(err(span, "Float.to_bytes expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_BYTES,
            }))
        }
        ("Float", "is_nan") => {
            no_targs("Float.is_nan")?;
            if args_len != 1 {
                return Err(err(span, "Float.is_nan expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_BOOL,
            }))
        }
        ("Float", "is_infinite") => {
            no_targs("Float.is_infinite")?;
            if args_len != 1 {
                return Err(err(span, "Float.is_infinite expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_BOOL,
            }))
        }
        ("Math", "sqrt") => {
            no_targs("Math.sqrt")?;
            if args_len != 1 {
                return Err(err(span, "Math.sqrt expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Numeric],
                ret: T_F64,
            }))
        }
        ("Bytes", "new") => {
            no_targs("Bytes.new")?;
            if args_len != 1 {
                return Err(err(span, "Bytes.new expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_I32)],
                ret: T_BYTES,
            }))
        }
        ("Bytes", "len") => {
            no_targs("Bytes.len")?;
            if args_len != 1 {
                return Err(err(span, "Bytes.len expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES)],
                ret: T_I32,
            }))
        }
        ("Bytes", "get_u8") => {
            no_targs("Bytes.get_u8")?;
            if args_len != 2 {
                return Err(err(span, "Bytes.get_u8 expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_I32)],
                ret: T_I32,
            }))
        }
        ("Bytes", "set_u8") => {
            no_targs("Bytes.set_u8")?;
            if args_len != 3 {
                return Err(err(span, "Bytes.set_u8 expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::Exact(T_BYTES),
                    ArgConstraint::Exact(T_I32),
                    ArgConstraint::Exact(T_I32),
                ],
                ret: T_BYTES,
            }))
        }
        ("Bytes", "slice") => {
            no_targs("Bytes.slice")?;
            if args_len != 3 {
                return Err(err(span, "Bytes.slice expects 3 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![
                    ArgConstraint::Exact(T_BYTES),
                    ArgConstraint::Exact(T_I32),
                    ArgConstraint::Exact(T_I32),
                ],
                ret: T_BYTES,
            }))
        }
        ("Bytes", "eq") => {
            no_targs("Bytes.eq")?;
            if args_len != 2 {
                return Err(err(span, "Bytes.eq expects 2 args"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES), ArgConstraint::Exact(T_BYTES)],
                ret: T_BOOL,
            }))
        }
        ("Atom", "intern") => {
            no_targs("Atom.intern")?;
            if args_len != 1 {
                return Err(err(span, "Atom.intern expects 1 arg"));
            }
            Ok(Some(BuiltinConstraints {
                args: vec![ArgConstraint::Exact(T_BYTES)],
                ret: T_ATOM,
            }))
        }
        _ => Ok(None),
    }
}
