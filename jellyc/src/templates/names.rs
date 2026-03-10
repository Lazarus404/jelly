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

use crate::ast::{Ty, TyKind};
use crate::error::{CompileError, ErrorKind};

/// Get the key for a type.
pub(super) fn type_key(t: &Ty) -> Result<String, CompileError> {
    fn enc(s: &str) -> String {
        // Identifier-safe fragment: [A-Za-z0-9_]+
        s.chars()
            .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
            .collect()
    }

    match &t.node {
        TyKind::Named(n) => Ok(enc(n)),
        TyKind::Generic { base, args } => {
            let mut out = enc(base);
            for a in args {
                out.push('_');
                out.push_str(&type_key(a)?);
            }
            Ok(out)
        }
        TyKind::Tuple(elems) => {
            let mut out = "Tuple".to_string();
            out.push('_');
            out.push_str(&elems.len().to_string());
            for el in elems {
                out.push('_');
                out.push_str(&type_key(el)?);
            }
            Ok(out)
        }
        TyKind::Fun { .. } => Err(CompileError::new(
            ErrorKind::Type,
            t.span,
            "function types are not supported as template arguments yet",
        )),
    }
}

/// Get the name for a specialization.
pub(super) fn spec_name(base: &str, type_args: &[Ty]) -> Result<String, CompileError> {
    let mut out = base.to_string();
    for a in type_args {
        out.push_str("__");
        out.push_str(&type_key(a)?);
    }
    Ok(out)
}
