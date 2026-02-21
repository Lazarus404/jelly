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

use crate::ast::Span;
use crate::source::Source;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErrorKind {
    Parse,
    Name,
    Type,
    Codegen,
    Internal,
}

#[derive(Clone, Debug)]
pub struct CompileError {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Span,
}

impl CompileError {
    pub fn new(kind: ErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            span,
        }
    }

    pub fn at(kind: ErrorKind, at: usize, message: impl Into<String>) -> Self {
        Self::new(kind, Span::point(at), message)
    }

    pub fn render(&self, src: &str, path: Option<&str>) -> String {
        let source = Source::new(src);
        let (line, col, src_line, caret) = source.render_span(self.span);

        let kind = match self.kind {
            ErrorKind::Parse => "parse error",
            ErrorKind::Name => "name error",
            ErrorKind::Type => "type error",
            ErrorKind::Codegen => "codegen error",
            ErrorKind::Internal => "internal error",
        };

        let loc = match path {
            Some(p) => format!("{}:{}:{}", p, line, col),
            None => format!("{}:{}", line, col),
        };

        format!(
            "{}: {}\n --> {}\n{}\n{}",
            kind, self.message, loc, src_line, caret
        )
    }
}

/// A compile-time warning (eg implicit narrowing conversion).
#[derive(Clone, Debug)]
pub struct CompileWarning {
    pub message: String,
    pub span: Span,
}

impl CompileWarning {
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn render(&self, src: &str, path: Option<&str>) -> String {
        let source = Source::new(src);
        let (line, col, src_line, caret) = source.render_span(self.span);

        let loc = match path {
            Some(p) => format!("{}:{}:{}", p, line, col),
            None => format!("{}:{}", line, col),
        };

        format!(
            "warning: {}\n --> {}\n{}\n{}",
            self.message, loc, src_line, caret
        )
    }
}

