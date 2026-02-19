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

