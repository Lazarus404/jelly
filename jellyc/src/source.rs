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

/// Source code for a file.
pub struct Source<'a> {
    pub text: &'a str,
    line_starts: Vec<usize>, // byte offsets
}

impl<'a> Source<'a> {
    pub fn new(text: &'a str) -> Self {
        let mut line_starts = vec![0usize];
        for (i, b) in text.as_bytes().iter().enumerate() {
            if *b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { text, line_starts }
    }

    /// Clamp an offset to the length of the text.
    pub fn clamp_offset(&self, off: usize) -> usize {
        off.min(self.text.len())
    }

    /// Get the line and column for an offset.
    pub fn line_col(&self, off: usize) -> (usize, usize) {
        let off = self.clamp_offset(off);
        let line_idx = match self.line_starts.binary_search(&off) {
            Ok(i) => i,
            Err(i) => i.saturating_sub(1),
        };
        let line_start = self.line_starts[line_idx];
        let line = line_idx + 1; // 1-based

        let mut col = 1usize;
        for (bi, ch) in self.text[line_start..off].char_indices() {
            let _ = bi;
            col += 1;
            let _ = ch;
        }
        (line, col)
    }

    /// Get the text for a line.
    pub fn line_text(&self, line_1: usize) -> &'a str {
        let line_idx = line_1.saturating_sub(1);
        let start = *self.line_starts.get(line_idx).unwrap_or(&0);
        let end = self
            .line_starts
            .get(line_idx + 1)
            .copied()
            .unwrap_or_else(|| self.text.len());
        let mut end2 = end;
        if end2 > start && self.text.as_bytes()[end2 - 1] == b'\n' {
            end2 -= 1;
            if end2 > start && self.text.as_bytes()[end2 - 1] == b'\r' {
                end2 -= 1;
            }
        }
        &self.text[start..end2]
    }

    /// Render a caret line.
    pub fn caret_line(&self, line_1: usize, col_1: usize) -> String {
        let text = self.line_text(line_1);
        let mut out = String::new();

        // Render spaces roughly aligned to character columns (tabs are treated as 1 column).
        let mut cur_col = 1usize;
        for ch in text.chars() {
            if cur_col >= col_1 {
                break;
            }
            out.push(if ch == '\t' { '\t' } else { ' ' });
            cur_col += 1;
        }
        out.push('^');
        out
    }

    /// Render a span as a line, column, source line, and caret line.
    pub fn render_span(&self, span: Span) -> (usize, usize, String, String) {
        let (line, col) = self.line_col(span.start);
        let src_line = self.line_text(line).to_string();
        let caret = self.caret_line(line, col);
        (line, col, src_line, caret)
    }
}

