use crate::error::{CompileError, ErrorKind};

/// Parse a single escape sequence starting after `\`. Returns the decoded bytes and the
/// number of source bytes consumed (excluding the leading `\`).
pub(super) fn parse_escape_sequence_impl(
    src: &str,
    i: usize,
    esc_at: usize,
) -> Result<(Vec<u8>, usize), CompileError> {
    let mut j = i;
    let ch = src[j..]
        .chars()
        .next()
        .ok_or_else(|| CompileError::at(ErrorKind::Parse, j, "unterminated escape"))?;
    j += ch.len_utf8();

    let (bytes, consumed) = match ch {
        '\\' => (vec![b'\\'], ch.len_utf8()),
        'n' => (vec![b'\n'], ch.len_utf8()),
        'r' => (vec![b'\r'], ch.len_utf8()),
        't' => (vec![b'\t'], ch.len_utf8()),
        '0' => (vec![0], ch.len_utf8()),
        '\'' => (vec![b'\''], ch.len_utf8()),
        '"' => (vec![b'"'], ch.len_utf8()),
        '`' => (vec![b'`'], ch.len_utf8()),
        'u' => {
            let (cp, _) = if src.get(j..).map(|s| s.starts_with('{')).unwrap_or(false) {
                j += 1;
                let start = j;
                let mut cp: u32 = 0;
                let mut digits = 0u32;
                while let Some(hc) = src[j..].chars().next() {
                    if hc == '}' {
                        break;
                    }
                    let h = hex_val(hc).ok_or_else(|| {
                        CompileError::at(ErrorKind::Parse, j, "invalid hex digit in \\u{...}")
                    })?;
                    if digits >= 6 {
                        return Err(CompileError::at(
                            ErrorKind::Parse,
                            start,
                            "too many hex digits in \\u{...}",
                        ));
                    }
                    cp = (cp << 4) | h;
                    digits += 1;
                    j += hc.len_utf8();
                }
                if src.get(j..).and_then(|s| s.chars().next()) != Some('}') {
                    return Err(CompileError::at(
                        ErrorKind::Parse,
                        start,
                        "unterminated \\u{...}",
                    ));
                }
                j += 1;
                if digits == 0 {
                    return Err(CompileError::at(
                        ErrorKind::Parse,
                        start,
                        "empty \\u{} escape",
                    ));
                }
                (cp, 0)
            } else {
                let mut v = 0u32;
                for _ in 0..4 {
                    let ch = src[j..].chars().next().ok_or_else(|| {
                        CompileError::at(ErrorKind::Parse, j, "unexpected EOF in \\uXXXX")
                    })?;
                    let h = hex_val(ch).ok_or_else(|| {
                        CompileError::at(
                            ErrorKind::Parse,
                            j,
                            format!("invalid hex digit '{}' in \\uXXXX", ch),
                        )
                    })?;
                    v = (v << 4) | h;
                    j += ch.len_utf8();
                }
                (v, j - i - 1)
            };
            let mut out = Vec::new();
            push_scalar_utf8(&mut out, cp, esc_at)?;
            (out, j - i)
        }
        _ => {
            return Err(CompileError::at(
                ErrorKind::Parse,
                esc_at,
                format!("unknown escape \\{}", ch),
            ))
        }
    };
    Ok((bytes, consumed))
}

/// Convert a hexadecimal character to a value.
fn hex_val(ch: char) -> Option<u32> {
    match ch {
        '0'..='9' => Some((ch as u32) - ('0' as u32)),
        'a'..='f' => Some((ch as u32) - ('a' as u32) + 10),
        'A'..='F' => Some((ch as u32) - ('A' as u32) + 10),
        _ => None,
    }
}

fn push_scalar_utf8(out: &mut Vec<u8>, cp: u32, at: usize) -> Result<(), CompileError> {
    if (0xD800..=0xDFFF).contains(&cp) {
        return Err(CompileError::at(
            ErrorKind::Parse,
            at,
            "invalid Unicode scalar (surrogate)",
        ));
    }
    if cp > 0x10FFFF {
        return Err(CompileError::at(
            ErrorKind::Parse,
            at,
            "invalid Unicode scalar (>U+10FFFF)",
        ));
    }
    let ch = char::from_u32(cp)
        .ok_or_else(|| CompileError::at(ErrorKind::Parse, at, "invalid Unicode scalar"))?;
    let mut buf = [0u8; 4];
    let s = ch.encode_utf8(&mut buf);
    out.extend_from_slice(s.as_bytes());
    Ok(())
}
