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

use crate::jlyb::{FunSig, TypeEntry};

use super::ids::{BlockId, TypeId, VRegId};
use super::model::IrModule;
use super::op::IrOp;
use super::terminator::IrTerminator;

fn type_name_from_tables(tid: TypeId, types: &[TypeEntry], sigs: &[FunSig]) -> String {
    if let Some(te) = types.get(tid as usize) {
        match te.kind {
            crate::jlyb::TypeKind::Function => {
                if let Some(sig) = sigs.get(te.p0 as usize) {
                    let mut out = "(".to_string();
                    for (i, a) in sig.args.iter().enumerate() {
                        if i != 0 {
                            out.push_str(", ");
                        }
                        out.push_str(&type_name_from_tables(*a, types, sigs));
                    }
                    out.push_str(") -> ");
                    out.push_str(&type_name_from_tables(sig.ret_type, types, sigs));
                    return out;
                }
            }
            crate::jlyb::TypeKind::Object => {
                const TUPLE_TAG: u32 = 0x8000_0000;
                if (te.p0 & TUPLE_TAG) != 0 {
                    let sig_id = te.p0 & !TUPLE_TAG;
                    if let Some(sig) = sigs.get(sig_id as usize) {
                        let mut out = "Tuple<".to_string();
                        for (i, a) in sig.args.iter().enumerate() {
                            if i != 0 {
                                out.push_str(", ");
                            }
                            out.push_str(&type_name_from_tables(*a, types, sigs));
                        }
                        out.push('>');
                        return out;
                    }
                }
                // Nominal object kinds: use stable tag (hash of nominal key).
                if te.p0 != 0 {
                    return format!("Object#{:08x}", te.p0);
                }
            }
            _ => {}
        }
    }
    match tid {
        crate::typectx::T_BOOL => "Bool".to_string(),
        crate::typectx::T_ATOM => "Atom".to_string(),
        crate::typectx::T_I8 => "I8".to_string(),
        crate::typectx::T_I16 => "I16".to_string(),
        crate::typectx::T_I32 => "I32".to_string(),
        crate::typectx::T_I64 => "I64".to_string(),
        crate::typectx::T_F16 => "F16".to_string(),
        crate::typectx::T_F32 => "F32".to_string(),
        crate::typectx::T_F64 => "F64".to_string(),
        crate::typectx::T_BYTES => "Bytes".to_string(),
        crate::typectx::T_DYNAMIC => "Dynamic".to_string(),
        crate::typectx::T_OBJECT => "Object".to_string(),
        crate::typectx::T_ARRAY_I32 => "Array<I32>".to_string(),
        crate::typectx::T_ARRAY_BYTES => "Array<Bytes>".to_string(),
        crate::typectx::T_LIST_I32 => "List<I32>".to_string(),
        crate::typectx::T_LIST_BYTES => "List<Bytes>".to_string(),
        _ => format!("t{tid}"),
    }
}

pub fn render_ir(m: &IrModule) -> String {
    fn v(v: VRegId) -> String {
        format!("v{}", v.0)
    }

    fn b(b: BlockId) -> String {
        format!("b{}", b.0)
    }

    fn atom_name(atom_id: u32, m: &IrModule) -> String {
        let Some(bytes) = m.atoms.get(atom_id as usize) else {
            return format!("<atom#{atom_id}>");
        };
        match std::str::from_utf8(bytes) {
            Ok(s) => format!("{s:?}"),
            Err(_) => format!("<atom#{atom_id}:non-utf8>"),
        }
    }

    fn normalize_debug_ids(mut s: String) -> String {
        // Replace newtype Debugs like `VRegId(12)` with `v12` (and `BlockId(3)` with `b3`)
        // without pulling in a regex dependency.
        fn replace_newtype(s: String, ty: &str, prefix: &str) -> String {
            let pat = format!("{ty}(");
            let mut out = String::with_capacity(s.len());
            let bytes = s.as_bytes();
            let mut i = 0usize;
            while i < bytes.len() {
                if s[i..].starts_with(&pat) {
                    i += pat.len();
                    let start = i;
                    while i < bytes.len() && bytes[i].is_ascii_digit() {
                        i += 1;
                    }
                    if i < bytes.len() && bytes[i] == b')' && i > start {
                        out.push_str(prefix);
                        out.push_str(&s[start..i]);
                        i += 1;
                        continue;
                    }
                    out.push_str(&pat);
                    i = start;
                    continue;
                }
                out.push(bytes[i] as char);
                i += 1;
            }
            out
        }
        s = replace_newtype(s, "VRegId", "v");
        s = replace_newtype(s, "BlockId", "b");
        s
    }

    fn render_op(op: &IrOp, m: &IrModule) -> String {
        match op {
            IrOp::ConstAtom { dst, atom_id } => format!(
                "ConstAtom {{ dst: {}, atom: {} }}",
                v(*dst),
                atom_name(*atom_id, m)
            ),
            IrOp::ObjHasAtom { dst, obj, atom_id } => format!(
                "ObjHasAtom {{ dst: {}, obj: {}, atom: {} }}",
                v(*dst),
                v(*obj),
                atom_name(*atom_id, m)
            ),
            IrOp::ObjGetAtom { dst, obj, atom_id } => format!(
                "ObjGetAtom {{ dst: {}, obj: {}, atom: {} }}",
                v(*dst),
                v(*obj),
                atom_name(*atom_id, m)
            ),
            IrOp::ObjSetAtom {
                obj,
                atom_id,
                value,
            } => format!(
                "ObjSetAtom {{ obj: {}, atom: {}, value: {} }}",
                v(*obj),
                atom_name(*atom_id, m),
                v(*value)
            ),
            IrOp::ConstBytes { dst, pool_index } => {
                let len = m
                    .const_bytes
                    .get(*pool_index as usize)
                    .map(|b| b.len())
                    .unwrap_or(0);
                format!("ConstBytes {{ dst: {}, len: {len} }}", v(*dst))
            }
            IrOp::ConstI64 { dst, pool_index } => {
                let val = m
                    .const_i64
                    .get(*pool_index as usize)
                    .copied()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "<bad-pool-index>".to_string());
                format!("ConstI64 {{ dst: {}, imm: {val} }}", v(*dst))
            }
            IrOp::ConstF64 { dst, pool_index } => {
                let val = m
                    .const_f64
                    .get(*pool_index as usize)
                    .copied()
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "<bad-pool-index>".to_string());
                format!("ConstF64 {{ dst: {}, imm: {val} }}", v(*dst))
            }
            _ => normalize_debug_ids(format!("{op:?}")),
        }
    }

    fn render_term(t: &IrTerminator) -> String {
        match t {
            IrTerminator::Jmp { target } => format!("Jmp {{ target: {} }}", b(*target)),
            IrTerminator::JmpIf {
                cond,
                then_tgt,
                else_tgt,
            } => format!(
                "JmpIf {{ cond: {}, then_tgt: {}, else_tgt: {} }}",
                v(*cond),
                b(*then_tgt),
                b(*else_tgt)
            ),
            IrTerminator::SwitchKind {
                kind,
                cases,
                default,
            } => {
                let mut out = format!("SwitchKind {{ kind: {}, cases: [", v(*kind));
                for (i, (k, tgt)) in cases.iter().enumerate() {
                    if i != 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&format!("({k}, {})", b(*tgt)));
                }
                out.push_str(&format!("], default: {} }}", b(*default)));
                out
            }
            IrTerminator::Ret { value } => format!("Ret {{ value: {} }}", v(*value)),
            IrTerminator::TailCall {
                callee,
                sig_id,
                arg_base,
                nargs,
            } => format!(
                "TailCall {{ callee: {}, sig_id: {}, arg_base: {}, nargs: {} }}",
                v(*callee),
                sig_id,
                v(*arg_base),
                nargs
            ),
            IrTerminator::Unreachable => "Unreachable".to_string(),
        }
    }

    let mut out = String::new();
    out.push_str("ir {\n");
    out.push_str(&format!("  funcs: {}\n", m.funcs.len()));
    out.push_str(&format!("  entry: {}\n", m.entry));
    for (fi, f) in m.funcs.iter().enumerate() {
        out.push_str(&format!(
            "  fn[{fi}] {} params={} entry=b{}\n",
            f.name.as_deref().unwrap_or("<anon>"),
            f.param_count,
            f.entry.0
        ));
        out.push_str("    vregs:\n");
        for (vi, tid) in f.vreg_types.iter().enumerate() {
            out.push_str(&format!(
                "      v{vi}: {}\n",
                type_name_from_tables(*tid, &m.types, &m.sigs)
            ));
        }
        for (bi, b) in f.blocks.iter().enumerate() {
            out.push_str(&format!(
                "    b{bi}{}:\n",
                b.label
                    .as_deref()
                    .map(|s| format!(" ({s})"))
                    .unwrap_or_default()
            ));
            for insn in &b.insns {
                let op_s = render_op(&insn.op, m);
                out.push_str(&format!(
                    "      @{}..{} {}\n",
                    insn.span.start, insn.span.end, op_s
                ));
            }
            out.push_str(&format!("      term: {}\n", render_term(&b.term)));
        }
    }
    out.push_str("}\n");
    out
}
