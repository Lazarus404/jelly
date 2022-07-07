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

// Common subexpression elimination (local, within basic blocks).
// Replaces redundant computations with moves from the first computation.

use crate::ir::{IrBlock, IrModule, IrOp, VRegId};
use std::collections::HashMap;

/// Key for CSE: (op discriminant, normalized operands).
/// Commutative ops use (min, max) for operand order.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum CseKey {
    AddI32(u32, u32),
    SubI32(u32, u32),
    EqI32(u32, u32),
    LtI32(u32, u32),
    Physeq(u32, u32),
    NegI32(u32),
    NotBool(u32),
    BytesLen(u32),
    BytesGetU8(u32, u32),
    BytesConcat2(u32, u32),
    ListHead(u32),
    ListTail(u32),
    ListIsNil(u32),
    ArrayLen(u32),
    ArrayGet(u32, u32),
    ObjHasAtom(u32, u32),
    ObjGetAtom(u32, u32),
    ObjGet(u32, u32),
    Kindof(u32),
    ToDyn(u32),
    FromDynI8(u32),
    FromDynI16(u32),
    FromDynI32(u32),
    FromDynI64(u32),
    FromDynF16(u32),
    FromDynF32(u32),
    FromDynF64(u32),
    FromDynBool(u32),
    FromDynPtr(u32),
}

fn cse_key(op: &IrOp) -> Option<CseKey> {
    use IrOp::*;
    match op {
        AddI32 { a, b, .. } => Some(CseKey::AddI32(
            a.0.min(b.0),
            a.0.max(b.0),
        )),
        SubI32 { a, b, .. } => Some(CseKey::SubI32(a.0, b.0)),
        EqI32 { a, b, .. } => Some(CseKey::EqI32(a.0.min(b.0), a.0.max(b.0))),
        LtI32 { a, b, .. } => Some(CseKey::LtI32(a.0, b.0)),
        Physeq { a, b, .. } => Some(CseKey::Physeq(a.0.min(b.0), a.0.max(b.0))),
        NegI32 { src, .. } => Some(CseKey::NegI32(src.0)),
        NotBool { src, .. } => Some(CseKey::NotBool(src.0)),
        BytesLen { bytes, .. } => Some(CseKey::BytesLen(bytes.0)),
        BytesGetU8 { bytes, index, .. } => Some(CseKey::BytesGetU8(bytes.0, index.0)),
        BytesConcat2 { a, b, .. } => Some(CseKey::BytesConcat2(a.0.min(b.0), a.0.max(b.0))),
        ListHead { list, .. } => Some(CseKey::ListHead(list.0)),
        ListTail { list, .. } => Some(CseKey::ListTail(list.0)),
        ListIsNil { list, .. } => Some(CseKey::ListIsNil(list.0)),
        ArrayLen { arr, .. } => Some(CseKey::ArrayLen(arr.0)),
        ArrayGet { arr, index, .. } => Some(CseKey::ArrayGet(arr.0, index.0)),
        ObjHasAtom { obj, atom_id, .. } => Some(CseKey::ObjHasAtom(obj.0, *atom_id)),
        ObjGetAtom { obj, atom_id, .. } => Some(CseKey::ObjGetAtom(obj.0, *atom_id)),
        ObjGet { obj, atom, .. } => Some(CseKey::ObjGet(obj.0, atom.0)),
        Kindof { src, .. } => Some(CseKey::Kindof(src.0)),
        ToDyn { src, .. } => Some(CseKey::ToDyn(src.0)),
        FromDynI8 { src, .. } => Some(CseKey::FromDynI8(src.0)),
        FromDynI16 { src, .. } => Some(CseKey::FromDynI16(src.0)),
        FromDynI32 { src, .. } => Some(CseKey::FromDynI32(src.0)),
        FromDynI64 { src, .. } => Some(CseKey::FromDynI64(src.0)),
        FromDynF16 { src, .. } => Some(CseKey::FromDynF16(src.0)),
        FromDynF32 { src, .. } => Some(CseKey::FromDynF32(src.0)),
        FromDynF64 { src, .. } => Some(CseKey::FromDynF64(src.0)),
        FromDynBool { src, .. } => Some(CseKey::FromDynBool(src.0)),
        FromDynPtr { src, .. } => Some(CseKey::FromDynPtr(src.0)),
        _ => None,
    }
}

/// Run local CSE within each basic block.
/// Returns true if any change was made.
pub fn common_subexpression_elimination(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        for block in &mut func.blocks {
            changed |= cse_block(block);
        }
    }

    changed
}

fn cse_block(block: &mut IrBlock) -> bool {
    use crate::ir::IrOp;
    let mut seen: HashMap<CseKey, VRegId> = HashMap::new();
    let mut changed = false;

    for ins in &mut block.insns {
        // ObjSetAtom/ObjSet kills ObjGetAtom/ObjGet for same obj+atom (store invalidates load).
        match &ins.op {
            IrOp::ObjSetAtom { obj, atom_id, .. } => {
                let keys_to_remove: Vec<CseKey> = seen
                    .keys()
                    .filter(|k| matches!(k, CseKey::ObjGetAtom(o, a) if *o == obj.0 && *a == *atom_id))
                    .copied()
                    .collect();
                for k in keys_to_remove {
                    seen.remove(&k);
                }
            }
            IrOp::ObjSet { obj, atom, .. } => {
                let keys_to_remove: Vec<CseKey> = seen
                    .keys()
                    .filter(|k| matches!(k, CseKey::ObjGet(o, a) if *o == obj.0 && *a == atom.0))
                    .copied()
                    .collect();
                for k in keys_to_remove {
                    seen.remove(&k);
                }
            }
            _ => {}
        }

        if let Some(key) = cse_key(&ins.op) {
            if let Some(&existing) = seen.get(&key) {
                if let Some(dst) = ins.op.def() {
                    if existing != dst {
                        ins.op = IrOp::Mov { dst, src: existing };
                        changed = true;
                    }
                }
            }
            if let Some(dst) = ins.op.def() {
                seen.insert(key, dst);
            }
        }
    }

    changed
}
