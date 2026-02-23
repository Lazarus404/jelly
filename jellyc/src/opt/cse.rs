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
//
// This pass is local and does not have alias analysis. For ops that may mutate
// heap objects (bytes/arrays/objects) or call unknown code, we must conservatively
// drop any cached load-like expressions from `seen` to avoid miscompilation.

use crate::ir::{IrBlock, IrModule, IrOp, VRegId};
use std::collections::HashMap;

/// Key for CSE: (op discriminant, normalized operands).
/// Commutative ops use (min, max) for operand order.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum CseKey {
    AddI32(u32, u32),
    SubI32(u32, u32),
    MulI32(u32, u32),
    EqI32(u32, u32),
    LtI32(u32, u32),
    AddI64(u32, u32),
    SubI64(u32, u32),
    MulI64(u32, u32),
    EqI64(u32, u32),
    LtI64(u32, u32),
    AddF16(u32, u32),
    SubF16(u32, u32),
    MulF16(u32, u32),
    AddF32(u32, u32),
    SubF32(u32, u32),
    MulF32(u32, u32),
    DivF32(u32, u32),
    EqF32(u32, u32),
    LtF32(u32, u32),
    AddF64(u32, u32),
    SubF64(u32, u32),
    MulF64(u32, u32),
    DivF64(u32, u32),
    EqF64(u32, u32),
    LtF64(u32, u32),
    Physeq(u32, u32),
    NegI32(u32),
    NegI64(u32),
    NegF32(u32),
    NegF64(u32),
    NotBool(u32),
    SextI64(u32),
    SextI16(u32),
    TruncI8(u32),
    TruncI16(u32),
    F16FromF32(u32),
    F32FromF16(u32),
    F32FromI32(u32),
    F64FromI32(u32),
    F64FromI64(u32),
    F64FromF32(u32),
    F32FromF64(u32),
    F32FromI64(u32),
    I64FromF32(u32),
    I32FromI64(u32),
    I32FromF64(u32),
    I64FromF64(u32),
    I32FromF32(u32),
    F16FromI32(u32),
    I32FromF16(u32),
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
    // Destination type matters for pointer-kind unboxing (Array<T> vs Array<U> are different typed values).
    FromDynPtr(u32, u32),
}

fn cse_key(op: &IrOp, vreg_types: &[u32]) -> Option<CseKey> {
    use IrOp::*;
    match op {
        AddI32 { a, b, .. } => Some(CseKey::AddI32(
            a.0.min(b.0),
            a.0.max(b.0),
        )),
        SubI32 { a, b, .. } => Some(CseKey::SubI32(a.0, b.0)),
        MulI32 { a, b, .. } => Some(CseKey::MulI32(a.0.min(b.0), a.0.max(b.0))),
        EqI32 { a, b, .. } => Some(CseKey::EqI32(a.0.min(b.0), a.0.max(b.0))),
        LtI32 { a, b, .. } => Some(CseKey::LtI32(a.0, b.0)),
        AddI64 { a, b, .. } => Some(CseKey::AddI64(a.0.min(b.0), a.0.max(b.0))),
        SubI64 { a, b, .. } => Some(CseKey::SubI64(a.0, b.0)),
        MulI64 { a, b, .. } => Some(CseKey::MulI64(a.0.min(b.0), a.0.max(b.0))),
        EqI64 { a, b, .. } => Some(CseKey::EqI64(a.0.min(b.0), a.0.max(b.0))),
        LtI64 { a, b, .. } => Some(CseKey::LtI64(a.0, b.0)),
        AddF16 { a, b, .. } => Some(CseKey::AddF16(a.0.min(b.0), a.0.max(b.0))),
        SubF16 { a, b, .. } => Some(CseKey::SubF16(a.0, b.0)),
        MulF16 { a, b, .. } => Some(CseKey::MulF16(a.0.min(b.0), a.0.max(b.0))),
        AddF32 { a, b, .. } => Some(CseKey::AddF32(a.0.min(b.0), a.0.max(b.0))),
        SubF32 { a, b, .. } => Some(CseKey::SubF32(a.0, b.0)),
        MulF32 { a, b, .. } => Some(CseKey::MulF32(a.0.min(b.0), a.0.max(b.0))),
        DivF32 { a, b, .. } => Some(CseKey::DivF32(a.0, b.0)),
        EqF32 { a, b, .. } => Some(CseKey::EqF32(a.0.min(b.0), a.0.max(b.0))),
        LtF32 { a, b, .. } => Some(CseKey::LtF32(a.0, b.0)),
        AddF64 { a, b, .. } => Some(CseKey::AddF64(a.0.min(b.0), a.0.max(b.0))),
        SubF64 { a, b, .. } => Some(CseKey::SubF64(a.0, b.0)),
        MulF64 { a, b, .. } => Some(CseKey::MulF64(a.0.min(b.0), a.0.max(b.0))),
        DivF64 { a, b, .. } => Some(CseKey::DivF64(a.0, b.0)),
        EqF64 { a, b, .. } => Some(CseKey::EqF64(a.0.min(b.0), a.0.max(b.0))),
        LtF64 { a, b, .. } => Some(CseKey::LtF64(a.0, b.0)),
        Physeq { a, b, .. } => Some(CseKey::Physeq(a.0.min(b.0), a.0.max(b.0))),
        NegI32 { src, .. } => Some(CseKey::NegI32(src.0)),
        NegI64 { src, .. } => Some(CseKey::NegI64(src.0)),
        NegF32 { src, .. } => Some(CseKey::NegF32(src.0)),
        NegF64 { src, .. } => Some(CseKey::NegF64(src.0)),
        NotBool { src, .. } => Some(CseKey::NotBool(src.0)),
        SextI64 { src, .. } => Some(CseKey::SextI64(src.0)),
        SextI16 { src, .. } => Some(CseKey::SextI16(src.0)),
        TruncI8 { src, .. } => Some(CseKey::TruncI8(src.0)),
        TruncI16 { src, .. } => Some(CseKey::TruncI16(src.0)),
        F16FromF32 { src, .. } => Some(CseKey::F16FromF32(src.0)),
        F32FromF16 { src, .. } => Some(CseKey::F32FromF16(src.0)),
        F32FromI32 { src, .. } => Some(CseKey::F32FromI32(src.0)),
        F64FromI32 { src, .. } => Some(CseKey::F64FromI32(src.0)),
        F64FromI64 { src, .. } => Some(CseKey::F64FromI64(src.0)),
        F64FromF32 { src, .. } => Some(CseKey::F64FromF32(src.0)),
        F32FromF64 { src, .. } => Some(CseKey::F32FromF64(src.0)),
        F32FromI64 { src, .. } => Some(CseKey::F32FromI64(src.0)),
        I64FromF32 { src, .. } => Some(CseKey::I64FromF32(src.0)),
        I32FromI64 { src, .. } => Some(CseKey::I32FromI64(src.0)),
        I32FromF64 { src, .. } => Some(CseKey::I32FromF64(src.0)),
        I64FromF64 { src, .. } => Some(CseKey::I64FromF64(src.0)),
        I32FromF32 { src, .. } => Some(CseKey::I32FromF32(src.0)),
        F16FromI32 { src, .. } => Some(CseKey::F16FromI32(src.0)),
        I32FromF16 { src, .. } => Some(CseKey::I32FromF16(src.0)),
        BytesLen { bytes, .. } => Some(CseKey::BytesLen(bytes.0)),
        BytesGetU8 { bytes, index, .. } => Some(CseKey::BytesGetU8(bytes.0, index.0)),
        // Bytes concatenation is NOT commutative; preserve operand order.
        BytesConcat2 { a, b, .. } => Some(CseKey::BytesConcat2(a.0, b.0)),
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
        FromDynPtr { dst, src } => {
            let dst_tid = vreg_types.get(dst.0 as usize).copied().unwrap_or(0);
            Some(CseKey::FromDynPtr(src.0, dst_tid))
        }
        _ => None,
    }
}

/// Run local CSE within each basic block.
/// Returns true if any change was made.
pub fn common_subexpression_elimination(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        for block in &mut func.blocks {
            changed |= cse_block(block, &func.vreg_types);
        }
    }

    changed
}

fn cse_block(block: &mut IrBlock, vreg_types: &[u32]) -> bool {
    use crate::ir::IrOp;
    let mut seen: HashMap<CseKey, VRegId> = HashMap::new();
    let mut changed = false;

    for ins in &mut block.insns {
        // Invalidation rules:
        //
        // This pass is local and does not have alias analysis. For ops that may mutate
        // heap objects (bytes/arrays/objects) or call unknown code, we must conservatively
        // drop any cached load-like expressions from `seen` to avoid miscompilation.
        match &ins.op {
            // Calls can mutate any reachable heap object (and also trap/throw).
            // Clear all cached expressions to remain sound.
            IrOp::Call { .. } => {
                seen.clear();
            }
            // Bytes mutation invalidates previous Bytes.get_u8 results for that bytes object.
            IrOp::BytesSetU8 { bytes, .. } => {
                let keys_to_remove: Vec<CseKey> = seen
                    .keys()
                    .filter(|k| matches!(k, CseKey::BytesGetU8(b, _) if *b == bytes.0))
                    .copied()
                    .collect();
                for k in keys_to_remove {
                    seen.remove(&k);
                }
            }
            // Array mutation invalidates previous Array.get results for that array.
            IrOp::ArraySet { arr, .. } => {
                let keys_to_remove: Vec<CseKey> = seen
                    .keys()
                    .filter(|k| matches!(k, CseKey::ArrayGet(a, _) if *a == arr.0))
                    .copied()
                    .collect();
                for k in keys_to_remove {
                    seen.remove(&k);
                }
            }
            IrOp::ObjSetAtom { obj, atom_id, .. } => {
                let keys_to_remove: Vec<CseKey> = seen
                    .keys()
                    .filter(|k| matches!(k, CseKey::ObjGetAtom(o, a) if *o == obj.0 && *a == *atom_id))
                    .chain(
                        seen.keys()
                            .filter(|k| matches!(k, CseKey::ObjHasAtom(o, a) if *o == obj.0 && *a == *atom_id)),
                    )
                    .chain(seen.keys().filter(|k| matches!(k, CseKey::ObjGet(o, _) if *o == obj.0)))
                    .copied()
                    .collect();
                for k in keys_to_remove {
                    seen.remove(&k);
                }
            }
            IrOp::ObjSet { obj, .. } => {
                let keys_to_remove: Vec<CseKey> = seen
                    .keys()
                    // Dynamic set can mutate any property; invalidate all object queries/loads for this object.
                    .filter(|k| {
                        matches!(
                            k,
                            CseKey::ObjGet(o, _)
                                | CseKey::ObjGetAtom(o, _)
                                | CseKey::ObjHasAtom(o, _)
                                if *o == obj.0
                        )
                    })
                    .copied()
                    .collect();
                for k in keys_to_remove {
                    seen.remove(&k);
                }
            }
            _ => {}
        }

        if let Some(key) = cse_key(&ins.op, vreg_types) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::ir::{BlockId, IrBlock, IrFunction, IrInsn, IrModule, IrTerminator, VRegId};
    use crate::typectx::{T_F64, T_I32};

    #[test]
    fn bytes_concat2_is_not_commutative() {
        // Ensure CSE does not treat concat(a,b) and concat(b,a) as identical.
        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![IrFunction {
                name: Some("f".to_string()),
                param_count: 0,
                cap_vregs: vec![],
                entry: BlockId(0),
                blocks: vec![IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::BytesConcat2 {
                                dst: VRegId(2),
                                a: VRegId(0),
                                b: VRegId(1),
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::BytesConcat2 {
                                dst: VRegId(3),
                                a: VRegId(1),
                                b: VRegId(0),
                            },
                        },
                    ],
                    term: IrTerminator::Ret { value: VRegId(2) },
                }],
                vreg_types: vec![],
            }],
            entry: 0,
        };

        let changed = common_subexpression_elimination(&mut m);
        assert!(!changed, "CSE must not fold swapped BytesConcat2");

        let f = &m.funcs[0];
        let b0 = &f.blocks[0];
        assert!(matches!(b0.insns[1].op, IrOp::BytesConcat2 { .. }));
    }

    #[test]
    fn does_not_cse_obj_get_across_call() {
        // Without invalidation, we might fold the second ObjGetAtom into a Mov from the first,
        // which is unsound because the call may mutate the object.
        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![IrFunction {
                name: Some("f".to_string()),
                param_count: 0,
                cap_vregs: vec![],
                entry: BlockId(0),
                blocks: vec![IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ObjGetAtom {
                                dst: VRegId(1),
                                obj: VRegId(0),
                                atom_id: 7,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::Call {
                                dst: VRegId(3),
                                callee: VRegId(2),
                                arg_base: VRegId(10),
                                nargs: 0,
                                sig_id: 0,
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ObjGetAtom {
                                dst: VRegId(4),
                                obj: VRegId(0),
                                atom_id: 7,
                            },
                        },
                    ],
                    term: IrTerminator::Ret { value: VRegId(4) },
                }],
                vreg_types: vec![],
            }],
            entry: 0,
        };

        let _changed = common_subexpression_elimination(&mut m);
        let b0 = &m.funcs[0].blocks[0];
        assert!(
            matches!(b0.insns[2].op, IrOp::ObjGetAtom { .. }),
            "expected second ObjGetAtom to remain (not be CSE'd across Call)"
        );
    }

    #[test]
    fn does_not_cse_bytes_get_across_bytes_set() {
        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![IrFunction {
                name: Some("f".to_string()),
                param_count: 0,
                cap_vregs: vec![],
                entry: BlockId(0),
                blocks: vec![IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::BytesGetU8 {
                                dst: VRegId(2),
                                bytes: VRegId(0),
                                index: VRegId(1),
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::BytesSetU8 {
                                bytes: VRegId(0),
                                index: VRegId(1),
                                value: VRegId(3),
                            },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::BytesGetU8 {
                                dst: VRegId(4),
                                bytes: VRegId(0),
                                index: VRegId(1),
                            },
                        },
                    ],
                    term: IrTerminator::Ret { value: VRegId(4) },
                }],
                vreg_types: vec![],
            }],
            entry: 0,
        };

        let _changed = common_subexpression_elimination(&mut m);
        let b0 = &m.funcs[0].blocks[0];
        assert!(
            matches!(b0.insns[2].op, IrOp::BytesGetU8 { .. }),
            "expected second BytesGetU8 to remain (not be CSE'd across BytesSetU8)"
        );
    }

    #[test]
    fn cse_eliminates_redundant_i32_from_f64_in_block() {
        // Two identical trapping conversions from the same src within a block can be CSE'd safely.
        let mut m = IrModule {
            types: vec![],
            sigs: vec![],
            const_i64: vec![],
            const_f64: vec![0.0],
            const_bytes: vec![],
            atoms: vec![],
            funcs: vec![IrFunction {
                name: Some("f".to_string()),
                param_count: 0,
                cap_vregs: vec![],
                entry: BlockId(0),
                blocks: vec![IrBlock {
                    label: None,
                    insns: vec![
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstF64 { dst: VRegId(0), pool_index: 0 },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::I32FromF64 { dst: VRegId(1), src: VRegId(0) },
                        },
                        IrInsn {
                            span: Span::point(0),
                            op: IrOp::I32FromF64 { dst: VRegId(2), src: VRegId(0) },
                        },
                    ],
                    term: IrTerminator::Ret { value: VRegId(1) },
                }],
                vreg_types: vec![T_F64, T_I32, T_I32],
            }],
            entry: 0,
        };

        let changed = common_subexpression_elimination(&mut m);
        assert!(changed);
        let b0 = &m.funcs[0].blocks[0];
        assert!(
            matches!(b0.insns[2].op, IrOp::Mov { dst: VRegId(2), src: VRegId(1) }),
            "expected second conversion to be replaced by Mov"
        );
    }
}
