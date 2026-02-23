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

// Constant propagation and folding.

use crate::ir::{IrInsn, IrModule, IrOp, IrTerminator, TypeId, VRegId};
use crate::typectx::{T_I32, T_I8};
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Constant {
    I32(i32),
    Bool(bool),
    F16(u16),
}

fn f16_bits_to_f32(bits: u16) -> f32 {
    if (bits & 0x7FFF) == 0 {
        return if (bits & 0x8000) != 0 { -0.0 } else { 0.0 };
    }
    let sign = ((bits as u32) & 0x8000) << 16;
    let mut exp = ((bits >> 10) & 0x1F) as u32;
    let mut mant = ((bits & 0x3FF) as u32) << 13;
    if exp == 0 {
        while (mant & 0x800000) == 0 {
            mant <<= 1;
            exp = exp.wrapping_sub(1);
        }
        exp += 1;
    } else if exp == 31 {
        return f32::from_bits(sign | 0x7F800000 | if mant != 0 { 0x00400000 } else { 0 });
    }
    exp += 127 - 15;
    f32::from_bits(sign | (exp << 23) | mant)
}

fn f32_to_f16_bits(f: f32) -> u16 {
    let u32_bits = f.to_bits();
    let sign = (u32_bits >> 16) & 0x8000;
    let exp = (u32_bits >> 23) & 0xFF;
    let mant = u32_bits & 0x7FFFFF;
    if exp == 0xFF {
        return (sign | 0x7C00 | if mant != 0 { 0x200 } else { 0 }) as u16;
    }
    if exp == 0 && mant == 0 {
        return sign as u16;
    }
    let exp16 = (exp as i32) - 127 + 15;
    if exp16 >= 31 {
        return (sign | 0x7C00) as u16;
    }
    if exp16 <= 0 {
        return sign as u16;
    }
    (sign | ((exp16 as u32) << 10) | (mant >> 13)) as u16
}

/// Run constant propagation and folding.
/// Returns true if any change was made.
fn fold_int_result(dst: VRegId, imm: i32, vreg_types: &[TypeId]) -> IrOp {
    let dst_tid = vreg_types.get(dst.0 as usize).copied().unwrap_or(T_I32);
    if dst_tid == T_I8 && imm >= -128 && imm <= 127 {
        IrOp::ConstI8Imm { dst, imm: imm as i8 as u8 }
    } else {
        IrOp::ConstI32 { dst, imm }
    }
}

fn block_successors(term: &IrTerminator) -> Vec<usize> {
    match term {
        IrTerminator::Jmp { target } => vec![target.0 as usize],
        IrTerminator::JmpIf { then_tgt, else_tgt, .. } => vec![then_tgt.0 as usize, else_tgt.0 as usize],
        IrTerminator::SwitchKind { cases, default, .. } => {
            let mut v: Vec<usize> = cases.iter().map(|(_, b)| b.0 as usize).collect();
            v.push(default.0 as usize);
            v
        }
        IrTerminator::Ret { .. } | IrTerminator::Unreachable => vec![],
    }
}

fn count_leading_phis(insns: &[IrInsn]) -> usize {
    insns
        .iter()
        .take_while(|ins| matches!(ins.op, IrOp::Phi { .. }))
        .count()
}

fn same_rewriteable_op(a: &IrOp, b: &IrOp) -> bool {
    use IrOp::*;
    match (a, b) {
        (ConstI32 { dst: ad, imm: ai }, ConstI32 { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (ConstI8Imm { dst: ad, imm: ai }, ConstI8Imm { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (ConstF16 { dst: ad, bits: ab }, ConstF16 { dst: bd, bits: bb }) => ad == bd && ab == bb,
        (ConstBool { dst: ad, imm: ai }, ConstBool { dst: bd, imm: bi }) => ad == bd && ai == bi,
        (Mov { dst: ad, src: asrc }, Mov { dst: bd, src: bsrc }) => ad == bd && asrc == bsrc,
        _ => false,
    }
}

pub fn constant_propagation(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let vreg_types = &func.vreg_types;

        // Build predecessor lists from normal CFG edges only.
        // (We intentionally do NOT propagate facts along exception edges; catch blocks are treated as unknown.)
        let nb = func.blocks.len();
        let mut preds: Vec<Vec<usize>> = vec![Vec::new(); nb];
        for (i, b) in func.blocks.iter().enumerate() {
            for succ in block_successors(&b.term) {
                if succ < nb {
                    preds[succ].push(i);
                }
            }
        }
        for p in &mut preds {
            p.sort_unstable();
            p.dedup();
        }

        // Forward dataflow: out constants per block.
        let mut out_maps: Vec<HashMap<VRegId, Constant>> = vec![HashMap::new(); nb];
        let mut changed_this_func = true;
        while changed_this_func {
            changed_this_func = false;
            for bid in 0..nb {
                // Meet: keep only facts that are identical across all predecessors.
                let mut constants: HashMap<VRegId, Constant> = HashMap::new();
                let mut first = true;
                for &p in &preds[bid] {
                    if first {
                        constants = out_maps[p].clone();
                        first = false;
                    } else {
                        let pred_map = &out_maps[p];
                        constants.retain(|k, v| pred_map.get(k) == Some(v));
                    }
                }
                if first {
                    constants.clear();
                }

                let block = &mut func.blocks[bid];
                let phi_count = count_leading_phis(&block.insns);

                // Phi constants: a phi result is constant if all incoming values are the same constant.
                for i in 0..phi_count {
                    let IrOp::Phi { dst, incomings } = &block.insns[i].op else { break };
                    let mut cand: Option<Constant> = None;
                    let mut ok = true;
                    for (pred, v) in incomings {
                        let p = pred.0 as usize;
                        let Some(c) = out_maps.get(p).and_then(|m| m.get(v)).copied() else {
                            ok = false;
                            break;
                        };
                        if let Some(prev) = cand {
                            if prev != c {
                                ok = false;
                                break;
                            }
                        } else {
                            cand = Some(c);
                        }
                    }
                    if ok {
                        if let Some(c) = cand {
                            constants.insert(*dst, c);
                        } else {
                            constants.remove(dst);
                        }
                    } else {
                        constants.remove(dst);
                    }
                }

                // Fold within block (skip leading phis; rewriting phis here could break phi grouping invariants).
                for ins in block.insns.iter_mut().skip(phi_count) {
                    let replacement = match &ins.op {
                        IrOp::ConstI32 { dst, imm } => {
                            constants.insert(*dst, Constant::I32(*imm));
                            None
                        }
                        IrOp::ConstI8Imm { dst, imm } => {
                            let val = *imm as i8 as i32;
                            constants.insert(*dst, Constant::I32(val));
                            None
                        }
                        IrOp::ConstF16 { dst, bits } => {
                            constants.insert(*dst, Constant::F16(*bits));
                            None
                        }
                        IrOp::ConstBool { dst, imm } => {
                            constants.insert(*dst, Constant::Bool(*imm));
                            None
                        }
                        IrOp::Mov { dst, src } => {
                            if let Some(&c) = constants.get(src) {
                                constants.insert(*dst, c);
                                match c {
                                    Constant::I32(imm) => Some(fold_int_result(*dst, imm, vreg_types)),
                                    Constant::Bool(imm) => Some(IrOp::ConstBool { dst: *dst, imm }),
                                    Constant::F16(bits) => Some(IrOp::ConstF16 { dst: *dst, bits }),
                                }
                            } else {
                                None
                            }
                        }
                        IrOp::AddI32 { dst, a, b } => {
                            if let (Some(Constant::I32(va)), Some(Constant::I32(vb))) =
                                (constants.get(a), constants.get(b))
                            {
                                let imm = va.wrapping_add(*vb);
                                constants.insert(*dst, Constant::I32(imm));
                                Some(fold_int_result(*dst, imm, vreg_types))
                            } else if constants.get(a) == Some(&Constant::I32(0)) {
                                // 0 + b -> Mov(dst, b); avoids peephole/reg-alloc issues with 0 + ObjGetAtom
                                if let Some(&c) = constants.get(b) {
                                    constants.insert(*dst, c);
                                }
                                Some(IrOp::Mov { dst: *dst, src: *b })
                            } else if constants.get(b) == Some(&Constant::I32(0)) {
                                // a + 0 -> Mov(dst, a)
                                if let Some(&c) = constants.get(a) {
                                    constants.insert(*dst, c);
                                }
                                Some(IrOp::Mov { dst: *dst, src: *a })
                            } else {
                                None
                            }
                        }
                        IrOp::SubI32 { dst, a, b } => {
                            if let (Some(Constant::I32(va)), Some(Constant::I32(vb))) =
                                (constants.get(a), constants.get(b))
                            {
                                let imm = va.wrapping_sub(*vb);
                                constants.insert(*dst, Constant::I32(imm));
                                Some(fold_int_result(*dst, imm, vreg_types))
                            } else {
                                None
                            }
                        }
                        IrOp::MulI32 { dst, a, b } => {
                            if let (Some(Constant::I32(va)), Some(Constant::I32(vb))) =
                                (constants.get(a), constants.get(b))
                            {
                                let imm = va.wrapping_mul(*vb);
                                constants.insert(*dst, Constant::I32(imm));
                                Some(fold_int_result(*dst, imm, vreg_types))
                            } else {
                                None
                            }
                        }
                        IrOp::NegI32 { dst, src } => {
                            if let Some(Constant::I32(v)) = constants.get(src) {
                                let imm = v.wrapping_neg();
                                constants.insert(*dst, Constant::I32(imm));
                                Some(fold_int_result(*dst, imm, vreg_types))
                            } else {
                                None
                            }
                        }
                        IrOp::EqI32 { dst, a, b } => {
                            if let (Some(Constant::I32(va)), Some(Constant::I32(vb))) =
                                (constants.get(a), constants.get(b))
                            {
                                let imm = *va == *vb;
                                constants.insert(*dst, Constant::Bool(imm));
                                Some(IrOp::ConstBool { dst: *dst, imm })
                            } else {
                                None
                            }
                        }
                        IrOp::LtI32 { dst, a, b } => {
                            if let (Some(Constant::I32(va)), Some(Constant::I32(vb))) =
                                (constants.get(a), constants.get(b))
                            {
                                let imm = va < vb;
                                constants.insert(*dst, Constant::Bool(imm));
                                Some(IrOp::ConstBool { dst: *dst, imm })
                            } else {
                                None
                            }
                        }
                        IrOp::AddF16 { dst, a, b } => {
                            if let (Some(Constant::F16(bits_a)), Some(Constant::F16(bits_b))) =
                                (constants.get(a), constants.get(b))
                            {
                                let fa = f16_bits_to_f32(*bits_a);
                                let fb = f16_bits_to_f32(*bits_b);
                                let bits = f32_to_f16_bits(fa + fb);
                                constants.insert(*dst, Constant::F16(bits));
                                Some(IrOp::ConstF16 { dst: *dst, bits })
                            } else {
                                None
                            }
                        }
                        IrOp::SubF16 { dst, a, b } => {
                            if let (Some(Constant::F16(bits_a)), Some(Constant::F16(bits_b))) =
                                (constants.get(a), constants.get(b))
                            {
                                let fa = f16_bits_to_f32(*bits_a);
                                let fb = f16_bits_to_f32(*bits_b);
                                let bits = f32_to_f16_bits(fa - fb);
                                constants.insert(*dst, Constant::F16(bits));
                                Some(IrOp::ConstF16 { dst: *dst, bits })
                            } else {
                                None
                            }
                        }
                        IrOp::MulF16 { dst, a, b } => {
                            if let (Some(Constant::F16(bits_a)), Some(Constant::F16(bits_b))) =
                                (constants.get(a), constants.get(b))
                            {
                                let fa = f16_bits_to_f32(*bits_a);
                                let fb = f16_bits_to_f32(*bits_b);
                                let bits = f32_to_f16_bits(fa * fb);
                                constants.insert(*dst, Constant::F16(bits));
                                Some(IrOp::ConstF16 { dst: *dst, bits })
                            } else {
                                None
                            }
                        }
                        IrOp::NotBool { dst, src } => {
                            if let Some(Constant::Bool(v)) = constants.get(src) {
                                let imm = !*v;
                                constants.insert(*dst, Constant::Bool(imm));
                                Some(IrOp::ConstBool { dst: *dst, imm })
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    if let Some(op) = replacement {
                        if !same_rewriteable_op(&ins.op, &op) {
                            ins.op = op;
                            changed = true;
                        }
                    }
                }

                // Terminator folding.
                let new_term: Option<IrTerminator> = match &block.term {
                    IrTerminator::JmpIf {
                        cond,
                        then_tgt,
                        else_tgt,
                    } => constants.get(cond).and_then(|c| match c {
                        Constant::Bool(b) => Some(IrTerminator::Jmp {
                            target: if *b { *then_tgt } else { *else_tgt },
                        }),
                        _ => None,
                    }),
                    IrTerminator::SwitchKind {
                        kind,
                        cases,
                        default,
                    } => constants.get(kind).and_then(|c| match c {
                        Constant::I32(v) => {
                            let tag = u8::try_from(*v).ok();
                            let target = tag
                                .and_then(|t| cases.iter().find_map(|(k, b)| if *k == t { Some(*b) } else { None }))
                                .unwrap_or(*default);
                            Some(IrTerminator::Jmp { target })
                        }
                        _ => None,
                    }),
                    _ => None,
                };
                if let Some(t) = new_term {
                    match (&block.term, &t) {
                        (IrTerminator::Jmp { target: a }, IrTerminator::Jmp { target: b }) if a == b => {}
                        _ => {
                            block.term = t;
                            changed = true;
                        }
                    }
                }

                if out_maps[bid] != constants {
                    out_maps[bid] = constants;
                    changed_this_func = true;
                }
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

    #[test]
    fn const_prop_folds_jmpif_with_const_bool_cond() {
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
                blocks: vec![
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstBool {
                                dst: VRegId(0),
                                imm: true,
                            },
                        }],
                        term: IrTerminator::JmpIf {
                            cond: VRegId(0),
                            then_tgt: BlockId(1),
                            else_tgt: BlockId(2),
                        },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::Ret { value: VRegId(0) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::Ret { value: VRegId(0) },
                    },
                ],
                vreg_types: vec![crate::typectx::T_BOOL],
            }],
            entry: 0,
        };

        let changed = constant_propagation(&mut m);
        assert!(changed);
        match m.funcs[0].blocks[0].term {
            IrTerminator::Jmp { target } => assert_eq!(target, BlockId(1)),
            _ => panic!("expected Jmp"),
        }
    }

    #[test]
    fn const_prop_folds_switch_kind_with_const_tag() {
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
                blocks: vec![
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstI32 { dst: VRegId(0), imm: 3 },
                        }],
                        term: IrTerminator::SwitchKind {
                            kind: VRegId(0),
                            cases: vec![(3, BlockId(1)), (5, BlockId(2))],
                            default: BlockId(2),
                        },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::Ret { value: VRegId(0) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::Ret { value: VRegId(0) },
                    },
                ],
                vreg_types: vec![crate::typectx::T_I32],
            }],
            entry: 0,
        };

        let changed = constant_propagation(&mut m);
        assert!(changed);
        match m.funcs[0].blocks[0].term {
            IrTerminator::Jmp { target } => assert_eq!(target, BlockId(1)),
            _ => panic!("expected Jmp"),
        }
    }

    #[test]
    fn const_prop_folds_jmpif_when_cond_constant_is_defined_in_pred_block() {
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
                blocks: vec![
                    IrBlock {
                        label: None,
                        insns: vec![IrInsn {
                            span: Span::point(0),
                            op: IrOp::ConstBool { dst: VRegId(0), imm: false },
                        }],
                        term: IrTerminator::Jmp { target: BlockId(1) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::JmpIf {
                            cond: VRegId(0),
                            then_tgt: BlockId(2),
                            else_tgt: BlockId(3),
                        },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::Ret { value: VRegId(0) },
                    },
                    IrBlock {
                        label: None,
                        insns: vec![],
                        term: IrTerminator::Ret { value: VRegId(0) },
                    },
                ],
                vreg_types: vec![crate::typectx::T_BOOL],
            }],
            entry: 0,
        };

        let changed = constant_propagation(&mut m);
        assert!(changed);
        match m.funcs[0].blocks[1].term {
            IrTerminator::Jmp { target } => assert_eq!(target, BlockId(3)),
            _ => panic!("expected Jmp"),
        }
    }
}
