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
use crate::ir::{IrModule, IrOp, IrTerminator, VRegId};
use std::collections::HashMap;

mod cfg;
mod float16;
mod fold;

use self::cfg::{block_successors, count_leading_phis};
use self::float16::{f16_bits_to_f32, f32_to_f16_bits};
use self::fold::{fold_int_result, same_rewriteable_op};

#[derive(Clone, Copy, Debug, PartialEq)]
enum Constant {
    I32(i32),
    Bool(bool),
    F16(u16),
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
                    let IrOp::Phi { dst, incomings } = &block.insns[i].op else {
                        break;
                    };
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
                                    Constant::I32(imm) => {
                                        Some(fold_int_result(*dst, imm, vreg_types))
                                    }
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
                                .and_then(|t| {
                                    cases
                                        .iter()
                                        .find_map(|(k, b)| if *k == t { Some(*b) } else { None })
                                })
                                .unwrap_or(*default);
                            Some(IrTerminator::Jmp { target })
                        }
                        _ => None,
                    }),
                    _ => None,
                };
                if let Some(t) = new_term {
                    match (&block.term, &t) {
                        (IrTerminator::Jmp { target: a }, IrTerminator::Jmp { target: b })
                            if a == b => {}
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
mod tests;
