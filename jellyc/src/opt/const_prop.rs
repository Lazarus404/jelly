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

use crate::ir::{IrModule, IrOp, TypeId, VRegId};
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
/// Constants are only propagated within a block; crossing block boundaries would
/// incorrectly treat phi results (loop-carried values) as constants.
fn fold_int_result(dst: VRegId, imm: i32, vreg_types: &[TypeId]) -> IrOp {
    let dst_tid = vreg_types.get(dst.0 as usize).copied().unwrap_or(T_I32);
    if dst_tid == T_I8 && imm >= -128 && imm <= 127 {
        IrOp::ConstI8Imm { dst, imm: imm as i8 as u8 }
    } else {
        IrOp::ConstI32 { dst, imm }
    }
}

pub fn constant_propagation(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let vreg_types = &func.vreg_types;
        for block in &mut func.blocks {
            let mut constants: HashMap<VRegId, Constant> = HashMap::new();
            for ins in &mut block.insns {
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
                    ins.op = op;
                    changed = true;
                }
            }
        }
    }

    changed
}
