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

use crate::ir::TypeId;
use crate::typectx::{T_F16, T_F32, T_F64, T_I16, T_I32, T_I64, T_I8};

pub(super) fn is_int(t: TypeId) -> bool {
    matches!(t, T_I8 | T_I16 | T_I32 | T_I64)
}

pub(super) fn is_float(t: TypeId) -> bool {
    matches!(t, T_F16 | T_F32 | T_F64)
}

/// Convert f32 to IEEE 754 binary16 bits (matches vm_f32_to_f16_bits in reg.c).
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

fn f16_bits_to_f32(bits: u16) -> f32 {
    let sign = ((bits & 0x8000) as u32) << 16;
    let exp = (bits >> 10) & 0x1F;
    let mant = (bits & 0x03FF) as u32;
    let out_bits = if exp == 0 {
        if mant == 0 {
            sign
        } else {
            // Subnormal: normalize.
            let mut e: i32 = -14;
            let mut m = mant;
            while (m & 0x0400) == 0 {
                m <<= 1;
                e -= 1;
            }
            m &= 0x03FF;
            let exp32 = ((e + 127) as u32) << 23;
            sign | exp32 | (m << 13)
        }
    } else if exp == 0x1F {
        // Inf/NaN
        sign | 0x7F80_0000 | (mant << 13)
    } else {
        let exp32 = ((exp as i32) - 15 + 127) as u32;
        sign | (exp32 << 23) | (mant << 13)
    };
    f32::from_bits(out_bits)
}

pub(super) fn wrap_int(t: TypeId, x: i64) -> i64 {
    match t {
        T_I8 => (x as i8) as i64,
        T_I16 => (x as i16) as i64,
        T_I32 => (x as i32) as i64,
        T_I64 => x,
        _ => x,
    }
}

pub(super) fn quantize_float(t: TypeId, x: f64) -> f64 {
    match t {
        T_F16 => {
            let bits = f32_to_f16_bits(x as f32);
            f16_bits_to_f32(bits) as f64
        }
        T_F32 => (x as f32) as f64,
        T_F64 => x,
        _ => x,
    }
}
