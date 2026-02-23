#![allow(dead_code)]

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

// Prelude module: hand-coded bytecode for built-in functions.
// Keep aligned with TypeCtx::new_program_base() and format constants.

use crate::typectx::{TypeCtx, T_ARRAY_BYTES, T_BOOL, T_BYTES, T_I32};

use super::{Function, Insn, Module, Op, PRELUDE_FUN_COUNT};

#[inline]
fn ins(op: Op, a: u8, b: u8, c: u8, imm: u32) -> Insn {
    Insn { op: op as u8, a, b, c, imm }
}

#[inline]
fn rel(delta: i32) -> u32 {
    delta as u32
}

#[inline]
fn mov(dst: u8, src: u8) -> Insn {
    ins(Op::Mov, dst, src, 0, 0)
}

#[inline]
fn ret(src: u8) -> Insn {
    ins(Op::Ret, src, 0, 0, 0)
}

#[inline]
fn jmp(delta: i32) -> Insn {
    ins(Op::Jmp, 0, 0, 0, rel(delta))
}

#[inline]
fn jmp_if(cond: u8, delta: i32) -> Insn {
    ins(Op::JmpIf, cond, 0, 0, rel(delta))
}

#[inline]
fn const_i32(dst: u8, imm: i32) -> Insn {
    ins(Op::ConstI32, dst, 0, 0, imm as u32)
}

#[inline]
fn const_bool(dst: u8, v: bool) -> Insn {
    // In this encoding, the boolean immediate is carried in `c`.
    ins(Op::ConstBool, dst, 0, if v { 1 } else { 0 }, 0)
}

#[inline]
fn add_i32(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::AddI32, dst, a, b, 0)
}

#[inline]
fn lt_i32(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::LtI32, dst, a, b, 0)
}

#[inline]
fn eq_i32(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::EqI32, dst, a, b, 0)
}

#[inline]
fn bytes_new(dst: u8, len: u8) -> Insn {
    ins(Op::BytesNew, dst, len, 0, 0)
}

#[inline]
fn bytes_len(dst: u8, src: u8) -> Insn {
    ins(Op::BytesLen, dst, src, 0, 0)
}

#[inline]
fn bytes_concat2(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::BytesConcat2, dst, a, b, 0)
}

#[inline]
fn bytes_concat_many(dst: u8, src_arr: u8) -> Insn {
    ins(Op::BytesConcatMany, dst, src_arr, 0, 0)
}

#[inline]
fn bytes_get_u8(dst: u8, src: u8, idx: u8) -> Insn {
    ins(Op::BytesGetU8, dst, src, idx, 0)
}

#[inline]
fn bytes_set_u8(value: u8, dst: u8, idx: u8) -> Insn {
    ins(Op::BytesSetU8, value, dst, idx, 0)
}

#[inline]
fn const_f64(dst: u8, pool_idx: u32) -> Insn {
    ins(Op::ConstF64, dst, 0, 0, pool_idx)
}

#[inline]
fn add_f64(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::AddF64, dst, a, b, 0)
}

#[inline]
fn mul_f64(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::MulF64, dst, a, b, 0)
}

#[inline]
fn div_f64(dst: u8, a: u8, b: u8) -> Insn {
    ins(Op::DivF64, dst, a, b, 0)
}

pub fn build_prelude_module() -> Module {
    let type_ctx = TypeCtx::new_program_base();
    let types = type_ctx.types.clone();
    let sigs = type_ctx.sigs.clone();

    let const_f64 = vec![0.5];
    let mut funcs = prelude_funcs_for_program();

    // fun2: __prelude_smoke() -> bytes  (entry)
    let f2 = Function {
        reg_types: vec![T_BYTES, T_I32],
        cap_start: 0,
        insns: vec![
            const_i32(1, 0),
            bytes_new(0, 1),
            ret(0),
        ],
    };
    funcs.push(f2);

    Module {
        types,
        sigs,
        atoms: vec![],
        const_i64: vec![],
        const_f64,
        const_bytes: vec![],
        funcs,
        entry: PRELUDE_FUN_COUNT,
        prelude_count: PRELUDE_FUN_COUNT,
        used_prelude: vec![],
    }
}

/// Prelude bytecode functions. Returns all four (bytes_concat2, concat_many, slice, eq).
/// Use `prelude_funcs_for_used` to emit only a subset.
pub fn prelude_funcs_for_program() -> Vec<Function> {
    prelude_funcs_for_used(&[1, 2, 3, 4])
}

/// Prelude bytecode functions for the given logical indices (1..=4).
/// `used` must be sorted ascending. Returns funcs in order of `used`.
pub fn prelude_funcs_for_used(used: &[u32]) -> Vec<Function> {
    let f0 = Function {
        reg_types: vec![T_BYTES, T_BYTES, T_BYTES],
        cap_start: 0,
        insns: vec![
            bytes_concat2(2, 0, 1),
            ret(2),
        ],
    };

    let f1 = Function {
        reg_types: vec![T_ARRAY_BYTES, T_BYTES],
        cap_start: 0,
        insns: vec![
            bytes_concat_many(1, 0),
            ret(1),
        ],
    };

    // bytes_slice(bytes, start, len) -> bytes
    const SLICE_IN: u8 = 0;
    const SLICE_START: u8 = 1;
    const SLICE_LEN: u8 = 2;
    const SLICE_OUT: u8 = 3;
    const SLICE_I: u8 = 4;
    const SLICE_I_LT_LEN: u8 = 5;
    const SLICE_SRC_IDX: u8 = 6;
    const SLICE_ONE: u8 = 7;
    const SLICE_BYTE: u8 = 8;
    const SLICE_I_NEXT: u8 = 9;

    let f2 = Function {
        reg_types: vec![T_BYTES, T_I32, T_I32, T_BYTES, T_I32, T_BOOL, T_I32, T_I32, T_I32, T_I32],
        cap_start: 0,
        insns: vec![
            bytes_new(SLICE_OUT, SLICE_LEN),
            const_i32(SLICE_I, 0),
            const_i32(SLICE_ONE, 1),
            lt_i32(SLICE_I_LT_LEN, SLICE_I, SLICE_LEN),
            jmp_if(SLICE_I_LT_LEN, 1), // to loop body
            jmp(6),                // to return
            add_i32(SLICE_SRC_IDX, SLICE_START, SLICE_I),
            bytes_get_u8(SLICE_BYTE, SLICE_IN, SLICE_SRC_IDX),
            bytes_set_u8(SLICE_BYTE, SLICE_OUT, SLICE_I),
            add_i32(SLICE_I_NEXT, SLICE_I, SLICE_ONE),
            mov(SLICE_I, SLICE_I_NEXT),
            jmp(-9), // back to `lt_i32`
            ret(SLICE_OUT),
        ],
    };

    // bytes_eq(a, b) -> bool
    const EQ_A: u8 = 0;
    const EQ_B: u8 = 1;
    const EQ_LEN_A: u8 = 2;
    const EQ_LEN_B: u8 = 3;
    const EQ_LEN_EQ: u8 = 4;
    const EQ_I: u8 = 5;
    const EQ_ONE: u8 = 6;
    const EQ_I_LT_LEN: u8 = 7;
    const EQ_BYTE_A: u8 = 8;
    const EQ_BYTE_B: u8 = 9;
    const EQ_BYTE_EQ: u8 = 10;

    let f3 = Function {
        reg_types: vec![
            T_BYTES, T_BYTES, T_I32, T_I32, T_BOOL, T_I32, T_I32, T_BOOL, T_I32, T_I32, T_BOOL,
        ],
        cap_start: 0,
        insns: vec![
            bytes_len(EQ_LEN_A, EQ_A),
            bytes_len(EQ_LEN_B, EQ_B),
            eq_i32(EQ_LEN_EQ, EQ_LEN_A, EQ_LEN_B),
            jmp_if(EQ_LEN_EQ, 2), // to loop init
            const_bool(EQ_LEN_EQ, false),
            ret(EQ_LEN_EQ),
            const_i32(EQ_I, 0),
            const_i32(EQ_ONE, 1),
            lt_i32(EQ_I_LT_LEN, EQ_I, EQ_LEN_A),
            jmp_if(EQ_I_LT_LEN, 2), // to byte compare
            const_bool(EQ_I_LT_LEN, true),
            ret(EQ_I_LT_LEN),
            bytes_get_u8(EQ_BYTE_A, EQ_A, EQ_I),
            bytes_get_u8(EQ_BYTE_B, EQ_B, EQ_I),
            eq_i32(EQ_BYTE_EQ, EQ_BYTE_A, EQ_BYTE_B),
            jmp_if(EQ_BYTE_EQ, 2), // to i++
            const_bool(EQ_BYTE_EQ, false),
            ret(EQ_BYTE_EQ),
            add_i32(EQ_I, EQ_I, EQ_ONE),
            jmp(-12), // back to `lt_i32`
        ],
    };

    let mut result = Vec::with_capacity(used.len());
    for &idx in used {
        match idx {
            1 => result.push(f0.clone()),
            2 => result.push(f1.clone()),
            3 => result.push(f2.clone()),
            4 => result.push(f3.clone()),
            _ => {}
        }
    }
    result
}
