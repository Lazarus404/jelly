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

// Bytecode format definitions and I/O (read/write).
// Kept separate from AST codegen per docs/compiler_architecture.md.
mod io_helpers;
mod module_abi;
mod module_io;
#[cfg(test)]
mod tests;

pub use module_abi::{extract_module_abi, ModuleAbi};

const MAGIC_JLYB: u32 = 0x4A4C5942;
const VERSION: u32 = 1;

// Native builtin indices (VM C implementations; not in module func table).
// Must match vm JELLY_NATIVE_BUILTIN_*.
pub const NATIVE_BUILTIN_MATH_SQRT: u32 = 0;
pub const NATIVE_BUILTIN_SYSTEM_EXIT: u32 = 1;
pub const NATIVE_BUILTIN_I32_TO_BYTES: u32 = 2;
pub const NATIVE_BUILTIN_F64_TO_BYTES: u32 = 3;
pub const NATIVE_BUILTIN_F64_IS_NAN: u32 = 4;
pub const NATIVE_BUILTIN_F64_IS_INFINITE: u32 = 5;
pub const NATIVE_BUILTIN_COUNT: u32 = 6;

// Prelude (bytecode) function indices. Logical index = NATIVE_BUILTIN_COUNT + prelude_offset.
// Only include prelude funcs that are used. Keep in sync with `prelude_funcs_for_program()`.
#[allow(dead_code)]
pub const PRELUDE_BYTES_CONCAT2: u32 = NATIVE_BUILTIN_COUNT + 0;
#[allow(dead_code)]
pub const PRELUDE_BYTES_CONCAT_MANY: u32 = NATIVE_BUILTIN_COUNT + 1;
pub const PRELUDE_BYTES_SLICE: u32 = NATIVE_BUILTIN_COUNT + 2;
pub const PRELUDE_BYTES_EQ: u32 = NATIVE_BUILTIN_COUNT + 3;
pub const PRELUDE_FUN_COUNT: u32 = 4; // bytecode prelude funcs (no math_sqrt)

// Reserved atom ids (must match the compiler's atom table prefix).
pub const ATOM___PROTO__: u32 = 0;
pub const ATOM_INIT: u32 = 1;

// Must match vm/src/include/jelly.h (jelly_type_kind)
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[allow(dead_code)]
pub enum TypeKind {
    Bool = 1,
    Atom = 2,
    I8 = 3,
    I16 = 4,
    I32 = 5,
    I64 = 6,
    F16 = 7,
    F32 = 8,
    F64 = 9,
    Bytes = 10,
    List = 11,
    Array = 12,
    Object = 13,
    Function = 14,
    Abstract = 15,
    Dynamic = 16,
}

// Must match vm/src/include/jelly.h (jelly_op)
#[repr(u8)]
#[derive(Clone, Copy)]
#[allow(dead_code)]
pub enum Op {
    // Control / misc
    Nop = 0,
    Ret = 1,
    Jmp = 2,
    JmpIf = 3,
    Mov = 4,
    Try = 5,
    EndTry = 6,
    Throw = 7,
    Assert = 8,

    // Calls / closures
    Call = 9,
    CallR = 10,
    TailCall = 11,
    TailCallR = 12,
    ConstFun = 13,
    Closure = 14,
    BindThis = 15,

    // Typed constants
    ConstI32 = 16,
    ConstI8Imm = 17,
    ConstBool = 18,
    ConstNull = 19,
    ConstAtom = 20,
    ConstF16 = 21,
    ConstF32 = 22,
    ConstI64 = 23,
    ConstF64 = 24,
    ConstBytes = 25,

    // Bytes helpers
    BytesConcat2 = 26,
    BytesConcatMany = 27,

    // I32 arithmetic
    AddI32 = 28,
    SubI32 = 29,
    MulI32 = 30,
    DivI32 = 31,
    ModI32 = 32,
    ShlI32 = 33,
    ShrI32 = 34,
    AddI32Imm = 35,
    SubI32Imm = 36,
    MulI32Imm = 37,

    // I64 arithmetic
    AddI64 = 38,
    SubI64 = 39,
    MulI64 = 40,
    DivI64 = 41,
    ModI64 = 42,
    ShlI64 = 43,
    ShrI64 = 44,

    // Float arithmetic
    AddF16 = 45,
    SubF16 = 46,
    MulF16 = 47,
    AddF32 = 48,
    SubF32 = 49,
    MulF32 = 50,
    DivF32 = 51,
    AddF64 = 52,
    SubF64 = 53,
    MulF64 = 54,
    DivF64 = 55,

    // Unary
    NegI32 = 56,
    NegI64 = 57,
    NegF32 = 58,
    NegF64 = 59,
    NotBool = 60,

    // Comparisons
    EqI32 = 61,
    LtI32 = 62,
    EqI32Imm = 63,
    LtI32Imm = 64,
    EqI64 = 65,
    LtI64 = 66,
    EqF32 = 67,
    LtF32 = 68,
    EqF64 = 69,
    LtF64 = 70,

    // Conversions / width changes
    SextI64 = 71,
    SextI16 = 72,
    TruncI8 = 73,
    TruncI16 = 74,
    I32FromI64 = 75,
    F64FromI32 = 76,
    I32FromF64 = 77,
    F64FromI64 = 78,
    I64FromF64 = 79,
    F32FromI32 = 80,
    I32FromF32 = 81,
    F64FromF32 = 82,
    F32FromF64 = 83,
    F32FromI64 = 84,
    I64FromF32 = 85,
    F16FromF32 = 86,
    F32FromF16 = 87,
    F16FromI32 = 88,
    I32FromF16 = 89,

    // Boxing/unboxing boundary + spill
    ToDyn = 90,
    FromDynI8 = 91,
    FromDynI16 = 92,
    FromDynI32 = 93,
    FromDynI64 = 94,
    FromDynF16 = 95,
    FromDynF32 = 96,
    FromDynF64 = 97,
    FromDynBool = 98,
    FromDynAtom = 99,
    FromDynPtr = 100,
    SpillPush = 101,
    SpillPop = 102,

    // Identity / type introspection
    Physeq = 103,
    Kindof = 104,
    SwitchKind = 105,
    CaseKind = 106,

    // Containers
    ListNil = 107,
    ListCons = 108,
    ListHead = 109,
    ListTail = 110,
    ListIsNil = 111,
    ArrayNew = 112,
    ArrayLen = 113,
    ArrayGet = 114,
    ArraySet = 115,
    BytesNew = 116,
    BytesLen = 117,
    BytesGetU8 = 118,
    BytesSetU8 = 119,
    ObjNew = 120,
    ObjHasAtom = 121,
    ObjGetAtom = 122,
    ObjSetAtom = 123,
    ObjGet = 124,
    ObjSet = 125,
}

#[derive(Clone, Copy, Debug)]
pub struct TypeEntry {
    pub kind: TypeKind,
    pub p0: u32,
}

#[derive(Clone, Debug)]
pub struct FunSig {
    pub ret_type: u32,
    pub args: Vec<u32>,
}

#[derive(Clone, Copy)]
pub struct Insn {
    pub op: u8,
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub imm: u32,
}

#[derive(Clone)]
pub struct Function {
    pub reg_types: Vec<u32>,
    /// First capture slot index; used when JELLY_BC_FEAT_CAP_START.
    pub cap_start: u32,
    pub insns: Vec<Insn>,
}

#[derive(Clone)]
pub struct Module {
    pub types: Vec<TypeEntry>,
    pub sigs: Vec<FunSig>,
    pub atoms: Vec<Vec<u8>>,
    pub const_i64: Vec<i64>,
    pub const_f64: Vec<f64>,
    pub const_bytes: Vec<Vec<u8>>,
    pub funcs: Vec<Function>,
    pub entry: u32,
    /// Number of prelude bytecode funcs at start of funcs. Used by linker to skip when merging.
    pub prelude_count: u32,
    /// For variable prelude: logical indices 1..=prelude_count map to these full prelude indices (1..=4).
    /// Empty means full prelude. Set by IR backend when using prelude-on-demand.
    pub used_prelude: Vec<u32>,
}

// tests moved to `format/tests.rs`
// module ABI extraction moved to `format/module_abi.rs`
