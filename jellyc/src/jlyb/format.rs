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
pub const NATIVE_BUILTIN_COUNT: u32 = 4;

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
    Nop = 0,

    // Control / misc
    Ret = 1,

    // Moves
    Mov = 2,

    // Calls / closures
    Call = 3,
    CallR = 4,
    ConstFun = 5,
    Closure = 6,
    BindThis = 7,

    // Typed constants
    ConstI32 = 8,
    ConstI8Imm = 9,
    ConstBool = 10,
    ConstNull = 11,
    ConstAtom = 12,
    ConstF16 = 13,
    ConstF32 = 14,
    ConstI64 = 15,
    ConstF64 = 16,
    ConstBytes = 17,

    // Bytes helpers
    BytesConcat2 = 18,
    BytesConcatMany = 19,

    // Control flow
    Jmp = 20,
    JmpIf = 21,

    // Exceptions
    Try = 22,
    EndTry = 23,
    Throw = 24,

    // Debug
    Assert = 25,

    // Arithmetic (ints)
    AddI32 = 26,
    SubI32 = 27,
    MulI32 = 28,
    DivI32 = 29,
    AddI32Imm = 30,
    SubI32Imm = 31,
    MulI32Imm = 32,
    AddI64 = 33,
    SubI64 = 34,
    MulI64 = 35,
    DivI64 = 36,

    // Arithmetic (floats)
    AddF16 = 37,
    SubF16 = 38,
    MulF16 = 39,
    AddF32 = 40,
    SubF32 = 41,
    MulF32 = 42,
    DivF32 = 43,
    AddF64 = 44,
    SubF64 = 45,
    MulF64 = 46,
    DivF64 = 47,

    // Unary
    NegI32 = 48,
    NegI64 = 49,
    NegF32 = 50,
    NegF64 = 51,
    NotBool = 52,

    // Comparisons
    EqI32 = 53,
    LtI32 = 54,
    EqI32Imm = 55,
    LtI32Imm = 56,
    EqI64 = 57,
    EqF32 = 58,
    EqF64 = 59,

    // Boxing/unboxing boundary + spill
    ToDyn = 60,
    FromDynI8 = 61,
    FromDynI16 = 62,
    FromDynI32 = 63,
    FromDynI64 = 64,
    FromDynF16 = 65,
    FromDynF32 = 66,
    FromDynF64 = 67,
    FromDynBool = 68,
    FromDynAtom = 69,
    FromDynPtr = 70,
    SpillPush = 71,
    SpillPop = 72,

    // Conversions / width changes
    SextI64 = 73,
    I32FromI64 = 74,
    F64FromI32 = 75,
    I32FromF64 = 76,
    F64FromI64 = 77,
    I64FromF64 = 78,
    F32FromI32 = 79,
    I32FromF32 = 80,
    F64FromF32 = 81,
    F32FromF64 = 82,
    F32FromI64 = 83,
    I64FromF32 = 84,
    SextI16 = 85,
    TruncI8 = 86,
    TruncI16 = 87,
    F16FromF32 = 88,
    F32FromF16 = 89,
    F16FromI32 = 90,
    I32FromF16 = 91,

    // Identity
    Physeq = 92,

    // Match / type introspection
    Kindof = 93,
    SwitchKind = 94,
    CaseKind = 95,

    // Containers
    ListNil = 96,
    ListCons = 97,
    ListHead = 98,
    ListTail = 99,
    ListIsNil = 100,
    ArrayNew = 101,
    ArrayLen = 102,
    ArrayGet = 103,
    ArraySet = 104,
    BytesNew = 105,
    BytesLen = 106,
    BytesGetU8 = 107,
    BytesSetU8 = 108,
    ObjNew = 109,
    ObjHasAtom = 110,
    ObjGetAtom = 111,
    ObjSetAtom = 112,
    ObjGet = 113,
    ObjSet = 114,

    // Comparisons (extended; appended to keep opcode numbers stable)
    LtI64 = 115,
    LtF32 = 116,
    LtF64 = 117,

    // Tail calls: replace current frame instead of pushing (proper tail recursion)
    TailCall = 118,
    TailCallR = 119,
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
