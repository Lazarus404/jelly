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

use std::collections::HashMap;
use std::io::{self, Read, Write};

const MAGIC_JLYB: u32 = 0x4A4C5942;
const VERSION: u32 = 1;

// Native builtin indices (VM C implementations; not in module func table).
// Must match vm JELLY_NATIVE_BUILTIN_*.
pub const NATIVE_BUILTIN_MATH_SQRT: u32 = 0;
pub const NATIVE_BUILTIN_COUNT: u32 = 1;

// Prelude (bytecode) function indices. Logical index = NATIVE_BUILTIN_COUNT + prelude_offset.
// Only include prelude funcs that are used. Keep in sync with `prelude_funcs_for_program()`.
pub const PRELUDE_BYTES_CONCAT2: u32 = 1;   // NATIVE_BUILTIN_COUNT + 0
pub const PRELUDE_BYTES_CONCAT_MANY: u32 = 2;
pub const PRELUDE_BYTES_SLICE: u32 = 3;
pub const PRELUDE_BYTES_EQ: u32 = 4;
pub const PRELUDE_FUN_COUNT: u32 = 4;  // bytecode prelude funcs (no math_sqrt)

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

fn wr_u8<W: Write>(w: &mut W, v: u8) -> io::Result<()> {
    w.write_all(&[v])
}
fn wr_u16le<W: Write>(w: &mut W, v: u16) -> io::Result<()> {
    w.write_all(&v.to_le_bytes())
}
fn wr_u32le<W: Write>(w: &mut W, v: u32) -> io::Result<()> {
    w.write_all(&v.to_le_bytes())
}

impl Module {
    pub fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
        let feat_cap_start = self.funcs.iter().any(|f| f.cap_start > 0);
        let features: u32 =
            (if self.const_i64.is_empty() && self.const_f64.is_empty() { 0 } else { 1u32 << 0 }) // CONST64
            | (if self.const_bytes.is_empty() { 0 } else { 1u32 << 1 }) // CONSTBYTES
            | (if feat_cap_start { 1u32 << 2 } else { 0 }); // CAP_START

        wr_u32le(w, MAGIC_JLYB)?;
        wr_u32le(w, VERSION)?;
        wr_u32le(w, features)?;
        wr_u32le(w, self.types.len() as u32)?;
        wr_u32le(w, self.sigs.len() as u32)?;
        wr_u32le(w, self.atoms.len() as u32)?;
        wr_u32le(w, self.funcs.len() as u32)?;
        wr_u32le(w, self.entry)?;
        if (features & (1u32 << 0)) != 0 {
            wr_u32le(w, self.const_i64.len() as u32)?;
            wr_u32le(w, self.const_f64.len() as u32)?;
        }
        if (features & (1u32 << 1)) != 0 {
            wr_u32le(w, self.const_bytes.len() as u32)?;
        }

        for t in &self.types {
            wr_u8(w, t.kind as u8)?;
            wr_u8(w, 0)?;
            wr_u16le(w, 0)?;
            wr_u32le(w, t.p0)?;
            wr_u32le(w, 0)?;
        }

        for s in &self.sigs {
            wr_u32le(w, s.ret_type)?;
            wr_u16le(w, s.args.len() as u16)?;
            wr_u16le(w, 0)?;
            for &a in &s.args {
                wr_u32le(w, a)?;
            }
        }

        for a in &self.atoms {
            wr_u32le(w, a.len() as u32)?;
            w.write_all(a)?;
        }

        if (features & (1u32 << 0)) != 0 {
            for &x in &self.const_i64 {
                w.write_all(&x.to_le_bytes())?;
            }
            for &x in &self.const_f64 {
                w.write_all(&x.to_le_bytes())?;
            }
        }

        if (features & (1u32 << 1)) != 0 {
            for b in &self.const_bytes {
                wr_u32le(w, b.len() as u32)?;
                w.write_all(b)?;
            }
        }

        for f in &self.funcs {
            wr_u32le(w, f.reg_types.len() as u32)?;
            if feat_cap_start {
                wr_u32le(w, f.cap_start)?;
            }
            wr_u32le(w, f.insns.len() as u32)?;
            for &rt in &f.reg_types {
                wr_u32le(w, rt)?;
            }
            for ins in &f.insns {
                wr_u8(w, ins.op)?;
                wr_u8(w, ins.a)?;
                wr_u8(w, ins.b)?;
                wr_u8(w, ins.c)?;
                wr_u32le(w, ins.imm)?;
            }
        }

        Ok(())
    }

    pub fn read_from<R: Read>(r: &mut R) -> Result<Module, io::Error> {
        fn rd_u8<R: Read>(r: &mut R) -> Result<u8, io::Error> {
            let mut b = [0u8; 1];
            r.read_exact(&mut b)?;
            Ok(b[0])
        }
        fn rd_u16le<R: Read>(r: &mut R) -> Result<u16, io::Error> {
            let mut b = [0u8; 2];
            r.read_exact(&mut b)?;
            Ok(u16::from_le_bytes(b))
        }
        fn rd_u32le<R: Read>(r: &mut R) -> Result<u32, io::Error> {
            let mut b = [0u8; 4];
            r.read_exact(&mut b)?;
            Ok(u32::from_le_bytes(b))
        }

        let magic = rd_u32le(r)?;
        if magic != MAGIC_JLYB {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "bad magic"));
        }
        let version = rd_u32le(r)?;
        if version != VERSION {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "unsupported version"));
        }
        let features = rd_u32le(r)?;
        let ntypes = rd_u32le(r)? as usize;
        let nsigs = rd_u32le(r)? as usize;
        let natoms = rd_u32le(r)? as usize;
        let nfuncs = rd_u32le(r)? as usize;
        let entry = rd_u32le(r)?;

        let feat_const64 = (features & 1u32) != 0;
        let (nconst_i64, nconst_f64) = if feat_const64 {
            (rd_u32le(r)? as usize, rd_u32le(r)? as usize)
        } else {
            (0usize, 0usize)
        };
        let feat_constbytes = (features & (1u32 << 1)) != 0;
        let nconst_bytes = if feat_constbytes {
            rd_u32le(r)? as usize
        } else {
            0
        };

        let mut types: Vec<TypeEntry> = Vec::with_capacity(ntypes);
        for _ in 0..ntypes {
            let kind_u8 = rd_u8(r)?;
            let _pad0 = rd_u8(r)?;
            let _pad1 = rd_u16le(r)?;
            let p0 = rd_u32le(r)?;
            let _p1 = rd_u32le(r)?;
            let kind = match kind_u8 {
                1 => TypeKind::Bool,
                2 => TypeKind::Atom,
                3 => TypeKind::I8,
                4 => TypeKind::I16,
                5 => TypeKind::I32,
                6 => TypeKind::I64,
                7 => TypeKind::F16,
                8 => TypeKind::F32,
                9 => TypeKind::F64,
                10 => TypeKind::Bytes,
                11 => TypeKind::List,
                12 => TypeKind::Array,
                13 => TypeKind::Object,
                14 => TypeKind::Function,
                15 => TypeKind::Abstract,
                16 => TypeKind::Dynamic,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unknown type kind",
                    ))
                }
            };
            types.push(TypeEntry { kind, p0 });
        }

        let mut sigs: Vec<FunSig> = Vec::with_capacity(nsigs);
        for _ in 0..nsigs {
            let ret_type = rd_u32le(r)?;
            let nargs = rd_u16le(r)? as usize;
            let _pad = rd_u16le(r)?;
            let mut args: Vec<u32> = Vec::with_capacity(nargs);
            for _ in 0..nargs {
                args.push(rd_u32le(r)?);
            }
            sigs.push(FunSig { ret_type, args });
        }

        let mut atoms: Vec<Vec<u8>> = Vec::with_capacity(natoms);
        for _ in 0..natoms {
            let n = rd_u32le(r)? as usize;
            let mut b = vec![0u8; n];
            r.read_exact(&mut b)?;
            atoms.push(b);
        }

        let mut const_i64: Vec<i64> = Vec::with_capacity(nconst_i64);
        let mut const_f64: Vec<f64> = Vec::with_capacity(nconst_f64);
        if feat_const64 {
            for _ in 0..nconst_i64 {
                let mut b = [0u8; 8];
                r.read_exact(&mut b)?;
                const_i64.push(i64::from_le_bytes(b));
            }
            for _ in 0..nconst_f64 {
                let mut b = [0u8; 8];
                r.read_exact(&mut b)?;
                const_f64.push(f64::from_le_bytes(b));
            }
        }

        let mut const_bytes: Vec<Vec<u8>> = Vec::with_capacity(nconst_bytes);
        for _ in 0..nconst_bytes {
            let n = rd_u32le(r)? as usize;
            let mut b = vec![0u8; n];
            r.read_exact(&mut b)?;
            const_bytes.push(b);
        }

        let feat_cap_start = (features & (1u32 << 2)) != 0;
        let mut funcs: Vec<Function> = Vec::with_capacity(nfuncs);
        for _ in 0..nfuncs {
            let nregs = rd_u32le(r)? as usize;
            let cap_start = if feat_cap_start { rd_u32le(r)? } else { 0 };
            let ninsns = rd_u32le(r)? as usize;
            let mut reg_types: Vec<u32> = Vec::with_capacity(nregs);
            for _ in 0..nregs {
                reg_types.push(rd_u32le(r)?);
            }
            let mut insns: Vec<Insn> = Vec::with_capacity(ninsns);
            for _ in 0..ninsns {
                let op = rd_u8(r)?;
                let a = rd_u8(r)?;
                let b = rd_u8(r)?;
                let c = rd_u8(r)?;
                let imm = rd_u32le(r)?;
                insns.push(Insn { op, a, b, c, imm });
            }
            funcs.push(Function { reg_types, cap_start, insns });
        }

        Ok(Module {
            types,
            sigs,
            atoms,
            const_i64,
            const_f64,
            const_bytes,
            funcs,
            entry,
            prelude_count: PRELUDE_FUN_COUNT,
            used_prelude: vec![],
        })
    }
}

const MOD_ABI_MAGIC: &[u8] = b"JLYMODABI1\0";

#[derive(Clone, Debug)]
pub struct ModuleAbi {
    pub exports: HashMap<String, u32>,
    pub imports: Vec<String>,
}

pub fn extract_module_abi(m: &Module) -> Option<ModuleAbi> {
    fn rd_u32le(b: &[u8], i: &mut usize) -> Option<u32> {
        if *i + 4 > b.len() {
            return None;
        }
        let v = u32::from_le_bytes([b[*i], b[*i + 1], b[*i + 2], b[*i + 3]]);
        *i += 4;
        Some(v)
    }

    for blob in &m.const_bytes {
        if !blob.starts_with(MOD_ABI_MAGIC) {
            continue;
        }
        let mut i = MOD_ABI_MAGIC.len();
        let n = match rd_u32le(blob, &mut i) {
            Some(x) => x as usize,
            None => continue,
        };
        let mut exports: HashMap<String, u32> = HashMap::new();
        for _ in 0..n {
            let len = match rd_u32le(blob, &mut i) {
                Some(x) => x as usize,
                None => return None,
            };
            if i + len > blob.len() {
                return None;
            }
            let name_bytes = &blob[i..i + len];
            i += len;
            let tid = match rd_u32le(blob, &mut i) {
                Some(x) => x,
                None => return None,
            };
            if let Ok(s) = std::str::from_utf8(name_bytes) {
                exports.insert(s.to_string(), tid);
            }
        }

        let nimports = match rd_u32le(blob, &mut i) {
            Some(x) => x as usize,
            None => return None,
        };
        let mut imports: Vec<String> = Vec::with_capacity(nimports);
        for _ in 0..nimports {
            let len = match rd_u32le(blob, &mut i) {
                Some(x) => x as usize,
                None => return None,
            };
            if i + len > blob.len() {
                return None;
            }
            let b = &blob[i..i + len];
            i += len;
            let s = std::str::from_utf8(b).ok()?.to_string();
            imports.push(s);
        }

        return Some(ModuleAbi { exports, imports });
    }
    None
}
