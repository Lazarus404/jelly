use std::io::{self, Read, Write};

use std::collections::HashMap;

use crate::ast::{Expr, ExprKind, Program, Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::regalloc::{self, InstrInfo, SpillPolicy, VReg};
use crate::typectx::TypeCtx;

const MAGIC_JLYB: u32 = 0x4A4C5942;
const VERSION: u32 = 1;

// Prelude function indices (injected at the start of every emitted module).
// Keep in sync with `prelude_funcs_for_program()`.
pub const PRELUDE_BYTES_CONCAT2: u32 = 0;
pub const PRELUDE_BYTES_CONCAT_MANY: u32 = 1;
pub const PRELUDE_BYTES_SLICE: u32 = 2;
pub const PRELUDE_BYTES_EQ: u32 = 3;
pub const PRELUDE_FUN_COUNT: u32 = 4;

// Reserved atom ids (must match the compiler's atom table prefix).
// These are used by VM-level prototype delegation and `new` lowering.
pub const ATOM___PROTO__: u32 = 0;
pub const ATOM_INIT: u32 = 1;

// Must match vm/include/jelly/type.h
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum TypeKind {
    I8 = 1,
    I16 = 2,
    I32 = 3,
    I64 = 4,
    F32 = 5,
    F64 = 6,
    Bool = 7,
    Atom = 8,
    Bytes = 9,
    Dynamic = 10,
    Function = 11,
    List = 12,
    Array = 13,
    Object = 14,
    Abstract = 15,
}

// Must match vm/include/jelly/bytecode.h
#[repr(u8)]
#[derive(Clone, Copy)]
#[allow(dead_code)]
pub enum Op {
    Mov = 2,
    Ret = 1,
    Call = 3,
    CallR = 4,
    ConstI32 = 10,
    ConstBool = 11,
    ConstAtom = 12,
    ConstFun = 13,
    ConstNull = 96,
    ConstBytes = 57,
    Jmp = 14,
    JmpIf = 15,
    Closure = 16,
    BindThis = 17,
    BytesNew = 90,
    BytesConcat2 = 94,
    BytesConcatMany = 95,
    ListNil = 70,
    ListCons = 71,
    ListHead = 72,
    ListTail = 73,
    ListIsNil = 74,
    ArrayNew = 80,
    ArrayLen = 81,
    ArrayGet = 82,
    ArraySet = 83,
    BytesLen = 91,
    BytesGetU8 = 92,
    BytesSetU8 = 93,
    ObjNew = 100,
    ObjHasAtom = 101,
    ObjGetAtom = 102,
    ObjSetAtom = 103,
    ObjGet = 108,
    ObjSet = 109,
    Try = 58,
    EndTry = 59,
    Throw = 64,
    ToDyn = 40,
    FromDynI32 = 41,
    FromDynBool = 46,
    FromDynPtr = 48,
    SpillPush = 50,
    SpillPop = 51,
    NotBool = 56,
    Physeq = 60,
    Kindof = 61,
    SwitchKind = 62,
    CaseKind = 63,
    EqI32 = 34,
    AddI32 = 20,
    SubI32 = 21,
    NegI32 = 52,
    LtI32 = 65,
}

#[derive(Clone, Copy)]
pub struct TypeEntry {
    pub kind: TypeKind,
    pub p0: u32,
}

#[derive(Clone)]
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
    pub insns: Vec<Insn>,
}

#[derive(Clone)]
pub struct Module {
    pub types: Vec<TypeEntry>,
    pub sigs: Vec<FunSig>,
    pub atoms: Vec<Vec<u8>>,
    pub const_bytes: Vec<Vec<u8>>,
    pub funcs: Vec<Function>,
    pub entry: u32,
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
        let features: u32 = if self.const_bytes.is_empty() { 0 } else { 1u32 << 1 }; // CONSTBYTES

        // header
        wr_u32le(w, MAGIC_JLYB)?;
        wr_u32le(w, VERSION)?;
        wr_u32le(w, features)?;
        wr_u32le(w, self.types.len() as u32)?;
        wr_u32le(w, self.sigs.len() as u32)?;
        wr_u32le(w, self.atoms.len() as u32)?;
        wr_u32le(w, self.funcs.len() as u32)?;
        wr_u32le(w, self.entry)?;
        if (features & (1u32 << 1)) != 0 {
            wr_u32le(w, self.const_bytes.len() as u32)?;
        }

        // type table
        for t in &self.types {
            wr_u8(w, t.kind as u8)?;
            wr_u8(w, 0)?;
            wr_u16le(w, 0)?;
            wr_u32le(w, t.p0)?;
            wr_u32le(w, 0)?;
        }

        // sig table
        for s in &self.sigs {
            wr_u32le(w, s.ret_type)?;
            wr_u16le(w, s.args.len() as u16)?;
            wr_u16le(w, 0)?;
            for &a in &s.args {
                wr_u32le(w, a)?;
            }
        }

        // atoms section
        for a in &self.atoms {
            wr_u32le(w, a.len() as u32)?;
            w.write_all(a)?;
        }

        // bytes const pool
        if (features & (1u32 << 1)) != 0 {
            for b in &self.const_bytes {
                wr_u32le(w, b.len() as u32)?;
                w.write_all(b)?;
            }
        }

        // functions
        for f in &self.funcs {
            wr_u32le(w, f.reg_types.len() as u32)?;
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

        // NOTE: The Rust compiler currently only emits CONSTBYTES. If CONST64 is present, bail
        // for now (we don't store those pools in `Module` yet).
        let feat_const64 = (features & 1u32) != 0;
        if feat_const64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unsupported feature: CONST64",
            ));
        }
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
                1 => TypeKind::I8,
                2 => TypeKind::I16,
                3 => TypeKind::I32,
                4 => TypeKind::I64,
                5 => TypeKind::F32,
                6 => TypeKind::F64,
                7 => TypeKind::Bool,
                8 => TypeKind::Atom,
                9 => TypeKind::Bytes,
                10 => TypeKind::Dynamic,
                11 => TypeKind::Function,
                12 => TypeKind::List,
                13 => TypeKind::Array,
                14 => TypeKind::Object,
                15 => TypeKind::Abstract,
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

        let mut const_bytes: Vec<Vec<u8>> = Vec::with_capacity(nconst_bytes);
        for _ in 0..nconst_bytes {
            let n = rd_u32le(r)? as usize;
            let mut b = vec![0u8; n];
            r.read_exact(&mut b)?;
            const_bytes.push(b);
        }

        let mut funcs: Vec<Function> = Vec::with_capacity(nfuncs);
        for _ in 0..nfuncs {
            let nregs = rd_u32le(r)? as usize;
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
            funcs.push(Function { reg_types, insns });
        }

        Ok(Module {
            types,
            sigs,
            atoms,
            const_bytes,
            funcs,
            entry,
        })
    }
}

const MOD_ABI_MAGIC: &[u8] = b"JLYMODABI1\0";

#[derive(Clone, Debug)]
pub struct ModuleAbi {
    pub exports: HashMap<String, u32>, // export name -> type id (in this module)
    pub imports: Vec<String>,          // imported module keys, in param order
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

pub fn build_prelude_module() -> Module {
    // Keep the prelude module format aligned with the program base tables so it
    // can be injected into user modules without type-id remapping.
    let type_ctx = TypeCtx::new_program_base();
    let types = type_ctx.types.clone();
    let sigs = type_ctx.sigs.clone();

    let mut funcs = prelude_funcs_for_program();

    // fun2: __prelude_smoke() -> bytes  (entry)
    // regs: r0 bytes, r1 i32
    let f2 = Function {
        reg_types: vec![0, 2],
        insns: vec![
            Insn {
                op: Op::ConstI32 as u8,
                a: 1,
                b: 0,
                c: 0,
                imm: 0,
            },
            Insn {
                op: Op::BytesNew as u8,
                a: 0,
                b: 1,
                c: 0,
                imm: 0,
            },
            Insn {
                op: Op::Ret as u8,
                a: 0,
                b: 0,
                c: 0,
                imm: 0,
            },
        ],
    };
    funcs.push(f2);

    Module {
        types,
        sigs,
        atoms: vec![],
        const_bytes: vec![],
        funcs,
        entry: PRELUDE_FUN_COUNT, // after the prelude funcs
    }
}

pub fn prelude_funcs_for_program() -> Vec<Function> {
    // Type IDs must match `TypeCtx::new_program_base()`:
    // - bytes = 0
    // - i32 = 2
    // - Array<bytes> = 5
    const T_BYTES: u32 = 0;
    const T_BOOL: u32 = 1;
    const T_I32: u32 = 2;
    const T_ARRAY_BYTES: u32 = 5;

    // fun0: __bytes_concat2(a,b) -> bytes
    // regs: r0 bytes (arg0), r1 bytes(arg1), r2 bytes(tmp)
    let f0 = Function {
        reg_types: vec![T_BYTES, T_BYTES, T_BYTES],
        insns: vec![
            Insn { op: Op::BytesConcat2 as u8, a: 2, b: 0, c: 1, imm: 0 },
            Insn { op: Op::Ret as u8, a: 2, b: 0, c: 0, imm: 0 },
        ],
    };

    // fun1: __bytes_concat_many(parts:Array<bytes>) -> bytes
    // regs: r0 Array<bytes> (arg0), r1 bytes(tmp)
    let f1 = Function {
        reg_types: vec![T_ARRAY_BYTES, T_BYTES],
        insns: vec![
            Insn { op: Op::BytesConcatMany as u8, a: 1, b: 0, c: 0, imm: 0 },
            Insn { op: Op::Ret as u8, a: 1, b: 0, c: 0, imm: 0 },
        ],
    };

    // fun2: __bytes_slice(src:bytes, start:i32, len:i32) -> bytes
    //
    // Copies `len` bytes from `src[start..start+len)` into a new bytes value.
    // Bounds are checked by the underlying VM ops (traps on OOB).
    //
    // regs:
    //  r0 bytes src
    //  r1 i32 start
    //  r2 i32 len
    //  r3 bytes dst
    //  r4 i32 i
    //  r5 bool cond
    //  r6 i32 src_i
    //  r7 i32 one
    //  r8 i32 byte
    //  r9 i32 i2
    let f2 = Function {
        reg_types: vec![T_BYTES, T_I32, T_I32, T_BYTES, T_I32, 1, T_I32, T_I32, T_I32, T_I32],
        insns: vec![
            // dst = BytesNew(len)
            Insn { op: Op::BytesNew as u8, a: 3, b: 2, c: 0, imm: 0 },
            // i = 0
            Insn { op: Op::ConstI32 as u8, a: 4, b: 0, c: 0, imm: 0 },
            // one = 1
            Insn { op: Op::ConstI32 as u8, a: 7, b: 0, c: 0, imm: 1 },
            // cond = (i < len)
            Insn { op: Op::LtI32 as u8, a: 5, b: 4, c: 2, imm: 0 },
            // if cond goto body
            Insn { op: Op::JmpIf as u8, a: 5, b: 0, c: 0, imm: 1 },
            // else goto exit
            Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: 6 },
            // body: src_i = start + i
            Insn { op: Op::AddI32 as u8, a: 6, b: 1, c: 4, imm: 0 },
            // byte = src[src_i]
            Insn { op: Op::BytesGetU8 as u8, a: 8, b: 0, c: 6, imm: 0 },
            // dst[i] = byte
            Insn { op: Op::BytesSetU8 as u8, a: 8, b: 3, c: 4, imm: 0 },
            // i2 = i + 1
            Insn { op: Op::AddI32 as u8, a: 9, b: 4, c: 7, imm: 0 },
            // i = i2
            Insn { op: Op::Mov as u8, a: 4, b: 9, c: 0, imm: 0 },
            // loop
            Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: (-9i32) as u32 },
            // exit: return dst
            Insn { op: Op::Ret as u8, a: 3, b: 0, c: 0, imm: 0 },
        ],
    };

    // fun3: __bytes_eq(a:bytes, b:bytes) -> bool
    //
    // Content equality; compares lengths then each byte.
    //
    // regs:
    //  r0 bytes a
    //  r1 bytes b
    //  r2 i32 lena
    //  r3 i32 lenb
    //  r4 bool tmp / result
    //  r5 i32 i
    //  r6 i32 one
    //  r7 bool cond / result
    //  r8 i32 byte_a
    //  r9 i32 byte_b
    //  r10 bool byte_eq / result
    let f3 = Function {
        reg_types: vec![
            T_BYTES, T_BYTES, T_I32, T_I32, T_BOOL, T_I32, T_I32, T_BOOL, T_I32, T_I32, T_BOOL,
        ],
        insns: vec![
            // lena = len(a)
            Insn { op: Op::BytesLen as u8, a: 2, b: 0, c: 0, imm: 0 },
            // lenb = len(b)
            Insn { op: Op::BytesLen as u8, a: 3, b: 1, c: 0, imm: 0 },
            // tmp = (lena == lenb)
            Insn { op: Op::EqI32 as u8, a: 4, b: 2, c: 3, imm: 0 },
            // if tmp goto len_ok else ret false
            Insn { op: Op::JmpIf as u8, a: 4, b: 0, c: 0, imm: 2 },
            // false
            Insn { op: Op::ConstBool as u8, a: 4, b: 0, c: 0, imm: 0 },
            Insn { op: Op::Ret as u8, a: 4, b: 0, c: 0, imm: 0 },

            // len_ok:
            // i = 0
            Insn { op: Op::ConstI32 as u8, a: 5, b: 0, c: 0, imm: 0 },
            // one = 1
            Insn { op: Op::ConstI32 as u8, a: 6, b: 0, c: 0, imm: 1 },

            // loop_cond:
            // cond = (i < lena)
            Insn { op: Op::LtI32 as u8, a: 7, b: 5, c: 2, imm: 0 },
            // if cond goto body else ret true
            Insn { op: Op::JmpIf as u8, a: 7, b: 0, c: 0, imm: 2 },
            // true
            Insn { op: Op::ConstBool as u8, a: 7, b: 0, c: 1, imm: 0 },
            Insn { op: Op::Ret as u8, a: 7, b: 0, c: 0, imm: 0 },

            // body:
            Insn { op: Op::BytesGetU8 as u8, a: 8, b: 0, c: 5, imm: 0 },
            Insn { op: Op::BytesGetU8 as u8, a: 9, b: 1, c: 5, imm: 0 },
            Insn { op: Op::EqI32 as u8, a: 10, b: 8, c: 9, imm: 0 },
            // if byte_eq goto inc else ret false
            Insn { op: Op::JmpIf as u8, a: 10, b: 0, c: 0, imm: 2 },
            Insn { op: Op::ConstBool as u8, a: 10, b: 0, c: 0, imm: 0 },
            Insn { op: Op::Ret as u8, a: 10, b: 0, c: 0, imm: 0 },

            // inc:
            Insn { op: Op::AddI32 as u8, a: 5, b: 5, c: 6, imm: 0 },
            Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: (-12i32) as u32 },
        ],
    };

    vec![f0, f1, f2, f3]
}

pub fn build_concat_literals_module(parts: &[Vec<u8>]) -> Module {
    assert!(!parts.is_empty());

    // Types:
    // 0 bytes, 1 i32, 2 Array<bytes>
    let types = vec![
        TypeEntry {
            kind: TypeKind::Bytes,
            p0: 0,
        },
        TypeEntry {
            kind: TypeKind::I32,
            p0: 0,
        },
        TypeEntry {
            kind: TypeKind::Array,
            p0: 0, // elem type_id = bytes
        },
    ];

    let const_bytes = parts.to_vec();

    // --- build a tiny SSA-style vreg program (typed) --------------------------
    //
    // Jelly VM regs are statically typed; therefore register allocation must be
    // type-aware (a physical reg cannot change type). We allocate per type_id
    // and then pack reg classes into a single 0..nregs range.
    //
    // Types here are module type_ids:
    // 0 bytes, 1 i32, 2 Array<bytes>
    #[derive(Clone, Copy)]
    struct VTmp {
        v: u32,
        tid: u32,
    }

    #[derive(Clone, Copy)]
    enum Opnd {
        V(u32),
        Z,
    }

    #[derive(Clone, Copy)]
    struct VInsn {
        op: Op,
        a: VTmp, // always a def for our subset (except for Ret/ArraySet where we special-case)
        b: Opnd,
        c: Opnd,
        imm: u32,
        // For ops where destination is not "a" (RET/ARRAY_SET), a.tid still carries useful info.
    }

    struct VAlloc {
        next_v: u32,
        vtypes: Vec<u32>, // index by vreg -> type_id
    }
    impl VAlloc {
        fn new() -> Self {
            Self { next_v: 0, vtypes: Vec::new() }
        }
        fn new_v(&mut self, tid: u32) -> VTmp {
            let v = self.next_v;
            self.next_v += 1;
            if v as usize == self.vtypes.len() {
                self.vtypes.push(tid);
            } else {
                self.vtypes[v as usize] = tid;
            }
            VTmp { v, tid }
        }
    }
    let mut alloc = VAlloc::new();

    let mut vinsns: Vec<VInsn> = Vec::new();
    let mut infos: Vec<InstrInfo> = Vec::new();

    let n = parts.len();
    if n == 1 {
        let v0 = alloc.new_v(0);
        vinsns.push(VInsn {
            op: Op::ConstBytes,
            a: v0,
            b: Opnd::Z,
            c: Opnd::Z,
            imm: 0,
        });
        infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v0.v)] });

        // ret v0
        vinsns.push(VInsn {
            op: Op::Ret,
            a: v0,
            b: Opnd::Z,
            c: Opnd::Z,
            imm: 0,
        });
        infos.push(InstrInfo { uses: vec![VReg(v0.v)], defs: vec![] });
    } else if n == 2 {
        let v0 = alloc.new_v(0);
        let v1 = alloc.new_v(0);
        let v2 = alloc.new_v(0);

        vinsns.push(VInsn { op: Op::ConstBytes, a: v0, b: Opnd::Z, c: Opnd::Z, imm: 0 });
        infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v0.v)] });
        vinsns.push(VInsn { op: Op::ConstBytes, a: v1, b: Opnd::Z, c: Opnd::Z, imm: 1 });
        infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v1.v)] });

        vinsns.push(VInsn {
            op: Op::BytesConcat2,
            a: v2,
            b: Opnd::V(v0.v),
            c: Opnd::V(v1.v),
            imm: 0,
        });
        infos.push(InstrInfo { uses: vec![VReg(v0.v), VReg(v1.v)], defs: vec![VReg(v2.v)] });

        vinsns.push(VInsn { op: Op::Ret, a: v2, b: Opnd::Z, c: Opnd::Z, imm: 0 });
        infos.push(InstrInfo { uses: vec![VReg(v2.v)], defs: vec![] });
    } else {
        // bytes literals
        let mut bytes_vs: Vec<VTmp> = Vec::with_capacity(n);
        for _ in 0..n {
            bytes_vs.push(alloc.new_v(0));
        }
        for (i, v) in bytes_vs.iter().enumerate() {
            vinsns.push(VInsn { op: Op::ConstBytes, a: *v, b: Opnd::Z, c: Opnd::Z, imm: i as u32 });
            infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v.v)] });
        }

        // len (i32)
        let v_len = alloc.new_v(1);
        vinsns.push(VInsn { op: Op::ConstI32, a: v_len, b: Opnd::Z, c: Opnd::Z, imm: n as u32 });
        infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_len.v)] });

        // arr (Array<bytes>)
        let v_arr = alloc.new_v(2);
        vinsns.push(VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_len.v), c: Opnd::Z, imm: 0 });
        infos.push(InstrInfo { uses: vec![VReg(v_len.v)], defs: vec![VReg(v_arr.v)] });

        // array_set for each element (idx is a fresh i32 vreg each time; SSA)
        for (i, bv) in bytes_vs.iter().enumerate() {
            let v_idx = alloc.new_v(1);
            vinsns.push(VInsn { op: Op::ConstI32, a: v_idx, b: Opnd::Z, c: Opnd::Z, imm: i as u32 });
            infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_idx.v)] });

            // ARRAY_SET is special: it uses a(bytes), b(array), c(idx) and has no def.
            vinsns.push(VInsn { op: Op::ArraySet, a: *bv, b: Opnd::V(v_arr.v), c: Opnd::V(v_idx.v), imm: 0 });
            infos.push(InstrInfo { uses: vec![VReg(bv.v), VReg(v_arr.v), VReg(v_idx.v)], defs: vec![] });
        }

        // out bytes = concat_many(arr)
        let v_out = alloc.new_v(0);
        vinsns.push(VInsn { op: Op::BytesConcatMany, a: v_out, b: Opnd::V(v_arr.v), c: Opnd::Z, imm: 0 });
        infos.push(InstrInfo { uses: vec![VReg(v_arr.v)], defs: vec![VReg(v_out.v)] });

        vinsns.push(VInsn { op: Op::Ret, a: v_out, b: Opnd::Z, c: Opnd::Z, imm: 0 });
        infos.push(InstrInfo { uses: vec![VReg(v_out.v)], defs: vec![] });
    }

    // --- type-aware LSRA ------------------------------------------------------
    let num_vregs = alloc.next_v;
    let vtypes = alloc.vtypes;
    let mut type_ids: Vec<u32> = vtypes.iter().copied().collect();
    type_ids.sort();
    type_ids.dedup();

    // global vreg -> final physical reg index (u8)
    let mut vreg_to_reg: Vec<u8> = vec![0; num_vregs as usize];

    // final reg_types (per physical reg index)
    let mut reg_types: Vec<u32> = Vec::new();

    let mut base: u16 = 0;
    for tid in type_ids {
        // Dense remap for vregs of this type.
        let mut globals: Vec<u32> = Vec::new();
        for (gv, &t) in vtypes.iter().enumerate() {
            if t == tid {
                globals.push(gv as u32);
            }
        }
        let mut g2l: Vec<Option<u32>> = vec![None; num_vregs as usize];
        for (li, &gv) in globals.iter().enumerate() {
            g2l[gv as usize] = Some(li as u32);
        }

        let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
        for ins in &infos {
            let mut uses = Vec::new();
            let mut defs = Vec::new();
            for &u in &ins.uses {
                if let Some(li) = g2l[u.0 as usize] {
                    uses.push(VReg(li));
                }
            }
            for &d in &ins.defs {
                if let Some(li) = g2l[d.0 as usize] {
                    defs.push(VReg(li));
                }
            }
            cls_instrs.push(InstrInfo { uses, defs });
        }

        let spillable = vec![false; globals.len()];
                let alloc = regalloc::lsra_allocate(256, globals.len() as u32, &cls_instrs, &spillable, SpillPolicy::Forbid, None)
            .expect("LSRA allocation failed for typed class");

        let used = alloc.used_pregs;
        if base as u32 + used as u32 > 256 {
            panic!("register allocation exceeded 256 regs");
        }

        // Extend reg_types for this class.
        let need = base as usize + used as usize;
        if reg_types.len() < need {
            reg_types.resize(need, 0);
        }
        for i in 0..used {
            reg_types[base as usize + i as usize] = tid;
        }

        for (li, &gv) in globals.iter().enumerate() {
            let p = alloc.vreg_to_preg[li].expect("unexpected spill with SpillPolicy::Forbid");
            let final_r = base + p.0;
            vreg_to_reg[gv as usize] = final_r as u8;
        }

        base += used;
    }

    // --- emit bytecode with allocated regs -----------------------------------
    let mut insns: Vec<Insn> = Vec::with_capacity(vinsns.len());
    for vi in &vinsns {
        match vi.op {
            Op::Ret => {
                insns.push(Insn { op: Op::Ret as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            Op::ArraySet => {
                // a=value(bytes), b=array, c=index
                let b = match vi.b { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                let c = match vi.c { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                insns.push(Insn {
                    op: Op::ArraySet as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b,
                    c,
                    imm: 0,
                });
            }
            _ => {
                let b = match vi.b { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                let c = match vi.c { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                insns.push(Insn {
                    op: vi.op as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b,
                    c,
                    imm: vi.imm,
                });
            }
        }
    }

    let f0 = Function { reg_types, insns };
    Module {
        types,
        sigs: vec![],
        atoms: vec![],
        const_bytes,
        funcs: vec![f0],
        entry: 0,
    }
}

pub fn build_let_if_concat_module(init: Vec<u8>, cond: bool, then_rhs: Vec<u8>, else_rhs: Vec<u8>) -> Module {
    // Types:
    // 0 bytes, 1 bool
    let types = vec![
        TypeEntry { kind: TypeKind::Bytes, p0: 0 },
        TypeEntry { kind: TypeKind::Bool, p0: 0 },
    ];

    // const_bytes: [init, then_rhs, else_rhs]
    let const_bytes = vec![init, then_rhs, else_rhs];

    // SSA vreg program:
    // v_init = const_bytes[0]
    // v_cond = const_bool
    // jmp_if v_cond -> then_label
    //   v_else = const_bytes[2]
    //   v_out0 = concat2(v_init, v_else)
    //   ret v_out0
    // then_label:
    //   v_then = const_bytes[1]
    //   v_out1 = concat2(v_init, v_then)
    //   ret v_out1
    #[derive(Clone, Copy)]
    struct VTmp {
        v: u32,
    }
    #[derive(Clone, Copy)]
    enum Opnd {
        V(u32),
        Z,
    }
    #[derive(Clone, Copy)]
    struct VInsn {
        op: Op,
        a: VTmp,
        b: Opnd,
        c: Opnd,
        imm: u32,
    }

    struct VAlloc {
        next_v: u32,
        vtypes: Vec<u32>,
    }
    impl VAlloc {
        fn new() -> Self {
            Self { next_v: 0, vtypes: Vec::new() }
        }
        fn new_v(&mut self, tid: u32) -> VTmp {
            let v = self.next_v;
            self.next_v += 1;
            if v as usize == self.vtypes.len() {
                self.vtypes.push(tid);
            } else {
                self.vtypes[v as usize] = tid;
            }
            VTmp { v }
        }
    }
    let mut alloc = VAlloc::new();

    let v_init = alloc.new_v(0);
    let v_cond = alloc.new_v(1);
    let v_else = alloc.new_v(0);
    let v_out0 = alloc.new_v(0);
    let v_then = alloc.new_v(0);
    let v_out1 = alloc.new_v(0);

    let mut vinsns: Vec<VInsn> = Vec::new();
    let mut infos: Vec<InstrInfo> = Vec::new();

    vinsns.push(VInsn { op: Op::ConstBytes, a: v_init, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_init.v)] });

    vinsns.push(VInsn { op: Op::ConstBool, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: if cond { 1 } else { 0 } });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_cond.v)] });

    // We'll patch the jump delta after we know pc positions; keep as imm=0 for vreg pass.
    vinsns.push(VInsn { op: Op::JmpIf, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_cond.v)], defs: vec![] });

    // else block
    vinsns.push(VInsn { op: Op::ConstBytes, a: v_else, b: Opnd::Z, c: Opnd::Z, imm: 2 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_else.v)] });

    vinsns.push(VInsn { op: Op::BytesConcat2, a: v_out0, b: Opnd::V(v_init.v), c: Opnd::V(v_else.v), imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_init.v), VReg(v_else.v)], defs: vec![VReg(v_out0.v)] });

    vinsns.push(VInsn { op: Op::Ret, a: v_out0, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_out0.v)], defs: vec![] });

    // then label
    let then_pc = vinsns.len() as u32;

    vinsns.push(VInsn { op: Op::ConstBytes, a: v_then, b: Opnd::Z, c: Opnd::Z, imm: 1 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_then.v)] });

    vinsns.push(VInsn { op: Op::BytesConcat2, a: v_out1, b: Opnd::V(v_init.v), c: Opnd::V(v_then.v), imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_init.v), VReg(v_then.v)], defs: vec![VReg(v_out1.v)] });

    vinsns.push(VInsn { op: Op::Ret, a: v_out1, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_out1.v)], defs: vec![] });

    // Compute actual bytecode jump delta: from next pc after JmpIf (pc=3) to then_pc.
    // JmpIf is at pc=2 in the emitted insn list (0 init, 1 cond, 2 jmp_if, ...).
    let jmp_if_pc: u32 = 2;
    let delta: i32 = (then_pc as i32) - ((jmp_if_pc + 1) as i32);

    // Type-aware LSRA (same as in build_concat_literals_module)
    let num_vregs = alloc.next_v;
    let vtypes = alloc.vtypes;
    let mut type_ids: Vec<u32> = vtypes.iter().copied().collect();
    type_ids.sort();
    type_ids.dedup();

    let mut vreg_to_reg: Vec<u8> = vec![0; num_vregs as usize];
    let mut reg_types: Vec<u32> = Vec::new();
    let mut base: u16 = 0;
    for tid in type_ids {
        let mut globals: Vec<u32> = Vec::new();
        for (gv, &t) in vtypes.iter().enumerate() {
            if t == tid {
                globals.push(gv as u32);
            }
        }
        let mut g2l: Vec<Option<u32>> = vec![None; num_vregs as usize];
        for (li, &gv) in globals.iter().enumerate() {
            g2l[gv as usize] = Some(li as u32);
        }

        let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
        for ins in &infos {
            let mut uses = Vec::new();
            let mut defs = Vec::new();
            for &u in &ins.uses {
                if let Some(li) = g2l[u.0 as usize] {
                    uses.push(VReg(li));
                }
            }
            for &d in &ins.defs {
                if let Some(li) = g2l[d.0 as usize] {
                    defs.push(VReg(li));
                }
            }
            cls_instrs.push(InstrInfo { uses, defs });
        }

        let spillable = vec![false; globals.len()];
        let alloc = regalloc::lsra_allocate(256, globals.len() as u32, &cls_instrs, &spillable, SpillPolicy::Forbid, None)
            .expect("LSRA allocation failed for typed class");

        let used = alloc.used_pregs;
        if base as u32 + used as u32 > 256 {
            panic!("register allocation exceeded 256 regs");
        }
        let need = base as usize + used as usize;
        if reg_types.len() < need {
            reg_types.resize(need, 0);
        }
        for i in 0..used {
            reg_types[base as usize + i as usize] = tid;
        }
        for (li, &gv) in globals.iter().enumerate() {
            let p = alloc.vreg_to_preg[li].expect("unexpected spill");
            vreg_to_reg[gv as usize] = (base + p.0) as u8;
        }
        base += used;
    }

    let mut insns: Vec<Insn> = Vec::with_capacity(vinsns.len());
    for (pc, vi) in vinsns.iter().enumerate() {
        match vi.op {
            Op::ConstBool => {
                // imm is in c for VM format; keep imm field 0.
                insns.push(Insn {
                    op: Op::ConstBool as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: (vi.imm & 1) as u8,
                    imm: 0,
                });
            }
            Op::JmpIf => {
                let imm = if pc as u32 == jmp_if_pc { delta as u32 } else { 0 };
                insns.push(Insn { op: Op::JmpIf as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm });
            }
            Op::Ret => {
                insns.push(Insn { op: Op::Ret as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            _ => {
                let b = match vi.b { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                let c = match vi.c { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                insns.push(Insn { op: vi.op as u8, a: vreg_to_reg[vi.a.v as usize], b, c, imm: vi.imm });
            }
        }
    }

    let f0 = Function { reg_types, insns };
    Module { types, sigs: vec![], atoms: vec![], const_bytes, funcs: vec![f0], entry: 0 }
}

pub fn build_let_y_if_join_concat_module(
    init: Vec<u8>,
    cond: bool,
    then_rhs: Vec<u8>,
    else_rhs: Vec<u8>,
    tail_rhs: Vec<u8>,
) -> Module {
    // Types:
    // 0 bytes, 1 bool
    let types = vec![
        TypeEntry { kind: TypeKind::Bytes, p0: 0 },
        TypeEntry { kind: TypeKind::Bool, p0: 0 },
    ];

    // const_bytes: [init, then_rhs, else_rhs, tail_rhs]
    let const_bytes = vec![init, then_rhs, else_rhs, tail_rhs];

    // Join via phi-elimination: each branch computes its own bytes result, then
    // assigns it into a join variable `y` using MOV, and `y` is used after the
    // if. This intentionally exercises LSRA across the join.
    //
    // Our allocator is SSA-first; to model a join variable with MOVs in both
    // branches we opt-in to allowing *one selected vreg* (`v_y`) to have
    // multiple defs (one per branch). This keeps the register file statically
    // typed and is safe for this straight-line CFG.
    #[derive(Clone, Copy)]
    struct VTmp {
        v: u32,
    }
    #[derive(Clone, Copy)]
    enum Opnd {
        V(u32),
        Z,
    }
    #[derive(Clone, Copy)]
    struct VInsn {
        op: Op,
        a: VTmp,
        b: Opnd,
        c: Opnd,
        imm: u32,
    }

    struct VAlloc {
        next_v: u32,
        vtypes: Vec<u32>,
    }
    impl VAlloc {
        fn new() -> Self {
            Self { next_v: 0, vtypes: Vec::new() }
        }
        fn new_v(&mut self, tid: u32) -> VTmp {
            let v = self.next_v;
            self.next_v += 1;
            if v as usize == self.vtypes.len() {
                self.vtypes.push(tid);
            } else {
                self.vtypes[v as usize] = tid;
            }
            VTmp { v }
        }
    }
    let mut alloc = VAlloc::new();

    // vregs
    let v_init = alloc.new_v(0);
    let v_cond = alloc.new_v(1);
    let v_else_rhs = alloc.new_v(0);
    let v_else_val = alloc.new_v(0);
    let v_y = alloc.new_v(0); // join variable (multi-def)
    let v_then_rhs = alloc.new_v(0);
    let v_then_val = alloc.new_v(0);
    let v_tail = alloc.new_v(0);
    let v_out = alloc.new_v(0);

    let mut vinsns: Vec<VInsn> = Vec::new();
    let mut infos: Vec<InstrInfo> = Vec::new();

    // init, cond
    vinsns.push(VInsn { op: Op::ConstBytes, a: v_init, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_init.v)] });
    vinsns.push(VInsn { op: Op::ConstBool, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: if cond { 1 } else { 0 } });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_cond.v)] });

    // jmp_if to then (patch imm later)
    vinsns.push(VInsn { op: Op::JmpIf, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_cond.v)], defs: vec![] });

    // else block
    vinsns.push(VInsn { op: Op::ConstBytes, a: v_else_rhs, b: Opnd::Z, c: Opnd::Z, imm: 2 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_else_rhs.v)] });
    vinsns.push(VInsn { op: Op::BytesConcat2, a: v_else_val, b: Opnd::V(v_init.v), c: Opnd::V(v_else_rhs.v), imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_init.v), VReg(v_else_rhs.v)], defs: vec![VReg(v_else_val.v)] });
    vinsns.push(VInsn { op: Op::Mov, a: v_y, b: Opnd::V(v_else_val.v), c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_else_val.v)], defs: vec![VReg(v_y.v)] });

    // jmp to join (patch imm later)
    vinsns.push(VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![], defs: vec![] });
    let jmp_pc: u32 = (vinsns.len() as u32) - 1;

    // then label
    let then_pc = vinsns.len() as u32;
    vinsns.push(VInsn { op: Op::ConstBytes, a: v_then_rhs, b: Opnd::Z, c: Opnd::Z, imm: 1 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_then_rhs.v)] });
    vinsns.push(VInsn { op: Op::BytesConcat2, a: v_then_val, b: Opnd::V(v_init.v), c: Opnd::V(v_then_rhs.v), imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_init.v), VReg(v_then_rhs.v)], defs: vec![VReg(v_then_val.v)] });
    vinsns.push(VInsn { op: Op::Mov, a: v_y, b: Opnd::V(v_then_val.v), c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_then_val.v)], defs: vec![VReg(v_y.v)] });

    // join label (falls through from then)
    let join_pc = vinsns.len() as u32;
    vinsns.push(VInsn { op: Op::ConstBytes, a: v_tail, b: Opnd::Z, c: Opnd::Z, imm: 3 });
    infos.push(InstrInfo { uses: vec![], defs: vec![VReg(v_tail.v)] });
    vinsns.push(VInsn { op: Op::BytesConcat2, a: v_out, b: Opnd::V(v_y.v), c: Opnd::V(v_tail.v), imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_y.v), VReg(v_tail.v)], defs: vec![VReg(v_out.v)] });
    vinsns.push(VInsn { op: Op::Ret, a: v_out, b: Opnd::Z, c: Opnd::Z, imm: 0 });
    infos.push(InstrInfo { uses: vec![VReg(v_out.v)], defs: vec![] });

    // Patch jump deltas.
    let jmp_if_pc: u32 = 2;
    let delta_to_then: i32 = (then_pc as i32) - ((jmp_if_pc + 1) as i32);
    let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);

    // Type-aware LSRA (same packing strategy as other builders).
    let num_vregs = alloc.next_v;
    let vtypes = alloc.vtypes;
    let mut allow_multi_def_global: Vec<bool> = vec![false; num_vregs as usize];
    allow_multi_def_global[v_y.v as usize] = true;
    let mut type_ids: Vec<u32> = vtypes.iter().copied().collect();
    type_ids.sort();
    type_ids.dedup();

    let mut vreg_to_reg: Vec<u8> = vec![0; num_vregs as usize];
    let mut reg_types: Vec<u32> = Vec::new();
    let mut base: u16 = 0;
    for tid in type_ids {
        let mut globals: Vec<u32> = Vec::new();
        for (gv, &t) in vtypes.iter().enumerate() {
            if t == tid {
                globals.push(gv as u32);
            }
        }
        let mut g2l: Vec<Option<u32>> = vec![None; num_vregs as usize];
        for (li, &gv) in globals.iter().enumerate() {
            g2l[gv as usize] = Some(li as u32);
        }

        let mut allow_multi_def_local: Vec<bool> = vec![false; globals.len()];
        for (li, &gv) in globals.iter().enumerate() {
            allow_multi_def_local[li] = allow_multi_def_global[gv as usize];
        }

        let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
        for ins in &infos {
            let mut uses = Vec::new();
            let mut defs = Vec::new();
            for &u in &ins.uses {
                if let Some(li) = g2l[u.0 as usize] {
                    uses.push(VReg(li));
                }
            }
            for &d in &ins.defs {
                if let Some(li) = g2l[d.0 as usize] {
                    defs.push(VReg(li));
                }
            }
            cls_instrs.push(InstrInfo { uses, defs });
        }

        let spillable = vec![false; globals.len()];
        let alloc = regalloc::lsra_allocate(
            256,
            globals.len() as u32,
            &cls_instrs,
            &spillable,
            SpillPolicy::Forbid,
            Some(&allow_multi_def_local),
        )
        .expect("LSRA allocation failed for typed class");

        let used = alloc.used_pregs;
        if base as u32 + used as u32 > 256 {
            panic!("register allocation exceeded 256 regs");
        }

        let need = base as usize + used as usize;
        if reg_types.len() < need {
            reg_types.resize(need, 0);
        }
        for i in 0..used {
            reg_types[base as usize + i as usize] = tid;
        }
        for (li, &gv) in globals.iter().enumerate() {
            let p = alloc.vreg_to_preg[li].expect("unexpected spill");
            vreg_to_reg[gv as usize] = (base + p.0) as u8;
        }
        base += used;
    }

    // Emit final bytecode.
    let mut insns: Vec<Insn> = Vec::with_capacity(vinsns.len());
    for (pc, vi) in vinsns.iter().enumerate() {
        match vi.op {
            Op::ConstBool => {
                insns.push(Insn {
                    op: Op::ConstBool as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: (vi.imm & 1) as u8,
                    imm: 0,
                });
            }
            Op::JmpIf => {
                let imm = if pc as u32 == jmp_if_pc { delta_to_then as u32 } else { 0 };
                insns.push(Insn { op: Op::JmpIf as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm });
            }
            Op::Jmp => {
                let imm = if pc as u32 == jmp_pc { delta_to_join as u32 } else { 0 };
                insns.push(Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm });
            }
            Op::Ret => {
                insns.push(Insn { op: Op::Ret as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            _ => {
                let b = match vi.b { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                let c = match vi.c { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                insns.push(Insn { op: vi.op as u8, a: vreg_to_reg[vi.a.v as usize], b, c, imm: vi.imm });
            }
        }
    }

    let f0 = Function { reg_types, insns };
    Module { types, sigs: vec![], atoms: vec![], const_bytes, funcs: vec![f0], entry: 0 }
}

pub fn build_program_module(p: &Program) -> Result<Module, CompileError> {
    let mut type_ctx = TypeCtx::new_program_base();
    let mut nested_funcs: Vec<Function> = Vec::new();

    #[derive(Clone, Copy)]
    struct VTmp {
        v: u32,
    }
    #[derive(Clone, Copy)]
    enum Opnd {
        V(u32),
        Z,
    }
    #[derive(Clone, Copy)]
    struct VInsn {
        op: Op,
        a: VTmp,
        b: Opnd,
        c: Opnd,
        imm: u32,
    }

    struct CallSite {
        pc: u32,       // VInsn index
        sig_id: u32,   // function signature ID
        args: Vec<u32> // vregs
    }

    let mut env: HashMap<String, VTmp> = HashMap::new();

    struct CompileCtx<'a> {
        // module tables
        type_ctx: &'a mut TypeCtx,
        nested_funcs: &'a mut Vec<Function>,

        // codegen state
        const_bytes: &'a mut Vec<Vec<u8>>,
        vtypes: &'a mut Vec<u32>,
        allow_multi_def: &'a mut Vec<bool>,
        next_v: &'a mut u32,
        vinsns: &'a mut Vec<VInsn>,
        infos: &'a mut Vec<InstrInfo>,
        call_sites: &'a mut Vec<CallSite>,

        // aux state
        loop_stack: &'a mut Vec<LoopCtx>,
        atoms: &'a mut Vec<Vec<u8>>,
        atom_ids: &'a mut HashMap<String, u32>,
    }

    impl<'a> CompileCtx<'a> {
        fn emit(&mut self, vi: VInsn, uses: Vec<u32>, defs: Vec<u32>) {
            self.vinsns.push(vi);
            self.infos.push(InstrInfo {
                uses: uses.into_iter().map(VReg).collect(),
                defs: defs.into_iter().map(VReg).collect(),
            });
        }

        fn new_v(&mut self, tid: u32) -> VTmp {
            let v = *self.next_v;
            *self.next_v += 1;
            if v as usize == self.vtypes.len() {
                self.vtypes.push(tid);
                self.allow_multi_def.push(false);
            } else {
                self.vtypes[v as usize] = tid;
            }
            VTmp { v }
        }
    }

    fn compile_expr(
        e: &Expr,
        env: &HashMap<String, VTmp>,
        ctx: &mut CompileCtx,
    ) -> Result<VTmp, CompileError> {
        compile_expr_expect(e, None, env, ctx)
    }

    fn compile_expr_expect(
        e: &Expr,
        expect: Option<u32>,
        env: &HashMap<String, VTmp>,
        ctx: &mut CompileCtx,
    ) -> Result<VTmp, CompileError> {
        fn compile_member_get(
            span: Span,
            base: &Expr,
            name: &str,
            expect_tid: u32,
            env: &HashMap<String, VTmp>,
            ctx: &mut CompileCtx,
        ) -> Result<VTmp, CompileError> {
            // Namespaces must be called (handled via ExprKind::Call builtin logic).
            if let ExprKind::Var(ns) = &base.node {
                if matches!(
                    ns.as_str(),
                    "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
                ) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        span,
                        "namespace members must be called (e.g. Bytes.len(x))",
                    ));
                }
            }

            let v_obj = compile_expr(base, env, ctx)?;
            let base_tid = ctx.vtypes[v_obj.v as usize];
            let te = ctx
                .type_ctx
                .types
                .get(base_tid as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad member base type id"))?;
            if te.kind != TypeKind::Object {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "member access currently only supported for Object (obj.field)",
                ));
            }
            let atom_id = if let Some(&id) = ctx.atom_ids.get(name) {
                id
            } else {
                let id = ctx.atoms.len() as u32;
                ctx.atoms.push(name.as_bytes().to_vec());
                ctx.atom_ids.insert(name.to_string(), id);
                id
            };

            if ctx.type_ctx.is_tuple_type(base_tid) {
                // Tuple element access: `t.0`, `t.1`, ...
                let idx: usize = name
                    .parse()
                    .map_err(|_| CompileError::new(ErrorKind::Type, span, "tuple element access must be .<index>"))?;
                let elems = ctx
                    .type_ctx
                    .tuple_elems(base_tid)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad tuple type"))?;
                if idx >= elems.len() {
                    return Err(CompileError::new(ErrorKind::Type, span, "tuple index out of range"));
                }
                let elem_tid = elems[idx];
                let v_elem = ctx.new_v(elem_tid);
                ctx.emit(
                    VInsn { op: Op::ObjGetAtom, a: v_elem, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                    vec![v_obj.v],
                    vec![v_elem.v],
                );
                if expect_tid == elem_tid {
                    return Ok(v_elem);
                }
                if expect_tid == 3 {
                    let vd = ctx.new_v(3);
                    ctx.emit(
                        VInsn { op: Op::ToDyn, a: vd, b: Opnd::V(v_elem.v), c: Opnd::Z, imm: 0 },
                        vec![v_elem.v],
                        vec![vd.v],
                    );
                    return Ok(vd);
                }
                return Err(CompileError::new(ErrorKind::Type, span, "tuple element type mismatch"));
            }

            let out = ctx.new_v(expect_tid);
            ctx.emit(
                VInsn { op: Op::ObjGetAtom, a: out, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                vec![v_obj.v],
                vec![out.v],
            );
            Ok(out)
        }

        fn compile_expr_expect(
            e: &Expr,
            expect_tid: u32,
            env: &HashMap<String, VTmp>,
            ctx: &mut CompileCtx,
        ) -> Result<VTmp, CompileError> {
            match &e.node {
                ExprKind::Member { base, name } => compile_member_get(e.span, base, name.as_str(), expect_tid, env, ctx),
                _ => {
                    let v = compile_expr(e, env, ctx)?;
                    if ctx.vtypes[v.v as usize] != expect_tid {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "type mismatch",
                        ));
                    }
                    Ok(v)
                }
            }
        }

        match &e.node {
            ExprKind::BytesLit(b) => {
                let idx = ctx.const_bytes.len() as u32;
                ctx.const_bytes.push(b.clone());
                let v = ctx.new_v(0);
                ctx.emit(VInsn { op: Op::ConstBytes, a: v, b: Opnd::Z, c: Opnd::Z, imm: idx }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::BoolLit(b) => {
                let v = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::ConstBool, a: v, b: Opnd::Z, c: Opnd::Z, imm: if *b { 1 } else { 0 } },
                    vec![],
                    vec![v.v],
                );
                Ok(v)
            }
            ExprKind::I32Lit(x) => {
                let v = ctx.new_v(2);
                ctx.emit(VInsn { op: Op::ConstI32, a: v, b: Opnd::Z, c: Opnd::Z, imm: *x as u32 }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::Var(name) => env
                .get(name)
                .copied()
                .ok_or_else(|| CompileError::new(ErrorKind::Name, e.span, format!("unknown variable '{}'", name))),
            ExprKind::Member { base, name } => {
                // Default to dynamic unless an operator/builtin provides an expected type.
                compile_member_get(e.span, base, name.as_str(), 3, env, ctx)
            }
            ExprKind::Call { callee, type_args, args } => {
                // Builtins are namespaced (e.g. `Bytes.get_u8`, `Array.new<I32>`).
                let builtin = match &callee.node {
                    ExprKind::Member { base, name } => match &base.node {
                        ExprKind::Var(ns) => Some((ns.clone(), name.clone())),
                        _ => None,
                    },
                    _ => None,
                };

                if let Some((ns, name)) = builtin {
                    let ns = ns.as_str();
                    let name = name.as_str();
                    // Bytes builtins
                    if ns == "Bytes" && name == "new" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.new expects 1 arg"));
                        }
                        let vlen = compile_expr_expect(&args[0], 2, env, ctx)?;
                        let out = ctx.new_v(0);
                        ctx.emit(VInsn { op: Op::BytesNew, a: out, b: Opnd::V(vlen.v), c: Opnd::Z, imm: 0 }, vec![vlen.v], vec![out.v]);
                        return Ok(out);
                    } else if ns == "Bytes" && name == "get_u8" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.get_u8 expects 2 args"));
                        }
                        let vb = compile_expr_expect(&args[0], 0, env, ctx)?;
                        let vi = compile_expr_expect(&args[1], 2, env, ctx)?;
                        let out = ctx.new_v(2);
                        ctx.emit(
                            VInsn { op: Op::BytesGetU8, a: out, b: Opnd::V(vb.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![vb.v, vi.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Bytes" && name == "set_u8" {
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.set_u8 expects 3 args"));
                        }
                        let vb = compile_expr_expect(&args[0], 0, env, ctx)?;
                        let vi = compile_expr_expect(&args[1], 2, env, ctx)?;
                        let vv = compile_expr_expect(&args[2], 2, env, ctx)?;
                        ctx.emit(
                            VInsn { op: Op::BytesSetU8, a: vv, b: Opnd::V(vb.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![vv.v, vb.v, vi.v],
                            vec![],
                        );
                        // Treat as expression returning the mutated bytes object.
                        return Ok(vb);
                    } else if ns == "Bytes" && name == "len" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.len expects 1 arg"));
                        }
                        let vb = compile_expr_expect(&args[0], 0, env, ctx)?;
                        let out = ctx.new_v(2);
                        ctx.emit(
                            VInsn { op: Op::BytesLen, a: out, b: Opnd::V(vb.v), c: Opnd::Z, imm: 0 },
                            vec![vb.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Bytes" && name == "slice" {
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.slice expects 3 args"));
                        }
                        let vb = compile_expr_expect(&args[0], 0, env, ctx)?;
                        let v_start = compile_expr_expect(&args[1], 2, env, ctx)?;
                        let v_len = compile_expr_expect(&args[2], 2, env, ctx)?;

                        let sig_args = vec![0u32, 2u32, 2u32]; // (bytes, i32, i32)
                        let fun_tid = ctx.type_ctx.intern_fun_type(0, &sig_args);
                        let sig_id = ctx.type_ctx.intern_sig(0, &sig_args);

                        let vcallee = ctx.new_v(fun_tid);
                        ctx.emit(
                            VInsn { op: Op::ConstFun, a: vcallee, b: Opnd::Z, c: Opnd::Z, imm: PRELUDE_BYTES_SLICE },
                            vec![],
                            vec![vcallee.v],
                        );

                        let out = ctx.new_v(0);
                        let pc = ctx.vinsns.len() as u32;
                        ctx.call_sites.push(CallSite { pc, sig_id, args: vec![vb.v, v_start.v, v_len.v] });

                        ctx.emit(
                            VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: 3 },
                            vec![vcallee.v, vb.v, v_start.v, v_len.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Bytes" && name == "eq" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.eq expects 2 args"));
                        }
                        let va = compile_expr_expect(&args[0], 0, env, ctx)?;
                        let vb = compile_expr_expect(&args[1], 0, env, ctx)?;

                        let sig_args = vec![0u32, 0u32]; // (bytes, bytes)
                        let fun_tid = ctx.type_ctx.intern_fun_type(1, &sig_args); // -> bool
                        let sig_id = ctx.type_ctx.intern_sig(1, &sig_args);

                        let vcallee = ctx.new_v(fun_tid);
                        ctx.emit(
                            VInsn { op: Op::ConstFun, a: vcallee, b: Opnd::Z, c: Opnd::Z, imm: PRELUDE_BYTES_EQ },
                            vec![],
                            vec![vcallee.v],
                        );

                        let out = ctx.new_v(1);
                        let pc = ctx.vinsns.len() as u32;
                        ctx.call_sites.push(CallSite { pc, sig_id, args: vec![va.v, vb.v] });

                        ctx.emit(
                            VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: 2 },
                            vec![vcallee.v, va.v, vb.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Atom" && name == "intern" {
                        if !type_args.is_empty() {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Atom.intern does not take type args"));
                        }
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Atom.intern expects 1 arg"));
                        }
                        let b = match &args[0].node {
                            ExprKind::BytesLit(bb) => bb,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    args[0].span,
                                    "Atom.intern expects a bytes literal",
                                ))
                            }
                        };
                        let name = std::str::from_utf8(b).map_err(|_| {
                            CompileError::new(ErrorKind::Type, args[0].span, "Atom.intern expects UTF-8 bytes")
                        })?;
                        let atom_id = if let Some(&id) = ctx.atom_ids.get(name) {
                            id
                        } else {
                            let id = ctx.atoms.len() as u32;
                            ctx.atoms.push(name.as_bytes().to_vec());
                            ctx.atom_ids.insert(name.to_string(), id);
                            id
                        };
                        let out = ctx.new_v(9);
                        ctx.emit(
                            VInsn { op: Op::ConstAtom, a: out, b: Opnd::Z, c: Opnd::Z, imm: atom_id },
                            vec![],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Object" && name == "get" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.get expects 2 args"));
                        }
                        let out_tid = if type_args.is_empty() {
                            3
                        } else {
                            if type_args.len() != 1 {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "Object.get<T>(obj, key): expects 1 type arg",
                                ));
                            }
                            ctx.type_ctx.resolve_ty(&type_args[0])?
                        };
                        let v_obj = compile_expr_expect(&args[0], 6, env, ctx)?;
                        let v_key = compile_expr_expect(&args[1], 9, env, ctx)?;
                        let out = ctx.new_v(out_tid);
                        ctx.emit(
                            VInsn { op: Op::ObjGet, a: out, b: Opnd::V(v_obj.v), c: Opnd::V(v_key.v), imm: 0 },
                            vec![v_obj.v, v_key.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Object" && name == "set" {
                        if !type_args.is_empty() {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.set does not take type args"));
                        }
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.set expects 3 args"));
                        }
                        let v_obj = compile_expr_expect(&args[0], 6, env, ctx)?;
                        let v_key = compile_expr_expect(&args[1], 9, env, ctx)?;
                        let v_val = compile_expr(&args[2], env, ctx)?;
                        ctx.emit(
                            VInsn { op: Op::ObjSet, a: v_val, b: Opnd::V(v_obj.v), c: Opnd::V(v_key.v), imm: 0 },
                            vec![v_val.v, v_obj.v, v_key.v],
                            vec![],
                        );
                        return Ok(v_obj);
                    } else if ns == "Array" && name == "new" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.new expects 1 arg"));
                        }
                        // Determine element type.
                        if type_args.len() != 1 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "Array.new<T>(len): missing type argument",
                            ));
                        }
                        let elem_tid = ctx.type_ctx.resolve_ty(&type_args[0])?;
                        let arr_tid = match elem_tid {
                            2 => 4,
                            0 => 5,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "only Array<I32> and Array<Bytes> supported for now",
                                ))
                            }
                        };
                        let vlen = compile_expr_expect(&args[0], 2, env, ctx)?;
                        let out = ctx.new_v(arr_tid);
                        ctx.emit(
                            VInsn { op: Op::ArrayNew, a: out, b: Opnd::V(vlen.v), c: Opnd::Z, imm: 0 },
                            vec![vlen.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Array" && name == "len" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.len expects 1 arg"));
                        }
                        let va = compile_expr(&args[0], env, ctx)?;
                        let at = ctx.vtypes[va.v as usize];
                        if at != 4 && at != 5 {
                            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Array.len(Array<T>)"));
                        }
                        let out = ctx.new_v(2);
                        ctx.emit(
                            VInsn { op: Op::ArrayLen, a: out, b: Opnd::V(va.v), c: Opnd::Z, imm: 0 },
                            vec![va.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Array" && name == "get" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get expects 2 args"));
                        }
                        let va = compile_expr(&args[0], env, ctx)?;
                        let vi = compile_expr_expect(&args[1], 2, env, ctx)?;
                        let at = ctx.vtypes[va.v as usize];
                        if (at != 4 && at != 5) || ctx.vtypes[vi.v as usize] != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get(Array<T>, i32)"));
                        }
                        let out_tid = if at == 4 { 2 } else { 0 };
                        let out = ctx.new_v(out_tid);
                        ctx.emit(
                            VInsn { op: Op::ArrayGet, a: out, b: Opnd::V(va.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![va.v, vi.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Array" && name == "set" {
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set expects 3 args"));
                        }
                        let va = compile_expr(&args[0], env, ctx)?;
                        let vi = compile_expr_expect(&args[1], 2, env, ctx)?;
                        let at = ctx.vtypes[va.v as usize];
                        let want_vt = if at == 4 { 2 } else if at == 5 { 0 } else { u32::MAX };
                        if want_vt == u32::MAX || ctx.vtypes[vi.v as usize] != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set(Array<T>, i32, T)"));
                        }
                        let vv = compile_expr_expect(&args[2], want_vt, env, ctx)?;
                        // JOP_ARRAY_SET: rB[rC] = rA
                        ctx.emit(
                            VInsn { op: Op::ArraySet, a: vv, b: Opnd::V(va.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![vv.v, va.v, vi.v],
                            vec![],
                        );
                        return Ok(va);
                    }
                }

                if !type_args.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "generic type arguments are only supported for builtins (for now)",
                    ));
                }

                let vcallee = compile_expr(callee, env, ctx)?;
                let callee_tid = ctx.vtypes[vcallee.v as usize];
                let te = ctx
                    .type_ctx
                    .types
                    .get(callee_tid as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad callee type id"))?;
                if te.kind != TypeKind::Function {
                    return Err(CompileError::new(ErrorKind::Type, callee.span, "call target is not a function"));
                }
                let sig_id = te.p0;
                let (sig_args, sig_ret) = {
                    let sig = ctx
                        .type_ctx
                        .sigs
                        .get(sig_id as usize)
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?;
                    (sig.args.clone(), sig.ret_type)
                };
                if args.len() != sig_args.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "call arity does not match function type"));
                }
                if sig_args.len() > 255 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "too many call arguments"));
                }
                let mut arg_vs: Vec<u32> = Vec::with_capacity(args.len());
                for (i, a) in args.iter().enumerate() {
                    let va = compile_expr(a, env, ctx)?;
                    if ctx.vtypes[va.v as usize] != sig_args[i] {
                        return Err(CompileError::new(ErrorKind::Type, a.span, "call argument type mismatch"));
                    }
                    arg_vs.push(va.v);
                }

                let out = ctx.new_v(sig_ret);
                let pc = ctx.vinsns.len() as u32;
                ctx.call_sites.push(CallSite { pc, sig_id, args: arg_vs.clone() });

                let mut uses: Vec<u32> = Vec::with_capacity(1 + arg_vs.len());
                uses.push(vcallee.v);
                uses.extend(arg_vs.iter().copied());

                ctx.emit(
                    VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: arg_vs.len() as u32 },
                    uses,
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Not(inner) => {
                let v = compile_expr(inner, env, ctx)?;
                if ctx.vtypes[v.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'!' expects bool"));
                }
                let out = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                    vec![v.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Add(_a, _b) => {
                // Special-case bytes concatenation: flatten long chains into concat_many(Array<bytes>).
                fn collect_add_terms<'e>(e: &'e Expr, out: &mut Vec<&'e Expr>) {
                    match &e.node {
                        ExprKind::Add(x, y) => {
                            collect_add_terms(x, out);
                            collect_add_terms(y, out);
                        }
                        _ => out.push(e),
                    }
                }

                let mut terms: Vec<&Expr> = Vec::new();
                collect_add_terms(e, &mut terms);

                // If it's a simple binary add, keep existing path.
                if terms.len() == 2 {
                    let va = compile_expr(terms[0], env, ctx)?;
                    let vb = compile_expr(terms[1], env, ctx)?;
                    let mut ta = ctx.vtypes[va.v as usize];
                    let mut tb = ctx.vtypes[vb.v as usize];
                    let (va, vb) = if ta != tb {
                        match (&terms[0].node, &terms[1].node) {
                            (ExprKind::Member { .. }, _) => {
                                let va = compile_expr_expect(terms[0], tb, env, ctx)?;
                                ta = tb;
                                (va, vb)
                            }
                            (_, ExprKind::Member { .. }) => {
                                let vb = compile_expr_expect(terms[1], ta, env, ctx)?;
                                tb = ta;
                                (va, vb)
                            }
                            _ => (va, vb),
                        }
                    } else {
                        (va, vb)
                    };
                    if ta != tb {
                        return Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects operands of same type"));
                    }
                    match ta {
                        0 => {
                            let out = ctx.new_v(0);
                            ctx.emit(
                                VInsn { op: Op::BytesConcat2, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                                vec![va.v, vb.v],
                                vec![out.v],
                            );
                            return Ok(out);
                        }
                        2 => {
                            let out = ctx.new_v(2);
                            ctx.emit(
                                VInsn { op: Op::AddI32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                                vec![va.v, vb.v],
                                vec![out.v],
                            );
                            return Ok(out);
                        }
                        _ => return Err(CompileError::new(ErrorKind::Type, e.span, "'+' not supported for this type yet")),
                    }
                }

                // Otherwise compile left-to-right and decide based on term types.
                let mut compiled: Vec<Option<VTmp>> = vec![None; terms.len()];
                let mut deferred_members: Vec<usize> = Vec::new();
                let mut t0: Option<u32> = None;
                for (i, t) in terms.iter().enumerate() {
                    if matches!(t.node, ExprKind::Member { .. }) {
                        deferred_members.push(i);
                        continue;
                    }
                    let v = compile_expr(t, env, ctx)?;
                    let tt = ctx.vtypes[v.v as usize];
                    if let Some(t0v) = t0 {
                        if tt != t0v {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects operands of same type"));
                        }
                    } else {
                        t0 = Some(tt);
                    }
                    compiled[i] = Some(v);
                }
                let t0 = t0.ok_or_else(|| CompileError::new(ErrorKind::Type, e.span, "cannot infer '+' operand type"))?;
                for i in deferred_members {
                    let v = compile_expr_expect(terms[i], t0, env, ctx)?;
                    compiled[i] = Some(v);
                }
                let compiled: Vec<VTmp> = compiled.into_iter().map(|x| x.expect("compiled term")).collect();

                match t0 {
                    0 => {
                        // Build Array<bytes> and concat_many.
                        let n = compiled.len();
                        let v_n = ctx.new_v(2);
                        ctx.emit(
                            VInsn { op: Op::ConstI32, a: v_n, b: Opnd::Z, c: Opnd::Z, imm: n as u32 },
                            vec![],
                            vec![v_n.v],
                        );
                        let v_arr = ctx.new_v(5); // Array<bytes>
                        ctx.emit(
                            VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_n.v), c: Opnd::Z, imm: 0 },
                            vec![v_n.v],
                            vec![v_arr.v],
                        );
                        for (i, part) in compiled.iter().enumerate() {
                            let v_i = ctx.new_v(2);
                            ctx.emit(
                                VInsn { op: Op::ConstI32, a: v_i, b: Opnd::Z, c: Opnd::Z, imm: i as u32 },
                                vec![],
                                vec![v_i.v],
                            );
                            ctx.emit(
                                VInsn { op: Op::ArraySet, a: *part, b: Opnd::V(v_arr.v), c: Opnd::V(v_i.v), imm: 0 },
                                vec![part.v, v_arr.v, v_i.v],
                                vec![],
                            );
                        }
                        let out = ctx.new_v(0);
                        ctx.emit(
                            VInsn { op: Op::BytesConcatMany, a: out, b: Opnd::V(v_arr.v), c: Opnd::Z, imm: 0 },
                            vec![v_arr.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    2 => {
                        // Fold i32 addition left-to-right (keeps semantics identical).
                        let mut acc = compiled[0];
                        for rhs in &compiled[1..] {
                            let out = ctx.new_v(2);
                            ctx.emit(
                                VInsn { op: Op::AddI32, a: out, b: Opnd::V(acc.v), c: Opnd::V(rhs.v), imm: 0 },
                                vec![acc.v, rhs.v],
                                vec![out.v],
                            );
                            acc = out;
                        }
                        Ok(acc)
                    }
                    _ => Err(CompileError::new(ErrorKind::Type, e.span, "'+' not supported for this type yet")),
                }
            }
            ExprKind::Sub(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                let ta = ctx.vtypes[va.v as usize];
                let tb = ctx.vtypes[vb.v as usize];
                if ta != tb {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'-' expects operands of same type"));
                }
                if ta != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'-' currently only supported for i32"));
                }
                let out = ctx.new_v(2);
                ctx.emit(
                    VInsn { op: Op::SubI32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                    vec![va.v, vb.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Neg(inner) => {
                let v = compile_expr(inner, env, ctx)?;
                if ctx.vtypes[v.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "unary '-' currently only supported for i32"));
                }
                let out = ctx.new_v(2);
                ctx.emit(
                    VInsn { op: Op::NegI32, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                    vec![v.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Eq(a, b) => {
                // If one side is a member access, allow it to take its type from the other side.
                let (va, vb) = match (&a.node, &b.node) {
                    (ExprKind::Member { .. }, _) => {
                        let vb = compile_expr(b, env, ctx)?;
                        let tb = ctx.vtypes[vb.v as usize];
                        let va = compile_expr_expect(a, tb, env, ctx)?;
                        (va, vb)
                    }
                    (_, ExprKind::Member { .. }) => {
                        let va = compile_expr(a, env, ctx)?;
                        let ta = ctx.vtypes[va.v as usize];
                        let vb = compile_expr_expect(b, ta, env, ctx)?;
                        (va, vb)
                    }
                    _ => (compile_expr(a, env, ctx)?, compile_expr(b, env, ctx)?),
                };
                let ta = ctx.vtypes[va.v as usize];
                let tb = ctx.vtypes[vb.v as usize];
                if ta != tb {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'==' expects operands of same type"));
                }
                let out = ctx.new_v(1);
                match ta {
                    1 => {
                        ctx.emit(
                            VInsn { op: Op::Physeq, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    2 => {
                        ctx.emit(
                            VInsn { op: Op::EqI32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    _ => Err(CompileError::new(ErrorKind::Type, e.span, "'==' not supported for this type yet")),
                }
            }
            ExprKind::Ne(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                let ta = ctx.vtypes[va.v as usize];
                let tb = ctx.vtypes[vb.v as usize];
                if ta != tb {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'!=' expects operands of same type"));
                }
                let t = ctx.new_v(1);
                match ta {
                    1 => {
                        ctx.emit(
                            VInsn { op: Op::Physeq, a: t, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![t.v],
                        );
                    }
                    2 => {
                        ctx.emit(
                            VInsn { op: Op::EqI32, a: t, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![t.v],
                        );
                    }
                    _ => return Err(CompileError::new(ErrorKind::Type, e.span, "'!=' not supported for this type yet")),
                }
                let out = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(t.v), c: Opnd::Z, imm: 0 },
                    vec![t.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Lt(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[va.v as usize] != 2 || ctx.vtypes[vb.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'<' currently only supported for i32"));
                }
                let out = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                    vec![va.v, vb.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Gt(a, b) => {
                // a > b  ==  b < a
                let vb = compile_expr(b, env, ctx)?;
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != 2 || ctx.vtypes[vb.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'>' currently only supported for i32"));
                }
                let out = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: out, b: Opnd::V(vb.v), c: Opnd::V(va.v), imm: 0 },
                    vec![vb.v, va.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Le(a, b) => {
                // a <= b  ==  !(b < a)
                let vb = compile_expr(b, env, ctx)?;
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != 2 || ctx.vtypes[vb.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'<=' currently only supported for i32"));
                }
                let t = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: t, b: Opnd::V(vb.v), c: Opnd::V(va.v), imm: 0 },
                    vec![vb.v, va.v],
                    vec![t.v],
                );
                let out = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(t.v), c: Opnd::Z, imm: 0 },
                    vec![t.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Ge(a, b) => {
                // a >= b  ==  !(a < b)
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[va.v as usize] != 2 || ctx.vtypes[vb.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'>=' currently only supported for i32"));
                }
                let t = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: t, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                    vec![va.v, vb.v],
                    vec![t.v],
                );
                let out = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(t.v), c: Opnd::Z, imm: 0 },
                    vec![t.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::And(a, b) => {
                // Short-circuit:
                //   if(!a) res=false else res=b
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
                }

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![va.v],
                    vec![],
                );

                // false path
                let v_false = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::ConstBool, a: v_false, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_false.v],
                );
                let v_res = ctx.new_v(1);
                ctx.allow_multi_def[v_res.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_false.v), c: Opnd::Z, imm: 0 },
                    vec![v_false.v],
                    vec![v_res.v],
                );
                let jmp_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                // rhs path
                let rhs_pc = ctx.vinsns.len() as u32;
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[vb.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
                }
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(vb.v), c: Opnd::Z, imm: 0 },
                    vec![vb.v],
                    vec![v_res.v],
                );
                let join_pc = ctx.vinsns.len() as u32;

                // Patch jumps.
                let delta_to_rhs: i32 = (rhs_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_rhs as u32;
                ctx.vinsns[jmp_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::Or(a, b) => {
                // Short-circuit:
                //   if(a) res=true else res=b
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
                }

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![va.v],
                    vec![],
                );

                // false path: compute b
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[vb.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
                }
                let v_res = ctx.new_v(1);
                ctx.allow_multi_def[v_res.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(vb.v), c: Opnd::Z, imm: 0 },
                    vec![vb.v],
                    vec![v_res.v],
                );
                let jmp_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                // true path
                let true_pc = ctx.vinsns.len() as u32;
                let v_true = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::ConstBool, a: v_true, b: Opnd::Z, c: Opnd::Z, imm: 1 },
                    vec![],
                    vec![v_true.v],
                );
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_true.v), c: Opnd::Z, imm: 0 },
                    vec![v_true.v],
                    vec![v_res.v],
                );
                let join_pc = ctx.vinsns.len() as u32;

                // Patch jumps.
                let delta_to_true: i32 = (true_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_true as u32;
                ctx.vinsns[jmp_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::If { cond, then_br, else_br } => {
                let v_cond = compile_expr(cond, env, ctx)?;
                if ctx.vtypes[v_cond.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, cond.span, "if condition must be bool"));
                }

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![v_cond.v],
                    vec![],
                );

                // else block
                let v_else = compile_expr(else_br, env, ctx)?;

                // then block (type must match else)
                let v_then_start_pc = ctx.vinsns.len() as u32; // after else code + mov + jmp we compute; placeholder for type checking
                let _ = v_then_start_pc;

                let v_res_tid = ctx.vtypes[v_else.v as usize];
                let v_res = ctx.new_v(v_res_tid);
                ctx.allow_multi_def[v_res.v as usize] = true; // join var multi-def
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_else.v), c: Opnd::Z, imm: 0 },
                    vec![v_else.v],
                    vec![v_res.v],
                );

                let jmp_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                // then label
                let then_pc = ctx.vinsns.len() as u32;
                let v_then = compile_expr(then_br, env, ctx)?;
                if ctx.vtypes[v_then.v as usize] != v_res_tid {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "if branches must return same type"));
                }
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_then.v), c: Opnd::Z, imm: 0 },
                    vec![v_then.v],
                    vec![v_res.v],
                );

                let join_pc = ctx.vinsns.len() as u32;

                // Patch jumps.
                let delta_to_then: i32 = (then_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_then as u32;
                ctx.vinsns[jmp_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::Block { stmts, expr } => {
                let mut scoped = env.clone();
                for s in stmts {
                    compile_stmt(s, &mut scoped, ctx)?;
                }
                compile_expr(expr, &scoped, ctx)
            }
            ExprKind::Try { body, catch_name, catch_body } => {
                let v_exc = ctx.new_v(3);
                let try_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Try, a: v_exc, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_exc.v],
                );

                let v_body = compile_expr(body, env, ctx)?;
                let tid = ctx.vtypes[v_body.v as usize];
                let v_res = ctx.new_v(tid);
                ctx.allow_multi_def[v_res.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_body.v), c: Opnd::Z, imm: 0 },
                    vec![v_body.v],
                    vec![v_res.v],
                );

                ctx.emit(
                    VInsn { op: Op::EndTry, a: v_exc, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                let jmp_over_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_exc, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                let catch_pc = ctx.vinsns.len() as u32;
                let mut catch_env = env.clone();
                if let Some(n) = catch_name {
                    catch_env.insert(n.clone(), v_exc);
                }
                let v_catch = compile_expr(catch_body, &catch_env, ctx)?;
                if ctx.vtypes[v_catch.v as usize] != tid {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "try and catch must return same type"));
                }
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_catch.v), c: Opnd::Z, imm: 0 },
                    vec![v_catch.v],
                    vec![v_res.v],
                );

                let join_pc = ctx.vinsns.len() as u32;
                let delta_to_catch: i32 = (catch_pc as i32) - ((try_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_over_pc + 1) as i32);
                ctx.vinsns[try_pc as usize].imm = delta_to_catch as u32;
                ctx.vinsns[jmp_over_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::Null => {
                let v = ctx.new_v(3);
                ctx.emit(VInsn { op: Op::ConstNull, a: v, b: Opnd::Z, c: Opnd::Z, imm: 0 }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::ArrayLit(elems) => {
                if elems.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "empty array literal requires a type annotation (not implemented yet)",
                    ));
                }
                let mut vs: Vec<VTmp> = Vec::with_capacity(elems.len());
                for e in elems {
                    vs.push(compile_expr(e, env, ctx)?);
                }
                let t0 = ctx.vtypes[vs[0].v as usize];
                for v in &vs[1..] {
                    if ctx.vtypes[v.v as usize] != t0 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "array literal elements must have same type",
                        ));
                    }
                }
                let arr_tid = match t0 {
                    2 => 4, // Array<i32>
                    0 => 5, // Array<bytes>
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "array literal only supports i32/bytes elements for now",
                        ))
                    }
                };

                let v_len = ctx.new_v(2);
                ctx.emit(
                    VInsn { op: Op::ConstI32, a: v_len, b: Opnd::Z, c: Opnd::Z, imm: vs.len() as u32 },
                    vec![],
                    vec![v_len.v],
                );
                let v_arr = ctx.new_v(arr_tid);
                ctx.emit(
                    VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_len.v), c: Opnd::Z, imm: 0 },
                    vec![v_len.v],
                    vec![v_arr.v],
                );
                for (i, part) in vs.iter().enumerate() {
                    let v_i = ctx.new_v(2);
                    ctx.emit(
                        VInsn { op: Op::ConstI32, a: v_i, b: Opnd::Z, c: Opnd::Z, imm: i as u32 },
                        vec![],
                        vec![v_i.v],
                    );
                    ctx.emit(
                        VInsn { op: Op::ArraySet, a: *part, b: Opnd::V(v_arr.v), c: Opnd::V(v_i.v), imm: 0 },
                        vec![part.v, v_arr.v, v_i.v],
                        vec![],
                    );
                }
                Ok(v_arr)
            }
            ExprKind::TupleLit(elems) => {
                let mut vs: Vec<VTmp> = Vec::with_capacity(elems.len());
                let mut elem_tids: Vec<u32> = Vec::with_capacity(elems.len());
                for e in elems {
                    let v = compile_expr(e, env, ctx)?;
                    elem_tids.push(ctx.vtypes[v.v as usize]);
                    vs.push(v);
                }

                let tup_tid = ctx.type_ctx.intern_tuple_type(&elem_tids);
                let v_tup = ctx.new_v(tup_tid);
                ctx.emit(
                    VInsn { op: Op::ObjNew, a: v_tup, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_tup.v],
                );

                for (i, v) in vs.iter().enumerate() {
                    let key = i.to_string();
                    let atom_id = if let Some(&id) = ctx.atom_ids.get(&key) {
                        id
                    } else {
                        let id = ctx.atoms.len() as u32;
                        ctx.atoms.push(key.as_bytes().to_vec());
                        ctx.atom_ids.insert(key, id);
                        id
                    };
                    ctx.emit(
                        VInsn { op: Op::ObjSetAtom, a: *v, b: Opnd::V(v_tup.v), c: Opnd::Z, imm: atom_id },
                        vec![v.v, v_tup.v],
                        vec![],
                    );
                }

                Ok(v_tup)
            }
            ExprKind::ObjLit(fields) => {
                let out_tid = match expect {
                    Some(et) => ctx
                        .type_ctx
                        .types
                        .get(et as usize)
                        .is_some_and(|te| te.kind == TypeKind::Object)
                        .then_some(et)
                        .unwrap_or(6),
                    None => 6,
                };

                let v_obj = ctx.new_v(out_tid);
                ctx.emit(
                    VInsn { op: Op::ObjNew, a: v_obj, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_obj.v],
                );
                for (k, ve) in fields {
                    let v_val = compile_expr(ve, env, ctx)?;
                    let atom_id = if let Some(&id) = ctx.atom_ids.get(k) {
                        id
                    } else {
                        let id = ctx.atoms.len() as u32;
                        ctx.atoms.push(k.as_bytes().to_vec());
                        ctx.atom_ids.insert(k.clone(), id);
                        id
                    };
                    ctx.emit(
                        VInsn { op: Op::ObjSetAtom, a: v_val, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                        vec![v_val.v, v_obj.v],
                        vec![],
                    );
                }
                Ok(v_obj)
            }
            ExprKind::Index { base, index } => {
                let v_base = compile_expr(base, env, ctx)?;
                let v_idx = compile_expr(index, env, ctx)?;
                if ctx.vtypes[v_idx.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, index.span, "index must be i32"));
                }
                match ctx.vtypes[v_base.v as usize] {
                    4 => {
                        let out = ctx.new_v(2);
                        ctx.emit(
                            VInsn { op: Op::ArrayGet, a: out, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_base.v, v_idx.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    0 => {
                        let out = ctx.new_v(2);
                        ctx.emit(
                            VInsn { op: Op::BytesGetU8, a: out, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_base.v, v_idx.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    _ => Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "indexing only supported for Array<i32> and bytes for now",
                    )),
                }
            }
            ExprKind::New { proto, args } => {
                let v_proto = compile_expr(proto, env, ctx)?;
                let t_proto = ctx.vtypes[v_proto.v as usize];
                let te = ctx
                    .type_ctx
                    .types
                    .get(t_proto as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, proto.span, "bad prototype type id"))?;
                if te.kind != TypeKind::Object {
                    return Err(CompileError::new(ErrorKind::Type, proto.span, "new expects an Object prototype"));
                }

                let self_tid = match expect {
                    Some(et) if et == 6 => 6,
                    Some(et) => {
                        let te = ctx
                            .type_ctx
                            .types
                            .get(et as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad expected type id"))?;
                        if te.kind != TypeKind::Object {
                            t_proto
                        } else if et != t_proto {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "new: expected object type does not match prototype type",
                            ));
                        } else {
                            et
                        }
                    }
                    None => t_proto,
                };

                let v_self = ctx.new_v(self_tid);
                ctx.emit(
                    VInsn { op: Op::ObjNew, a: v_self, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_self.v],
                );

                // self.__proto__ = proto
                ctx.emit(
                    VInsn { op: Op::ObjSetAtom, a: v_proto, b: Opnd::V(v_self.v), c: Opnd::Z, imm: ATOM___PROTO__ },
                    vec![v_proto.v, v_self.v],
                    vec![],
                );

                // if proto has init, call init(self, ...args)
                let v_has = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::ObjHasAtom, a: v_has, b: Opnd::V(v_proto.v), c: Opnd::Z, imm: ATOM_INIT },
                    vec![v_proto.v],
                    vec![v_has.v],
                );
                let v_no = ctx.new_v(1);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: v_no, b: Opnd::V(v_has.v), c: Opnd::Z, imm: 0 },
                    vec![v_has.v],
                    vec![v_no.v],
                );

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: v_no, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![v_no.v],
                    vec![],
                );

                // Build expected init function type: (Object, Dynamic, ...) -> Object.
                let mut sig_args: Vec<u32> = Vec::with_capacity(1 + args.len());
                sig_args.push(6);
                sig_args.extend(std::iter::repeat(3u32).take(args.len()));
                let fun_tid = ctx.type_ctx.intern_fun_type(6, &sig_args);
                let sig_id = ctx.type_ctx.intern_sig(6, &sig_args);

                let v_init = ctx.new_v(fun_tid);
                ctx.emit(
                    VInsn { op: Op::ObjGetAtom, a: v_init, b: Opnd::V(v_proto.v), c: Opnd::Z, imm: ATOM_INIT },
                    vec![v_proto.v],
                    vec![v_init.v],
                );

                // Build call args: (Object, Dynamic, ...) -> Object.
                // Marshal `self` into an Object-typed vreg (tid 6) when needed.
                let v_self_obj = if self_tid == 6 {
                    v_self
                } else {
                    let v = ctx.new_v(6);
                    ctx.emit(
                        VInsn { op: Op::Mov, a: v, b: Opnd::V(v_self.v), c: Opnd::Z, imm: 0 },
                        vec![v_self.v],
                        vec![v.v],
                    );
                    v
                };

                let mut arg_vs: Vec<u32> = Vec::with_capacity(1 + args.len());
                arg_vs.push(v_self_obj.v);
                for a in args {
                    let v = compile_expr(a, env, ctx)?;
                    let vd = if ctx.vtypes[v.v as usize] == 3 {
                        v
                    } else {
                        let out = ctx.new_v(3);
                        ctx.emit(
                            VInsn { op: Op::ToDyn, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                            vec![v.v],
                            vec![out.v],
                        );
                        out
                    };
                    arg_vs.push(vd.v);
                }

                let v_tmp_ret = ctx.new_v(6);
                let pc = ctx.vinsns.len() as u32;
                ctx.call_sites.push(CallSite { pc, sig_id, args: arg_vs.clone() });

                let mut uses: Vec<u32> = Vec::with_capacity(1 + arg_vs.len());
                uses.push(v_init.v);
                uses.extend(arg_vs.iter().copied());
                ctx.emit(
                    VInsn { op: Op::CallR, a: v_tmp_ret, b: Opnd::V(v_init.v), c: Opnd::Z, imm: arg_vs.len() as u32 },
                    uses,
                    vec![v_tmp_ret.v],
                );

                // Patch jump over the init call when missing.
                let join_pc = ctx.vinsns.len() as u32;
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_if_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_join as u32;

                Ok(v_self)
            }
            ExprKind::Match { .. } => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "match is not supported in the AST backend yet (use --backend ir)",
            )),
            ExprKind::TypeApp { .. } => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "templates are not supported in the AST backend yet (use --backend ir)",
            )),
            ExprKind::Fn { .. } => Err(CompileError::new(ErrorKind::Type, e.span, "fn literal not supported here yet")),
        }
    }

    fn compile_stmt(s: &Stmt, env: &mut HashMap<String, VTmp>, ctx: &mut CompileCtx) -> Result<(), CompileError> {
        match &s.node {
            StmtKind::ImportModule { .. } | StmtKind::ImportFrom { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "modules are not supported in the AST backend yet (use --backend ir)",
            )),
            StmtKind::Prototype { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "prototype must be expanded before AST backend compilation",
            )),
            StmtKind::Let {
                exported,
                name,
                type_params,
                ty,
                expr,
            } => {
                if *exported {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "modules are not supported in the AST backend yet (use --backend ir)",
                    ));
                }
                if !type_params.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "templates are not supported in the AST backend yet (use --backend ir)",
                    ));
                }
                if env.contains_key(name) {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        format!("duplicate let binding '{}'", name),
                    ));
                }
                match ty {
                    None => {
                        if matches!(expr.node, ExprKind::Fn { .. }) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                expr.span,
                                "fn literal requires a type annotation on its let binding",
                            ));
                        }
                        let v = compile_expr(expr, env, ctx)?;
                        env.insert(name.clone(), v);
                        Ok(())
                    }
                    Some(ty) => {
                        if let ExprKind::Fn { params, body, tail } = &expr.node {
                            let fun_tid = ctx.type_ctx.resolve_ty(ty)?;
                            let te = ctx
                                .type_ctx
                                .types
                                .get(fun_tid as usize)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, ty.span, "bad function type id"))?;
                            if te.kind != TypeKind::Function {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    ty.span,
                                    "fn literal requires a function type annotation",
                                ));
                            }

                            let sig_id = te.p0 as usize;
                            let sig = ctx
                                .type_ctx
                                .sigs
                                .get(sig_id)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, ty.span, "bad fun sig id"))?
                                .clone();

                            if params.len() != sig.args.len() {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    expr.span,
                                    "function literal arity does not match annotated type",
                                ));
                            }
                            for (i, (_pn, pty)) in params.iter().enumerate() {
                                if let Some(pt) = pty {
                                    let ptid = ctx.type_ctx.resolve_ty(pt)?;
                                    if ptid != sig.args[i] {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            pt.span,
                                            "function parameter type does not match annotated type",
                                        ));
                                    }
                                }
                            }

                            // Build a very small typed function (no captures) and reference it via CONST_FUN.
                            let mut reg_types: Vec<u32> = sig.args.clone();
                            let mut insns: Vec<Insn> = Vec::new();
                            let mut param_regs: HashMap<String, u8> = HashMap::new();
                            for (i, (pn, _)) in params.iter().enumerate() {
                                param_regs.insert(pn.clone(), i as u8);
                            }

                            fn alloc_reg(reg_types: &mut Vec<u32>, tid: u32, span: Span) -> Result<u8, CompileError> {
                                if reg_types.len() >= 256 {
                                    return Err(CompileError::new(
                                        ErrorKind::Codegen,
                                        span,
                                        "register allocation exceeded 256 regs",
                                    ));
                                }
                                reg_types.push(tid);
                                Ok((reg_types.len() - 1) as u8)
                            }

                            fn compile_small_expr(
                                e: &Expr,
                                param_regs: &HashMap<String, u8>,
                                out_reg_types: &mut Vec<u32>,
                                insns: &mut Vec<Insn>,
                            ) -> Result<(u8, u32), CompileError> {
                                match &e.node {
                                    ExprKind::Var(n) => {
                                        let r = *param_regs.get(n).ok_or_else(|| {
                                            CompileError::new(ErrorKind::Name, e.span, format!("unknown variable '{}'", n))
                                        })?;
                                        Ok((r, out_reg_types[r as usize]))
                                    }
                                    ExprKind::I32Lit(x) => {
                                        let r = alloc_reg(out_reg_types, 2, e.span)?;
                                        insns.push(Insn { op: Op::ConstI32 as u8, a: r, b: 0, c: 0, imm: *x as u32 });
                                        Ok((r, 2))
                                    }
                                    ExprKind::Index { base, index } => {
                                        let (rb, tb) = compile_small_expr(base, param_regs, out_reg_types, insns)?;
                                        let (ri, ti) = compile_small_expr(index, param_regs, out_reg_types, insns)?;
                                        if ti != 2 {
                                            return Err(CompileError::new(ErrorKind::Type, index.span, "index must be i32"));
                                        }
                                        match tb {
                                            4 => {
                                                let ro = alloc_reg(out_reg_types, 2, e.span)?;
                                                insns.push(Insn { op: Op::ArrayGet as u8, a: ro, b: rb, c: ri, imm: 0 });
                                                Ok((ro, 2))
                                            }
                                            0 => {
                                                let ro = alloc_reg(out_reg_types, 2, e.span)?;
                                                insns.push(Insn { op: Op::BytesGetU8 as u8, a: ro, b: rb, c: ri, imm: 0 });
                                                Ok((ro, 2))
                                            }
                                            _ => Err(CompileError::new(
                                                ErrorKind::Type,
                                                e.span,
                                                "indexing not supported for this type",
                                            )),
                                        }
                                    }
                                    _ => Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        "unsupported expression in fn literal (MVP)",
                                    )),
                                }
                            }

                            // Choose return expression: first explicit return, else tail.
                            let mut ret_expr: Option<&Expr> = None;
                            for st in body {
                                match &st.node {
                                    StmtKind::Return { expr: Some(e) } => {
                                        if ret_expr.is_some() {
                                            return Err(CompileError::new(
                                                ErrorKind::Type,
                                                st.span,
                                                "multiple returns in fn literal not supported yet",
                                            ));
                                        }
                                        ret_expr = Some(e);
                                    }
                                    StmtKind::Return { expr: None } => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            st.span,
                                            "return; without value not supported",
                                        ))
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            st.span,
                                            "only 'return <expr>;' supported in fn literal body for now",
                                        ))
                                    }
                                }
                            }
                            let tail_expr = tail.as_deref();
                            let re = ret_expr
                                .or(tail_expr)
                                .ok_or_else(|| CompileError::new(ErrorKind::Type, expr.span, "fn literal has no return value"))?;

                            let (rr, rt) = compile_small_expr(re, &param_regs, &mut reg_types, &mut insns)?;
                            if rt != sig.ret_type {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    re.span,
                                    "fn literal return type does not match annotation",
                                ));
                            }
                            insns.push(Insn { op: Op::Ret as u8, a: rr, b: 0, c: 0, imm: 0 });

                            let fn_index = PRELUDE_FUN_COUNT + 1u32 + (ctx.nested_funcs.len() as u32);
                            ctx.nested_funcs.push(Function { reg_types, insns });

                            let v_fun = ctx.new_v(fun_tid);
                            ctx.emit(
                                VInsn { op: Op::ConstFun, a: v_fun, b: Opnd::Z, c: Opnd::Z, imm: fn_index },
                                vec![],
                                vec![v_fun.v],
                            );
                            env.insert(name.clone(), v_fun);
                            return Ok(());
                        }

                        let expected_tid = ctx.type_ctx.resolve_ty(ty)?;

                        // Empty array literals require an expected type. Allow them only in typed
                        // `let` initializers for now.
                        if let ExprKind::ArrayLit(elems) = &expr.node {
                            if elems.is_empty() {
                                match expected_tid {
                                    4 | 5 => {
                                        let v_len = ctx.new_v(2);
                                        ctx.emit(
                                            VInsn { op: Op::ConstI32, a: v_len, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                                            vec![],
                                            vec![v_len.v],
                                        );
                                        let v_arr = ctx.new_v(expected_tid);
                                        ctx.emit(
                                            VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_len.v), c: Opnd::Z, imm: 0 },
                                            vec![v_len.v],
                                            vec![v_arr.v],
                                        );
                                        env.insert(name.clone(), v_arr);
                                        return Ok(());
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            expr.span,
                                            "empty array literal requires an Array<i32> or Array<bytes> annotation",
                                        ))
                                    }
                                }
                            }
                        }

                        let v = compile_expr_expect(expr, Some(expected_tid), env, ctx)?;
                        if ctx.vtypes[v.v as usize] != expected_tid {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                s.span,
                                "let annotation does not match expression type",
                            ));
                        }
                        env.insert(name.clone(), v);
                        Ok(())
                    }
                }
            }
            StmtKind::Assign { name, expr } => {
                let dst = env
                    .get(name)
                    .copied()
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, format!("unknown variable '{}'", name)))?;
                let rhs = compile_expr(expr, env, ctx)?;
                let td = ctx.vtypes[dst.v as usize];
                let tr = ctx.vtypes[rhs.v as usize];
                if td != tr {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        format!("assignment to '{}' changes type", name),
                    ));
                }
                ctx.allow_multi_def[dst.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: dst, b: Opnd::V(rhs.v), c: Opnd::Z, imm: 0 },
                    vec![rhs.v],
                    vec![dst.v],
                );
                Ok(())
            }
            StmtKind::MemberAssign { base, name, expr } => {
                let v_obj = compile_expr(base, env, ctx)?;
                let base_tid = ctx.vtypes[v_obj.v as usize];
                let te = ctx
                    .type_ctx
                    .types
                    .get(base_tid as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, base.span, "bad member assignment base type id"))?;
                if te.kind != TypeKind::Object {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        base.span,
                        "member assignment target must be Object",
                    ));
                }
                if ctx.type_ctx.is_tuple_type(base_tid) {
                    return Err(CompileError::new(ErrorKind::Type, base.span, "tuples are immutable"));
                }
                let v_val = compile_expr(expr, env, ctx)?;
                let atom_id = if let Some(&id) = ctx.atom_ids.get(name) {
                    id
                } else {
                    let id = ctx.atoms.len() as u32;
                    ctx.atoms.push(name.as_bytes().to_vec());
                    ctx.atom_ids.insert(name.clone(), id);
                    id
                };
                ctx.emit(
                    VInsn { op: Op::ObjSetAtom, a: v_val, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                    vec![v_val.v, v_obj.v],
                    vec![],
                );
                Ok(())
            }
            StmtKind::While { cond, body } => {
                let cond_pc = ctx.vinsns.len() as u32;
                let v_cond = compile_expr(cond, env, ctx)?;
                if ctx.vtypes[v_cond.v as usize] != 1 {
                    return Err(CompileError::new(ErrorKind::Type, cond.span, "while condition must be bool"));
                }

                // if(cond) jump to body, else jump to exit.
                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![v_cond.v],
                    vec![],
                );
                let jmp_exit_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                let body_pc = ctx.vinsns.len() as u32;
                ctx.loop_stack.push(LoopCtx { anchor: v_cond, breaks: Vec::new(), continues: Vec::new() });
                let mut scoped = env.clone();
                for bs in body {
                    compile_stmt(bs, &mut scoped, ctx)?;
                }
                let loop_ctx = ctx.loop_stack.pop().expect("loop ctx");

                let jmp_back_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                let exit_pc = ctx.vinsns.len() as u32;

                let delta_to_body: i32 = (body_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_exit: i32 = (exit_pc as i32) - ((jmp_exit_pc + 1) as i32);
                let delta_to_cond: i32 = (cond_pc as i32) - ((jmp_back_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_body as u32;
                ctx.vinsns[jmp_exit_pc as usize].imm = delta_to_exit as u32;
                ctx.vinsns[jmp_back_pc as usize].imm = delta_to_cond as u32;

                // Patch break/continue sites inside the body.
                for pc in loop_ctx.breaks {
                    let d: i32 = (exit_pc as i32) - ((pc + 1) as i32);
                    ctx.vinsns[pc as usize].imm = d as u32;
                }
                for pc in loop_ctx.continues {
                    let d: i32 = (cond_pc as i32) - ((pc + 1) as i32);
                    ctx.vinsns[pc as usize].imm = d as u32;
                }
                Ok(())
            }
            StmtKind::Break => {
                let anchor = ctx
                    .loop_stack
                    .last()
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "break used outside of while"))?
                    .anchor;
                let pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: anchor, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                ctx.loop_stack.last_mut().expect("loop ctx").breaks.push(pc);
                Ok(())
            }
            StmtKind::Continue => {
                let anchor = ctx
                    .loop_stack
                    .last()
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "continue used outside of while"))?
                    .anchor;
                let pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: anchor, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                ctx.loop_stack.last_mut().expect("loop ctx").continues.push(pc);
                Ok(())
            }
            StmtKind::Throw { expr } => {
                let v = compile_expr(expr, env, ctx)?;
                let payload = if ctx.vtypes[v.v as usize] == 3 {
                    v
                } else {
                    let vd = ctx.new_v(3);
                    ctx.emit(VInsn { op: Op::ToDyn, a: vd, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 }, vec![v.v], vec![vd.v]);
                    vd
                };
                ctx.emit(
                    VInsn { op: Op::Throw, a: payload, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![payload.v],
                    vec![],
                );
                Ok(())
            }
            StmtKind::IndexAssign { base, index, expr } => {
                let v_base = compile_expr(base, env, ctx)?;
                let v_idx = compile_expr(index, env, ctx)?;
                let v_val = compile_expr(expr, env, ctx)?;
                if ctx.vtypes[v_idx.v as usize] != 2 {
                    return Err(CompileError::new(ErrorKind::Type, index.span, "index must be i32"));
                }
                match ctx.vtypes[v_base.v as usize] {
                    4 => {
                        if ctx.vtypes[v_val.v as usize] != 2 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                expr.span,
                                "Array<i32> index assignment requires i32 value",
                            ));
                        }
                        ctx.emit(
                            VInsn { op: Op::ArraySet, a: v_val, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_val.v, v_base.v, v_idx.v],
                            vec![],
                        );
                        Ok(())
                    }
                    0 => {
                        if ctx.vtypes[v_val.v as usize] != 2 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                expr.span,
                                "bytes index assignment requires i32 value",
                            ));
                        }
                        ctx.emit(
                            VInsn { op: Op::BytesSetU8, a: v_val, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_val.v, v_base.v, v_idx.v],
                            vec![],
                        );
                        Ok(())
                    }
                    _ => Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "index assignment only supported for Array<i32> and bytes",
                    )),
                }
            }
            StmtKind::Return { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "return only allowed in fn bodies (not implemented yet)",
            )),
        }
    }

    struct LoopCtx {
        anchor: VTmp,
        breaks: Vec<u32>,
        continues: Vec<u32>,
    }

    let mut const_bytes: Vec<Vec<u8>> = Vec::new();
    let mut vtypes: Vec<u32> = Vec::new();
    let mut next_v: u32 = 0;
    let mut allow_multi_def: Vec<bool> = Vec::new();
    let mut call_sites: Vec<CallSite> = Vec::new();
    let mut vinsns: Vec<VInsn> = Vec::new();
    let mut infos: Vec<InstrInfo> = Vec::new();

    let mut loop_stack: Vec<LoopCtx> = Vec::new();
    // Reserve well-known atoms at stable ids (see `ATOM_*` constants).
    let mut atoms: Vec<Vec<u8>> = vec![b"__proto__".to_vec(), b"init".to_vec()];
    let mut atom_ids: HashMap<String, u32> = HashMap::from([
        ("__proto__".to_string(), ATOM___PROTO__),
        ("init".to_string(), ATOM_INIT),
    ]);

    let mut ctx = CompileCtx {
        type_ctx: &mut type_ctx,
        nested_funcs: &mut nested_funcs,
        const_bytes: &mut const_bytes,
        vtypes: &mut vtypes,
        allow_multi_def: &mut allow_multi_def,
        next_v: &mut next_v,
        vinsns: &mut vinsns,
        infos: &mut infos,
        call_sites: &mut call_sites,
        loop_stack: &mut loop_stack,
        atoms: &mut atoms,
        atom_ids: &mut atom_ids,
    };

    for s in &p.stmts {
        compile_stmt(s, &mut env, &mut ctx)?;
    }

    // final expr must be bytes for our MVP
    let outv = compile_expr(&p.expr, &env, &mut ctx)?;
    if ctx.vtypes[outv.v as usize] != 0 {
        return Err(CompileError::new(
            ErrorKind::Type,
            p.expr.span,
            "program must evaluate to bytes",
        ));
    }
    ctx.emit(VInsn { op: Op::Ret, a: outv, b: Opnd::Z, c: Opnd::Z, imm: 0 }, vec![outv.v], vec![]);

    // End compilation borrows; emit phase reads the finalized vectors.
    drop(ctx);

    // Type-aware LSRA with opt-in multi-def per join dest.
    let num_vregs = next_v;
    let mut type_ids: Vec<u32> = vtypes.iter().copied().collect();
    type_ids.sort();
    type_ids.dedup();

    let mut vreg_to_reg: Vec<u8> = vec![0; num_vregs as usize];
    let mut reg_types: Vec<u32> = Vec::new();

    let mut base: u16 = 0;
    for tid in type_ids {
        let mut globals: Vec<u32> = Vec::new();
        for (gv, &t) in vtypes.iter().enumerate() {
            if t == tid {
                globals.push(gv as u32);
            }
        }
        let mut g2l: Vec<Option<u32>> = vec![None; num_vregs as usize];
        for (li, &gv) in globals.iter().enumerate() {
            g2l[gv as usize] = Some(li as u32);
        }

        let mut allow_local: Vec<bool> = vec![false; globals.len()];
        for (li, &gv) in globals.iter().enumerate() {
            allow_local[li] = allow_multi_def[gv as usize];
        }

        let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
        for ins in &infos {
            let mut uses = Vec::new();
            let mut defs = Vec::new();
            for &u in &ins.uses {
                if let Some(li) = g2l[u.0 as usize] {
                    uses.push(VReg(li));
                }
            }
            for &d in &ins.defs {
                if let Some(li) = g2l[d.0 as usize] {
                    defs.push(VReg(li));
                }
            }
            cls_instrs.push(InstrInfo { uses, defs });
        }

        let spillable = vec![false; globals.len()];
        let alloc = regalloc::lsra_allocate(
            256,
            globals.len() as u32,
            &cls_instrs,
            &spillable,
            SpillPolicy::Forbid,
            Some(&allow_local),
        )
        .map_err(|e| CompileError::new(ErrorKind::Codegen, Span::point(0), format!("register allocation failed: {:?}", e)))?;

        let used = alloc.used_pregs;
        if base as u32 + used as u32 > 256 {
            return Err(CompileError::new(ErrorKind::Codegen, Span::point(0), "register allocation exceeded 256 regs"));
        }

        let need = base as usize + used as usize;
        if reg_types.len() < need {
            reg_types.resize(need, 0);
        }
        for i in 0..used {
            reg_types[base as usize + i as usize] = tid;
        }

        for (li, &gv) in globals.iter().enumerate() {
            let p = alloc
                .vreg_to_preg[li]
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "unexpected spill"))?;
            vreg_to_reg[gv as usize] = (base + p.0) as u8;
        }

        base += used;
    }

    // Emit final bytecode (CALLR expands to MOV* + CALLR, so we must re-patch jump deltas).
    let mut call_by_pc: Vec<Option<usize>> = vec![None; vinsns.len()];
    for (i, cs) in call_sites.iter().enumerate() {
        call_by_pc[cs.pc as usize] = Some(i);
    }

    let mut need_sigs: Vec<u32> = call_sites.iter().map(|cs| cs.sig_id).collect();
    need_sigs.sort();
    need_sigs.dedup();

    let mut arg_block_start: HashMap<u32, u8> = HashMap::new();
    for sig_id in need_sigs {
        let sig = type_ctx
            .sigs
            .get(sig_id as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad fun sig id"))?;
        if reg_types.len() + sig.args.len() > 256 {
            return Err(CompileError::new(ErrorKind::Codegen, Span::point(0), "register allocation exceeded 256 regs"));
        }
        let start = reg_types.len() as u8;
        for &tid in &sig.args {
            reg_types.push(tid);
        }
        arg_block_start.insert(sig_id, start);
    }

    let mut old_to_new: Vec<u32> = vec![0; vinsns.len() + 1];
    let mut npc: u32 = 0;
    for (opc, _vi) in vinsns.iter().enumerate() {
        old_to_new[opc] = npc;
        if let Some(ci) = call_by_pc[opc] {
            npc += 1 + (call_sites[ci].args.len() as u32);
        } else {
            npc += 1;
        }
    }
    old_to_new[vinsns.len()] = npc;

    let extra_movs: usize = call_sites.iter().map(|cs| cs.args.len()).sum();
    let mut insns: Vec<Insn> = Vec::with_capacity(vinsns.len() + extra_movs);

    for (opc, vi) in vinsns.iter().enumerate() {
        if let Some(ci) = call_by_pc[opc] {
            let cs = &call_sites[ci];
            let start = *arg_block_start
                .get(&cs.sig_id)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "missing arg block"))?;

            for (i, &av) in cs.args.iter().enumerate() {
                let dst = start
                    .checked_add(i as u8)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "arg block overflow"))?;
                let src = vreg_to_reg[av as usize];
                insns.push(Insn { op: Op::Mov as u8, a: dst, b: src, c: 0, imm: 0 });
            }

            let nargs = cs.args.len() as u8;
            let dst = vreg_to_reg[vi.a.v as usize];
            let callee = match vi.b {
                Opnd::V(x) => vreg_to_reg[x as usize],
                Opnd::Z => return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad call callee operand")),
            };
            insns.push(Insn { op: Op::CallR as u8, a: dst, b: callee, c: nargs, imm: start as u32 });
            continue;
        }

        match vi.op {
            Op::ConstBool => {
                insns.push(Insn {
                    op: Op::ConstBool as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: (vi.imm & 1) as u8,
                    imm: 0,
                });
            }
            Op::JmpIf => {
                let old_target = (opc as i32 + 1) + (vi.imm as i32);
                if old_target < 0 || (old_target as usize) > vinsns.len() {
                    return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad jmp_if target"));
                }
                let new_pc = old_to_new[opc];
                let new_target = old_to_new[old_target as usize];
                let new_delta: i32 = (new_target as i32) - ((new_pc + 1) as i32);
                insns.push(Insn {
                    op: Op::JmpIf as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: 0,
                    imm: new_delta as u32,
                });
            }
            Op::Jmp => {
                let old_target = (opc as i32 + 1) + (vi.imm as i32);
                if old_target < 0 || (old_target as usize) > vinsns.len() {
                    return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad jmp target"));
                }
                let new_pc = old_to_new[opc];
                let new_target = old_to_new[old_target as usize];
                let new_delta: i32 = (new_target as i32) - ((new_pc + 1) as i32);
                insns.push(Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: new_delta as u32 });
            }
            Op::Try => {
                let old_target = (opc as i32 + 1) + (vi.imm as i32);
                if old_target < 0 || (old_target as usize) > vinsns.len() {
                    return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad try target"));
                }
                let new_pc = old_to_new[opc];
                let new_target = old_to_new[old_target as usize];
                let new_delta: i32 = (new_target as i32) - ((new_pc + 1) as i32);
                insns.push(Insn { op: Op::Try as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: new_delta as u32 });
            }
            Op::EndTry => {
                insns.push(Insn { op: Op::EndTry as u8, a: 0, b: 0, c: 0, imm: 0 });
            }
            Op::Throw => {
                insns.push(Insn { op: Op::Throw as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            Op::Ret => {
                insns.push(Insn { op: Op::Ret as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            Op::CallR => return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "CALLR without callsite record")),
            _ => {
                let b = match vi.b { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                let c = match vi.c { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                insns.push(Insn {
                    op: vi.op as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b,
                    c,
                    imm: vi.imm,
                });
            }
        }
    }

    let f0 = Function { reg_types, insns };
    let mut funcs: Vec<Function> = Vec::with_capacity((PRELUDE_FUN_COUNT as usize) + 1 + nested_funcs.len());
    // Inject prelude at fixed indices 0..PRELUDE_FUN_COUNT.
    funcs.extend(prelude_funcs_for_program());
    funcs.push(f0);
    funcs.extend(nested_funcs);
    Ok(Module {
        types: type_ctx.types,
        sigs: type_ctx.sigs,
        atoms,
        const_bytes,
        funcs,
        entry: PRELUDE_FUN_COUNT,
    })
}

