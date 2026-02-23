use std::io::{self, Read, Write};

use super::io_helpers::{rd_u16le, rd_u32le, rd_u8, wr_u16le, wr_u32le, wr_u8};
use super::{
    FunSig, Function, Insn, Module, Op, TypeEntry, TypeKind, MAGIC_JLYB, PRELUDE_FUN_COUNT, VERSION,
};

impl Module {
    pub fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
        // CAP_START feature:
        //
        // We must include `cap_start` in the bytecode when any closure with captures may exist.
        // Note that `cap_start` can legitimately be 0 (eg a capture-only function with no params),
        // so `cap_start > 0` is NOT a sufficient signal by itself.
        let feat_cap_start = self.funcs.iter().any(|f| {
            f.cap_start > 0
                || f.insns
                    .iter()
                    .any(|i| i.op == Op::Closure as u8 && i.c != 0)
        });
        let features: u32 = (if self.const_i64.is_empty() && self.const_f64.is_empty() {
                0
            } else {
                1u32 << 0
            }) // CONST64
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
        let magic = rd_u32le(r)?;
        if magic != MAGIC_JLYB {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "bad magic"));
        }
        let version = rd_u32le(r)?;
        if version != VERSION {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unsupported version",
            ));
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
            funcs.push(Function {
                reg_types,
                cap_start,
                insns,
            });
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
