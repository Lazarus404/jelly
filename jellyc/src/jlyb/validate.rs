use crate::jlyb::{Insn, Module, Op, TypeKind, NATIVE_BUILTIN_COUNT};

fn type_kind(m: &Module, tid: u32) -> Result<TypeKind, String> {
    m.types
        .get(tid as usize)
        .map(|te| te.kind)
        .ok_or_else(|| format!("type id out of range: tid={tid} (types_len={})", m.types.len()))
}

fn rk(m: &Module, reg_types: &[u32], r: u8) -> Result<TypeKind, String> {
    let tid = *reg_types
        .get(r as usize)
        .ok_or_else(|| format!("reg out of range: r={r} (nregs={})", reg_types.len()))?;
    type_kind(m, tid)
}

fn slot_size_bytes(k: TypeKind) -> usize {
    match k {
        TypeKind::I8
        | TypeKind::I16
        | TypeKind::I32
        | TypeKind::F32
        | TypeKind::F16
        | TypeKind::Bool
        | TypeKind::Atom => 4,
        TypeKind::I64 | TypeKind::F64 => 8,
        TypeKind::Dynamic
        | TypeKind::Bytes
        | TypeKind::Function
        | TypeKind::List
        | TypeKind::Array
        | TypeKind::Object
        | TypeKind::Abstract => std::mem::size_of::<usize>(),
    }
}

fn is_i32ish(k: TypeKind) -> bool {
    matches!(k, TypeKind::I8 | TypeKind::I16 | TypeKind::I32)
}

fn is_ptr_kind(k: TypeKind) -> bool {
    matches!(
        k,
        TypeKind::Bytes | TypeKind::Function | TypeKind::List | TypeKind::Array | TypeKind::Object | TypeKind::Abstract
    )
}

fn unary_elem_tid(m: &Module, container_tid: u32, want: TypeKind, why: &'static str) -> Result<u32, String> {
    let te = m.types.get(container_tid as usize).ok_or_else(|| format!("{why}: type id out of range"))?;
    if te.kind != want {
        return Err(format!("{why}: expected {want:?} container kind, got {:?}", te.kind));
    }
    Ok(te.p0)
}

fn err(func_i: usize, pc: usize, msg: impl Into<String>) -> String {
    format!("func[{func_i}] pc={pc}: {}", msg.into())
}

fn validate_switch_kind_tables(func_i: usize, insns: &[Insn]) -> Result<(), String> {
    let ninsns = insns.len() as i32;
    let mut pc: usize = 0;
    while pc < insns.len() {
        let ins = &insns[pc];
        if ins.op == Op::SwitchKind as u8 {
            let ncases = ins.b as usize;
            let table_first = pc + 1;
            let table_end = table_first + ncases;
            if table_end > insns.len() {
                return Err(err(func_i, pc, "switch_kind case table out of range"));
            }
            // Default delta is relative to end-of-table.
            let dd = ins.imm as i32;
            let dtgt = (table_end as i32) + dd;
            if dtgt < 0 || dtgt > ninsns {
                return Err(err(func_i, pc, "switch_kind default target out of range"));
            }
            for i in 0..ncases {
                let ci = &insns[table_first + i];
                if ci.op != Op::CaseKind as u8 {
                    return Err(err(func_i, pc, "switch_kind expects case_kind entries"));
                }
                let cd = ci.imm as i32;
                let ctgt = (table_end as i32) + cd;
                if ctgt < 0 || ctgt > ninsns {
                    return Err(err(func_i, table_first + i, "case_kind target out of range"));
                }
            }
            pc = table_end;
            continue;
        }
        if ins.op == Op::CaseKind as u8 {
            return Err(err(func_i, pc, "case_kind without preceding switch_kind"));
        }
        pc += 1;
    }
    Ok(())
}

pub fn validate_module(m: &Module) -> Result<(), String> {
    // Logical index: 0=native, 1..nfuncs=bytecode.
    let nfuncs_logical_max = m.funcs.len() as u32;

    for (fi, f) in m.funcs.iter().enumerate() {
        let nregs = f.reg_types.len() as u32;
        if nregs > 256 {
            return Err(format!("func[{fi}]: too many regs (nregs={nregs})"));
        }

        // Validate reg type ids.
        for (ri, &tid) in f.reg_types.iter().enumerate() {
            if (tid as usize) >= m.types.len() {
                return Err(format!(
                    "func[{fi}]: reg_types[{ri}] tid out of range: tid={tid} (types_len={})",
                    m.types.len()
                ));
            }
        }

        validate_switch_kind_tables(fi, &f.insns)?;

        for (pc, ins) in f.insns.iter().enumerate() {
            let op = ins.op;
            match op {
                x if x == Op::Nop as u8 => {}
                x if x == Op::Ret as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "ret reg out of range"));
                    }
                }
                x if x == Op::Mov as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "mov reg out of range"));
                    }
                    // Match VM validation: allow mov between different type IDs if either:
                    // - kinds match (nominal subtypes), OR
                    // - slot sizes match (raw copy).
                    if f.reg_types[ins.a as usize] != f.reg_types[ins.b as usize] {
                        let ka = rk(m, &f.reg_types, ins.a)?;
                        let kb = rk(m, &f.reg_types, ins.b)?;
                        let sa = slot_size_bytes(ka);
                        let sb = slot_size_bytes(kb);
                        if ka == kb {
                            // same kind, different type IDs - ok
                        } else if sa == sb && sa > 0 {
                            // same slot size - ok
                        } else {
                            return Err(err(fi, pc, "mov type mismatch"));
                        }
                    }
                }
                x if x == Op::Jmp as u8 => {
                    let d = ins.imm as i32;
                    let tgt = (pc as i32 + 1) + d;
                    if tgt < 0 || tgt > (f.insns.len() as i32) {
                        return Err(err(fi, pc, "jmp target out of range"));
                    }
                }
                x if x == Op::JmpIf as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "jmp_if cond reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "jmp_if cond must be bool"));
                    }
                    let d = ins.imm as i32;
                    let tgt = (pc as i32 + 1) + d;
                    if tgt < 0 || tgt > (f.insns.len() as i32) {
                        return Err(err(fi, pc, "jmp_if target out of range"));
                    }
                }
                x if x == Op::Assert as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "assert cond reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "assert cond must be bool"));
                    }
                }
                x if x == Op::Try as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "try reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "try dst must be Dynamic"));
                    }
                    if ins.b > 1 {
                        return Err(err(fi, pc, "try b must be 0/1 (trap_only flag)"));
                    }
                    let d = ins.imm as i32;
                    let tgt = (pc as i32 + 1) + d;
                    if tgt < 0 || tgt > (f.insns.len() as i32) {
                        return Err(err(fi, pc, "try catch target out of range"));
                    }
                }
                x if x == Op::EndTry as u8 => {}
                x if x == Op::Throw as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "throw reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "throw payload must be Dynamic"));
                    }
                }
                x if x == Op::Closure as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "closure reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Function {
                        return Err(err(fi, pc, "closure dst must be function"));
                    }
                    if ins.imm > nfuncs_logical_max {
                        return Err(err(fi, pc, "closure func index out of range"));
                    }
                    let first = ins.b as u32;
                    let ncaps = ins.c as u32;
                    if first + ncaps > nregs {
                        return Err(err(fi, pc, "closure capture range out of range"));
                    }
                }
                x if x == Op::BindThis as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "bind_this reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Function || rk(m, &f.reg_types, ins.b)? != TypeKind::Function {
                        return Err(err(fi, pc, "bind_this requires function regs"));
                    }
                }
                x if x == Op::Call as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "call reg out of range"));
                    }
                    if ins.imm > nfuncs_logical_max {
                        return Err(err(fi, pc, "call func index out of range"));
                    }
                    let first = ins.b as u32;
                    let nargs = ins.c as u32;
                    if first + nargs > nregs {
                        return Err(err(fi, pc, "call arg range out of range"));
                    }
                }
                x if x == Op::CallR as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "callr reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Function {
                        return Err(err(fi, pc, "callr callee must be function"));
                    }
                    let first = ins.imm;
                    let nargs = ins.c as u32;
                    if first >= nregs {
                        return Err(err(fi, pc, "callr arg base out of range"));
                    }
                    if first + nargs > nregs {
                        return Err(err(fi, pc, "callr arg range out of range"));
                    }
                }
                x if x == Op::ConstI32 as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    let k = rk(m, &f.reg_types, ins.a)?;
                    if k != TypeKind::I8 && k != TypeKind::I16 && k != TypeKind::I32 {
                        return Err(err(fi, pc, "const_i32 dst must be i8/i16/i32"));
                    }
                    // Range constraints for small ints (stored as i32 slots in the VM).
                    if k == TypeKind::I8 {
                        let v = ins.imm as i32;
                        if v < -128 || v > 127 {
                            return Err(err(fi, pc, "const_i32 out of range for i8"));
                        }
                    } else if k == TypeKind::I16 {
                        let v = ins.imm as i32;
                        if v < -32768 || v > 32767 {
                            return Err(err(fi, pc, "const_i32 out of range for i16"));
                        }
                    }
                }
                x if x == Op::ConstI8Imm as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I8 {
                        return Err(err(fi, pc, "const_i8_imm dst must be i8"));
                    }
                }
                x if x == Op::ConstF16 as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F16 {
                        return Err(err(fi, pc, "const_f16 dst must be f16"));
                    }
                }
                x if x == Op::ConstBool as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "const_bool dst must be bool"));
                    }
                    if ins.c > 1 {
                        return Err(err(fi, pc, "const_bool imm must be 0/1"));
                    }
                }
                x if x == Op::ConstNull as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "const_null dst must be Dynamic"));
                    }
                }
                x if x == Op::ConstAtom as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Atom {
                        return Err(err(fi, pc, "const_atom dst must be atom"));
                    }
                    if (ins.imm as usize) >= m.atoms.len() {
                        return Err(err(fi, pc, "const_atom atom id out of range"));
                    }
                }
                x if x == Op::ConstF32 as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32 {
                        return Err(err(fi, pc, "const_f32 dst must be f32"));
                    }
                }
                x if x == Op::ConstI64 as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I64 {
                        return Err(err(fi, pc, "const_i64 dst must be i64"));
                    }
                    if (ins.imm as usize) >= m.const_i64.len() {
                        return Err(err(fi, pc, "const_i64 pool index out of range"));
                    }
                }
                x if x == Op::ConstF64 as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F64 {
                        return Err(err(fi, pc, "const_f64 dst must be f64"));
                    }
                    if (ins.imm as usize) >= m.const_f64.len() {
                        return Err(err(fi, pc, "const_f64 pool index out of range"));
                    }
                }
                x if x == Op::ConstBytes as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bytes {
                        return Err(err(fi, pc, "const_bytes dst must be bytes"));
                    }
                    if (ins.imm as usize) >= m.const_bytes.len() {
                        return Err(err(fi, pc, "const_bytes pool index out of range"));
                    }
                }
                x if x == Op::ConstFun as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "const reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Function {
                        return Err(err(fi, pc, "const_fun dst must be function"));
                    }
                    if ins.imm > nfuncs_logical_max {
                        return Err(err(fi, pc, "const_fun func index out of range"));
                    }
                }
                x if x == Op::ToDyn as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "to_dyn reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "to_dyn dst must be Dynamic"));
                    }
                }
                x if x == Op::FromDynI8 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::I8 {
                        return Err(err(fi, pc, "from_dyn_i8 types required"));
                    }
                }
                x if x == Op::FromDynI16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::I16 {
                        return Err(err(fi, pc, "from_dyn_i16 types required"));
                    }
                }
                x if x == Op::FromDynI32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::I32 {
                        return Err(err(fi, pc, "from_dyn_i32 types required"));
                    }
                }
                x if x == Op::FromDynI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::I64 {
                        return Err(err(fi, pc, "from_dyn_i64 types required"));
                    }
                }
                x if x == Op::FromDynF16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::F16 {
                        return Err(err(fi, pc, "from_dyn_f16 types required"));
                    }
                }
                x if x == Op::FromDynF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::F32 {
                        return Err(err(fi, pc, "from_dyn_f32 types required"));
                    }
                }
                x if x == Op::FromDynF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::F64 {
                        return Err(err(fi, pc, "from_dyn_f64 types required"));
                    }
                }
                x if x == Op::FromDynBool as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "from_dyn_bool types required"));
                    }
                }
                x if x == Op::FromDynAtom as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic || rk(m, &f.reg_types, ins.a)? != TypeKind::Atom {
                        return Err(err(fi, pc, "from_dyn_atom types required"));
                    }
                }
                x if x == Op::FromDynPtr as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "dyn conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "from_dyn_ptr src must be Dynamic"));
                    }
                    let dk = rk(m, &f.reg_types, ins.a)?;
                    if !is_ptr_kind(dk) {
                        return Err(err(fi, pc, "from_dyn_ptr dst must be pointer-kind"));
                    }
                }
                x if x == Op::SpillPush as u8 || x == Op::SpillPop as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "spill reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "spill ops require Dynamic reg"));
                    }
                }
                x if x == Op::AddI32 as u8
                    || x == Op::SubI32 as u8
                    || x == Op::MulI32 as u8
                    || x == Op::DivI32 as u8 =>
                {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "arith reg out of range"));
                    }
                    if !is_i32ish(rk(m, &f.reg_types, ins.a)?) || !is_i32ish(rk(m, &f.reg_types, ins.b)?) || !is_i32ish(rk(m, &f.reg_types, ins.c)?) {
                        return Err(err(fi, pc, "arith i8/i16/i32 types required"));
                    }
                }
                x if x == Op::AddI32Imm as u8 || x == Op::SubI32Imm as u8 || x == Op::MulI32Imm as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "arith_imm reg out of range"));
                    }
                    if !is_i32ish(rk(m, &f.reg_types, ins.a)?) || !is_i32ish(rk(m, &f.reg_types, ins.b)?) {
                        return Err(err(fi, pc, "arith_imm i8/i16/i32 types required"));
                    }
                }
                x if x == Op::AddI64 as u8 || x == Op::SubI64 as u8 || x == Op::MulI64 as u8 || x == Op::DivI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "arith reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I64
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::I64
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::I64
                    {
                        return Err(err(fi, pc, "arith i64 types required"));
                    }
                }
                x if x == Op::AddF32 as u8 || x == Op::SubF32 as u8 || x == Op::MulF32 as u8 || x == Op::DivF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "arith reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::F32
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::F32
                    {
                        return Err(err(fi, pc, "arith f32 types required"));
                    }
                }
                x if x == Op::AddF16 as u8 || x == Op::SubF16 as u8 || x == Op::MulF16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "arith reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F16
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::F16
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::F16
                    {
                        return Err(err(fi, pc, "arith f16 types required"));
                    }
                }
                x if x == Op::AddF64 as u8 || x == Op::SubF64 as u8 || x == Op::MulF64 as u8 || x == Op::DivF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "arith reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F64
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::F64
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::F64
                    {
                        return Err(err(fi, pc, "arith f64 types required"));
                    }
                }
                x if x == Op::NegI32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "neg reg out of range"));
                    }
                    if !is_i32ish(rk(m, &f.reg_types, ins.a)?) || !is_i32ish(rk(m, &f.reg_types, ins.b)?) {
                        return Err(err(fi, pc, "neg_i32 types must be i8/i16/i32"));
                    }
                }
                x if x == Op::NegI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "neg reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I64 || rk(m, &f.reg_types, ins.b)? != TypeKind::I64 {
                        return Err(err(fi, pc, "neg_i64 types required"));
                    }
                }
                x if x == Op::NegF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "neg reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32 || rk(m, &f.reg_types, ins.b)? != TypeKind::F32 {
                        return Err(err(fi, pc, "neg_f32 types required"));
                    }
                }
                x if x == Op::NegF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "neg reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F64 || rk(m, &f.reg_types, ins.b)? != TypeKind::F64 {
                        return Err(err(fi, pc, "neg_f64 types required"));
                    }
                }
                x if x == Op::NotBool as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "not reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool || rk(m, &f.reg_types, ins.b)? != TypeKind::Bool {
                        return Err(err(fi, pc, "not_bool types required"));
                    }
                }
                x if x == Op::Physeq as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "physeq reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "physeq dst must be bool"));
                    }
                }
                x if x == Op::EqI32 as u8 || x == Op::LtI32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "eq/lt reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "eq/lt dst must be bool"));
                    }
                    if !is_i32ish(rk(m, &f.reg_types, ins.b)?) || !is_i32ish(rk(m, &f.reg_types, ins.c)?) {
                        return Err(err(fi, pc, "eq/lt_i32 operands must be i8/i16/i32"));
                    }
                }
                x if x == Op::EqI32Imm as u8 || x == Op::LtI32Imm as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "eq/lt_imm reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool {
                        return Err(err(fi, pc, "eq/lt_imm dst must be bool"));
                    }
                    if !is_i32ish(rk(m, &f.reg_types, ins.b)?) {
                        return Err(err(fi, pc, "eq/lt_imm src must be i8/i16/i32"));
                    }
                }
                x if x == Op::EqI64 as u8 || x == Op::LtI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "eq/lt reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::I64
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::I64
                    {
                        return Err(err(fi, pc, "eq/lt_i64 types required"));
                    }
                }
                x if x == Op::EqF32 as u8 || x == Op::LtF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "eq/lt reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::F32
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::F32
                    {
                        return Err(err(fi, pc, "eq/lt_f32 types required"));
                    }
                }
                x if x == Op::EqF64 as u8 || x == Op::LtF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "eq/lt reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::F64
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::F64
                    {
                        return Err(err(fi, pc, "eq/lt_f64 types required"));
                    }
                }
                x if x == Op::SextI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I64 {
                        return Err(err(fi, pc, "sext_i64 dst must be i64"));
                    }
                    if !is_i32ish(rk(m, &f.reg_types, ins.b)?) {
                        return Err(err(fi, pc, "sext_i64 src must be i8/i16/i32"));
                    }
                }
                x if x == Op::SextI16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I16 || rk(m, &f.reg_types, ins.b)? != TypeKind::I8 {
                        return Err(err(fi, pc, "sext_i16 dst must be i16, src i8"));
                    }
                }
                x if x == Op::TruncI8 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I8 {
                        return Err(err(fi, pc, "trunc_i8 dst must be i8"));
                    }
                    let sb = rk(m, &f.reg_types, ins.b)?;
                    if sb != TypeKind::I16 && sb != TypeKind::I32 {
                        return Err(err(fi, pc, "trunc_i8 src must be i16 or i32"));
                    }
                }
                x if x == Op::TruncI16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I16 || rk(m, &f.reg_types, ins.b)? != TypeKind::I32 {
                        return Err(err(fi, pc, "trunc_i16 dst must be i16, src i32"));
                    }
                }
                x if x == Op::I32FromI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::I64 {
                        return Err(err(fi, pc, "i32_from_i64 types required"));
                    }
                }
                x if x == Op::F64FromI32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F64 || rk(m, &f.reg_types, ins.b)? != TypeKind::I32 {
                        return Err(err(fi, pc, "f64_from_i32 types required"));
                    }
                }
                x if x == Op::I32FromF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::F64 {
                        return Err(err(fi, pc, "i32_from_f64 types required"));
                    }
                }
                x if x == Op::F64FromI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F64 || rk(m, &f.reg_types, ins.b)? != TypeKind::I64 {
                        return Err(err(fi, pc, "f64_from_i64 types required"));
                    }
                }
                x if x == Op::I64FromF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I64 || rk(m, &f.reg_types, ins.b)? != TypeKind::F64 {
                        return Err(err(fi, pc, "i64_from_f64 types required"));
                    }
                }
                x if x == Op::F32FromI32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32 || rk(m, &f.reg_types, ins.b)? != TypeKind::I32 {
                        return Err(err(fi, pc, "f32_from_i32 types required"));
                    }
                }
                x if x == Op::I32FromF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::F32 {
                        return Err(err(fi, pc, "i32_from_f32 types required"));
                    }
                }
                x if x == Op::F64FromF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F64 || rk(m, &f.reg_types, ins.b)? != TypeKind::F32 {
                        return Err(err(fi, pc, "f64_from_f32 types required"));
                    }
                }
                x if x == Op::F32FromF64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32 || rk(m, &f.reg_types, ins.b)? != TypeKind::F64 {
                        return Err(err(fi, pc, "f32_from_f64 types required"));
                    }
                }
                x if x == Op::F16FromF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F16 || rk(m, &f.reg_types, ins.b)? != TypeKind::F32 {
                        return Err(err(fi, pc, "f16_from_f32 types required"));
                    }
                }
                x if x == Op::F32FromF16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32 || rk(m, &f.reg_types, ins.b)? != TypeKind::F16 {
                        return Err(err(fi, pc, "f32_from_f16 types required"));
                    }
                }
                x if x == Op::F16FromI32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F16 || rk(m, &f.reg_types, ins.b)? != TypeKind::I32 {
                        return Err(err(fi, pc, "f16_from_i32 types required"));
                    }
                }
                x if x == Op::I32FromF16 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::F16 {
                        return Err(err(fi, pc, "i32_from_f16 types required"));
                    }
                }
                x if x == Op::F32FromI64 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::F32 || rk(m, &f.reg_types, ins.b)? != TypeKind::I64 {
                        return Err(err(fi, pc, "f32_from_i64 types required"));
                    }
                }
                x if x == Op::I64FromF32 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "conv reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I64 || rk(m, &f.reg_types, ins.b)? != TypeKind::F32 {
                        return Err(err(fi, pc, "i64_from_f32 types required"));
                    }
                }
                x if x == Op::Kindof as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "kindof reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::Dynamic {
                        return Err(err(fi, pc, "kindof types required"));
                    }
                }
                x if x == Op::SwitchKind as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "switch_kind reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 {
                        return Err(err(fi, pc, "switch_kind src must be i32"));
                    }
                    let ncases = ins.b as usize;
                    if pc + 1 + ncases > f.insns.len() {
                        return Err(err(fi, pc, "switch_kind case table out of range"));
                    }
                }
                x if x == Op::CaseKind as u8 => {}
                x if x == Op::ListNil as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "list nil reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::List {
                        return Err(err(fi, pc, "list_nil dst must be list"));
                    }
                }
                x if x == Op::ListCons as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "list cons reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::List {
                        return Err(err(fi, pc, "list_cons dst must be list"));
                    }
                    if rk(m, &f.reg_types, ins.c)? != TypeKind::List {
                        return Err(err(fi, pc, "list_cons tail must be list"));
                    }
                    if f.reg_types[ins.a as usize] != f.reg_types[ins.c as usize] {
                        return Err(err(fi, pc, "list_cons list type mismatch"));
                    }
                    let list_tid = f.reg_types[ins.a as usize];
                    let elem_tid = unary_elem_tid(m, list_tid, TypeKind::List, "list_cons")?;
                    if elem_tid != f.reg_types[ins.b as usize] {
                        return Err(err(fi, pc, "list_cons head type mismatch"));
                    }
                }
                x if x == Op::ListHead as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "list head reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::List {
                        return Err(err(fi, pc, "list_head src must be list"));
                    }
                    let list_tid = f.reg_types[ins.b as usize];
                    let elem_tid = unary_elem_tid(m, list_tid, TypeKind::List, "list_head")?;
                    if elem_tid != f.reg_types[ins.a as usize] {
                        return Err(err(fi, pc, "list_head dst type mismatch"));
                    }
                }
                x if x == Op::ListTail as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "list tail reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::List || rk(m, &f.reg_types, ins.b)? != TypeKind::List {
                        return Err(err(fi, pc, "list_tail types required"));
                    }
                    if f.reg_types[ins.a as usize] != f.reg_types[ins.b as usize] {
                        return Err(err(fi, pc, "list_tail list type mismatch"));
                    }
                }
                x if x == Op::ListIsNil as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "list is_nil reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool || rk(m, &f.reg_types, ins.b)? != TypeKind::List {
                        return Err(err(fi, pc, "list_is_nil types required"));
                    }
                }
                x if x == Op::ArrayNew as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "array new reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Array || rk(m, &f.reg_types, ins.b)? != TypeKind::I32 {
                        return Err(err(fi, pc, "array_new types required"));
                    }
                }
                x if x == Op::ArrayLen as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "array len reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::Array {
                        return Err(err(fi, pc, "array_len types required"));
                    }
                }
                x if x == Op::ArrayGet as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "array get reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Array || rk(m, &f.reg_types, ins.c)? != TypeKind::I32 {
                        return Err(err(fi, pc, "array_get types required"));
                    }
                    let arr_tid = f.reg_types[ins.b as usize];
                    let elem_tid = m
                        .types
                        .get(arr_tid as usize)
                        .map(|te| te.p0)
                        .ok_or_else(|| err(fi, pc, "array_get array type entry out of range"))?;
                    if elem_tid != f.reg_types[ins.a as usize] {
                        return Err(err(fi, pc, "array_get dst type mismatch"));
                    }
                }
                x if x == Op::ArraySet as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "array set reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Array || rk(m, &f.reg_types, ins.c)? != TypeKind::I32 {
                        return Err(err(fi, pc, "array_set types required"));
                    }
                    let arr_tid = f.reg_types[ins.b as usize];
                    let elem_tid = m
                        .types
                        .get(arr_tid as usize)
                        .map(|te| te.p0)
                        .ok_or_else(|| err(fi, pc, "array_set array type entry out of range"))?;
                    if elem_tid != f.reg_types[ins.a as usize] {
                        return Err(err(fi, pc, "array_set src type mismatch"));
                    }
                }
                x if x == Op::BytesNew as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "bytes new reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bytes || rk(m, &f.reg_types, ins.b)? != TypeKind::I32 {
                        return Err(err(fi, pc, "bytes_new types required"));
                    }
                }
                x if x == Op::BytesLen as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "bytes len reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32 || rk(m, &f.reg_types, ins.b)? != TypeKind::Bytes {
                        return Err(err(fi, pc, "bytes_len types required"));
                    }
                }
                x if x == Op::BytesGetU8 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "bytes get_u8 reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::Bytes
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::I32
                    {
                        return Err(err(fi, pc, "bytes_get_u8 types required"));
                    }
                }
                x if x == Op::BytesSetU8 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "bytes set_u8 reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::I32
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::Bytes
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::I32
                    {
                        return Err(err(fi, pc, "bytes_set_u8 types required"));
                    }
                }
                x if x == Op::BytesConcat2 as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "bytes concat2 reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bytes
                        || rk(m, &f.reg_types, ins.b)? != TypeKind::Bytes
                        || rk(m, &f.reg_types, ins.c)? != TypeKind::Bytes
                    {
                        return Err(err(fi, pc, "bytes_concat2 types required"));
                    }
                }
                x if x == Op::BytesConcatMany as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "bytes concat_many reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bytes {
                        return Err(err(fi, pc, "bytes_concat_many dst must be bytes"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Array {
                        return Err(err(fi, pc, "bytes_concat_many src must be array"));
                    }
                    let arr_tid = f.reg_types[ins.b as usize];
                    let elem_tid = unary_elem_tid(m, arr_tid, TypeKind::Array, "bytes_concat_many")?;
                    if type_kind(m, elem_tid)? != TypeKind::Bytes {
                        return Err(err(fi, pc, "bytes_concat_many requires Array<bytes>"));
                    }
                }
                x if x == Op::ObjNew as u8 => {
                    if (ins.a as u32) >= nregs {
                        return Err(err(fi, pc, "obj new reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Object {
                        return Err(err(fi, pc, "obj_new dst must be object"));
                    }
                }
                x if x == Op::ObjHasAtom as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "obj has reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.a)? != TypeKind::Bool || rk(m, &f.reg_types, ins.b)? != TypeKind::Object {
                        return Err(err(fi, pc, "obj_has types required"));
                    }
                    if (ins.imm as usize) >= m.atoms.len() {
                        return Err(err(fi, pc, "obj_has atom id out of range"));
                    }
                }
                x if x == Op::ObjGetAtom as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "obj get reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Object {
                        return Err(err(fi, pc, "obj_get src must be object"));
                    }
                    if (ins.imm as usize) >= m.atoms.len() {
                        return Err(err(fi, pc, "obj_get atom id out of range"));
                    }
                }
                x if x == Op::ObjSetAtom as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs {
                        return Err(err(fi, pc, "obj set reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Object {
                        return Err(err(fi, pc, "obj_set dst must be object"));
                    }
                    if (ins.imm as usize) >= m.atoms.len() {
                        return Err(err(fi, pc, "obj_set atom id out of range"));
                    }
                }
                x if x == Op::ObjGet as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "obj get reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Object {
                        return Err(err(fi, pc, "obj_get src must be object"));
                    }
                    if rk(m, &f.reg_types, ins.c)? != TypeKind::Atom {
                        return Err(err(fi, pc, "obj_get key must be atom"));
                    }
                }
                x if x == Op::ObjSet as u8 => {
                    if (ins.a as u32) >= nregs || (ins.b as u32) >= nregs || (ins.c as u32) >= nregs {
                        return Err(err(fi, pc, "obj set reg out of range"));
                    }
                    if rk(m, &f.reg_types, ins.b)? != TypeKind::Object {
                        return Err(err(fi, pc, "obj_set dst must be object"));
                    }
                    if rk(m, &f.reg_types, ins.c)? != TypeKind::Atom {
                        return Err(err(fi, pc, "obj_set key must be atom"));
                    }
                }
                // If an opcode isn't explicitly validated above, fail fast.
                //
                // This keeps the validator in sync with the VM's rules as opcodes are added/changed,
                // and prevents accidental "accept by default" behavior.
                _ => {
                    if op > 117 {
                        return Err(err(fi, pc, format!("unknown opcode: {}", ins.op)));
                    }
                    return Err(err(fi, pc, format!("opcode not yet validated: {}", ins.op)));
                }
            }
        }
    }
    Ok(())
}

/// Stricter validation for compiler-emitted modules.
///
/// This runs the standard bytecode validator and then checks additional invariants that are
/// important for the typed-register VM contract but are not encoded directly in the ISA:
///
/// - For direct `CALL`, the caller arg window types must exactly match the callee's param register
///   types (`callee.reg_types[0..nargs)`), for bytecode callees in this module (native builtins are
///   excluded from this check).
pub fn validate_module_strict(m: &Module) -> Result<(), String> {
    validate_module(m)?;

    // Logical indices: 0..NATIVE_BUILTIN_COUNT = native builtins, then 1..nfuncs = bytecode funcs.
    let nfuncs_logical_max = m.funcs.len() as u32;

    for (caller_fi, f) in m.funcs.iter().enumerate() {
        let nregs = f.reg_types.len();
        for (pc, ins) in f.insns.iter().enumerate() {
            if ins.op != Op::Call as u8 {
                continue;
            }
            // Skip native builtins (not represented in `m.funcs`).
            if ins.imm < NATIVE_BUILTIN_COUNT {
                continue;
            }
            if ins.imm == 0 || ins.imm > nfuncs_logical_max {
                continue; // already rejected by validate_module, but keep defensive
            }
            let callee_idx = (ins.imm - NATIVE_BUILTIN_COUNT) as usize;
            let callee = m
                .funcs
                .get(callee_idx)
                .ok_or_else(|| err(caller_fi, pc, "call callee func index out of range"))?;

            let nargs = ins.c as usize;
            if nargs > callee.reg_types.len() {
                return Err(err(
                    caller_fi,
                    pc,
                    "call nargs exceeds callee reg file (bad callee ABI)",
                ));
            }

            let first = ins.b as usize;
            if first + nargs > nregs {
                return Err(err(caller_fi, pc, "call arg range out of range"));
            }

            for i in 0..nargs {
                let caller_tid = *f
                    .reg_types
                    .get(first + i)
                    .ok_or_else(|| err(caller_fi, pc, "call arg reg out of range"))?;
                let callee_tid = callee.reg_types[i];
                if caller_tid != callee_tid {
                    return Err(err(
                        caller_fi,
                        pc,
                        &format!(
                            "call arg type mismatch at arg {i}: caller tid={caller_tid} callee tid={callee_tid}"
                        ),
                    ));
                }
            }
        }
    }

    Ok(())
}

