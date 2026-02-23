use crate::jlyb::{Module, NATIVE_BUILTIN_COUNT};

use super::ctx::ValidateCtx;
use super::ops;
use super::switch_kind::validate_switch_kind_tables;

pub fn validate_module(m: &Module) -> Result<(), String> {
    // Logical index: 0..NATIVE=native, NATIVE..NATIVE+nfuncs=bytecode.
    let nfuncs_logical_max =
        NATIVE_BUILTIN_COUNT + (m.funcs.len().saturating_sub(1) as u32);

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
            let ctx = ValidateCtx {
                m,
                func_i: fi,
                pc,
                reg_types: &f.reg_types,
                nregs,
                nfuncs_logical_max,
            };
            ops::validate_insn(&ctx, ins)?;
        }
    }
    Ok(())
}
