use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule};

pub(super) fn validate_call_windows(
    ir: &IrModule,
    f: &IrFunction,
    call_windows: &[(u32, u32, u8)],
) -> Result<(), CompileError> {
    for &(sig_id, arg_base_v, nargs) in call_windows {
        let sig = ir.sigs.get(sig_id as usize).ok_or_else(|| {
            CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "bad fun sig id",
            )
        })?;
        if sig.args.len() != (nargs as usize) {
            return Err(CompileError::new(
                ErrorKind::Internal,
                crate::ast::Span::point(0),
                "CALLR arg count does not match signature",
            ));
        }
        for i in 0..(nargs as u32) {
            let vi = (arg_base_v + i) as usize;
            if vi >= f.vreg_types.len() {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "CALLR arg vreg out of range",
                ));
            }
            let vt = f.vreg_types[vi];
            let st = sig.args[i as usize];
            if vt != st {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!(
                        "CALLR arg window type mismatch (vreg {}: {} != {})",
                        vi, vt, st
                    ),
                ));
            }
        }
    }
    Ok(())
}
