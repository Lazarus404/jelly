use std::collections::{HashMap, HashSet};

use crate::error::{CompileError, ErrorKind};
use crate::ir::{IrFunction, IrModule, IrOp};
use crate::jlyb::{self, Function, Module};

/// Collect prelude function indices used in the IR via ConstFun, Call, or Closure.
fn collect_used_prelude_indices(ir: &IrModule) -> HashSet<u32> {
    let prelude_start = jlyb::NATIVE_BUILTIN_COUNT;
    let prelude_end = prelude_start + jlyb::PRELUDE_FUN_COUNT;
    let mut used = HashSet::new();
    for f in &ir.funcs {
        let nvregs = f.vreg_types.len();
        let mut const_fun_of_vreg: Vec<Option<u32>> = vec![None; nvregs];
        let order = super::block_order_rpo(f);
        for b in &order {
            let bi = b.0 as usize;
            let blk = &f.blocks[bi];
            for ins in &blk.insns {
                match &ins.op {
                    IrOp::ConstFun { dst, func_index } => {
                        if (prelude_start..prelude_end).contains(func_index) {
                            used.insert(*func_index);
                        }
                        let di = dst.0 as usize;
                        if di < nvregs {
                            const_fun_of_vreg[di] = Some(*func_index);
                        }
                    }
                    IrOp::Mov { dst, src } => {
                        let di = dst.0 as usize;
                        let si = src.0 as usize;
                        if di < nvregs && si < nvregs {
                            const_fun_of_vreg[di] = const_fun_of_vreg[si];
                        }
                    }
                    IrOp::Call { callee, .. } => {
                        let ci = callee.0 as usize;
                        if ci < nvregs {
                            if let Some(fi) = const_fun_of_vreg[ci] {
                                if (prelude_start..prelude_end).contains(&fi) {
                                    used.insert(fi);
                                }
                            }
                        }
                    }
                    IrOp::Closure { func_index, .. } => {
                        if (prelude_start..prelude_end).contains(func_index) {
                            used.insert(*func_index);
                        }
                    }
                    _ => {}
                }
                // Clear def before updating (so Mov/ConstFun set fresh)
                if let Some(d) = ins.op.def() {
                    let di = d.0 as usize;
                    if di < nvregs {
                        const_fun_of_vreg[di] = None;
                    }
                }
                match &ins.op {
                    IrOp::ConstFun { dst, func_index } => {
                        let di = dst.0 as usize;
                        if di < nvregs {
                            const_fun_of_vreg[di] = Some(*func_index);
                        }
                    }
                    IrOp::Mov { dst, src } => {
                        let di = dst.0 as usize;
                        let si = src.0 as usize;
                        if di < nvregs && si < nvregs {
                            const_fun_of_vreg[di] = const_fun_of_vreg[si];
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    used
}

pub fn emit_ir_module(ir: &IrModule) -> Result<Module, CompileError> {
    if ir.funcs.is_empty() {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            crate::ast::Span::point(0),
            "IR module has no functions",
        ));
    }
    if ir.entry >= ir.funcs.len() {
        return Err(CompileError::new(
            ErrorKind::Codegen,
            crate::ast::Span::point(0),
            "IR module entry function index out of range",
        ));
    }

    let used_prelude: Vec<u32> = {
        let mut v: Vec<u32> = collect_used_prelude_indices(ir).into_iter().collect();
        v.sort_unstable();
        v
    };
    // prelude_funcs_for_used expects 1..=4 (concat2, concat_many, slice, eq); we have logical
    // NATIVE..=NATIVE+3. Map by subtracting (NATIVE-1) so 3->1, 4->2, 5->3, 6->4.
    let prelude_used_1based: Vec<u32> = used_prelude
        .iter()
        .map(|u| u - (jlyb::NATIVE_BUILTIN_COUNT - 1))
        .collect();
    let prelude_funcs = jlyb::prelude_funcs_for_used(&prelude_used_1based);
    let prelude_count = prelude_funcs.len() as u32;

    // Map IR "logical function indices" (0,1=native, 2..=5=prelude, 6=main, 7+=nested)
    // to the bytecode CALL/CALLR function index space (0=native, 1..=funcs.len()).
    //
    // The previous arithmetic mapping assumed nested function indices were assigned in the same
    // order they appear in `ir.funcs`. That assumption breaks when we lower nested lambdas in a
    // different traversal order (eg. outer lambda assigned before inner lambda is emitted).
    let mut user_fun_to_bc_fi: HashMap<u32, u32> = HashMap::new();
    for (i, f) in ir.funcs.iter().enumerate() {
        // Bytecode function array index (0-based) in the final module:
        // prelude funcs first, then `ir.funcs` in order.
        let bc_array_idx: u32 = prelude_count + (i as u32);
        // Emit fid that linker expects: prelude uses NATIVE..NATIVE+used_prelude.len(),
        // user uses NATIVE+used_prelude.len()+user_idx. Avoids collision with native (0,1).
        let emitted_fid: u32 = jlyb::NATIVE_BUILTIN_COUNT
            + (used_prelude.len() as u32)
            + (bc_array_idx - prelude_count);

        let logical_idx: u32 = if i == 0 {
            jlyb::NATIVE_BUILTIN_COUNT + jlyb::PRELUDE_FUN_COUNT // main/program entry (6)
        } else {
            // Nested functions are named `lambda{logical_idx}` by lowering.
            let Some(name) = &f.name else {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    "missing nested function name for mapping",
                ));
            };
            let Some(s) = name.strip_prefix("lambda") else {
                return Err(CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("unexpected function name '{name}' for mapping"),
                ));
            };
            s.parse::<u32>().map_err(|_| {
                CompileError::new(
                    ErrorKind::Internal,
                    crate::ast::Span::point(0),
                    format!("bad lambda name '{name}' for mapping"),
                )
            })?
        };
        user_fun_to_bc_fi.insert(logical_idx, emitted_fid);
    }

    let map_func_index = |old: u32| -> u32 {
        if old < jlyb::NATIVE_BUILTIN_COUNT {
            return old;
        }
        let prelude_end = jlyb::NATIVE_BUILTIN_COUNT + jlyb::PRELUDE_FUN_COUNT;
        if (jlyb::NATIVE_BUILTIN_COUNT..prelude_end).contains(&old) {
            // Old prelude index (2..=5) -> NATIVE_BUILTIN_COUNT + position.
            // Must not collide with native builtins (0, 1).
            return used_prelude
                .iter()
                .position(|&u| u == old)
                .map(|p| jlyb::NATIVE_BUILTIN_COUNT + (p as u32))
                .unwrap_or(old);
        }
        user_fun_to_bc_fi.get(&old).copied().unwrap_or(old)
    };

    fn emit_function(
        ir: &IrModule,
        f: &IrFunction,
        map_func_index: &impl Fn(u32) -> u32,
    ) -> Result<Function, CompileError> {
        // Phase 1: IR -> virtual instruction stream (vregs) + regalloc info.
        let vs = super::phase1::build_virtual_stream(ir, f, map_func_index)?;
        // Phase 2: regalloc + ABI layout.
        let alloc = super::phase2::allocate_registers(ir, f, &vs)?;
        // Phase 3: operand mapping + spill insertion + peephole.
        let insns = super::phase3::emit_final_insns(&vs, &alloc)?;
        Ok(Function {
            reg_types: alloc.reg_types,
            cap_start: alloc.cap_start,
            insns,
        })
    }

    let const_f64 = ir.const_f64.clone();

    let mut funcs: Vec<Function> = Vec::with_capacity((prelude_count as usize) + ir.funcs.len());
    funcs.extend(prelude_funcs);
    for f in &ir.funcs {
        funcs.push(emit_function(ir, f, &map_func_index)?);
    }

    Ok(Module {
        types: ir.types.clone(),
        sigs: ir.sigs.clone(),
        atoms: ir.atoms.clone(),
        const_i64: ir.const_i64.clone(),
        const_f64,
        const_bytes: ir.const_bytes.clone(),
        funcs,
        // Module entry is an index into the `funcs` array (0-based, including any prelude funcs).
        entry: prelude_count + (ir.entry as u32),
        prelude_count,
        used_prelude: used_prelude.clone(),
    })
}
