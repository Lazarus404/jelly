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

use std::collections::HashMap;

use crate::jlyb;
use crate::typectx::TypeCtx;
use crate::typectx::{T_BYTES, T_DYNAMIC, T_OBJECT};

pub fn link_modules_and_build_entry(
    mods: &[jlyb::Module],
    import_lists: &[Vec<usize>],
    entry_idx: usize,
    repl_mode: bool,
) -> Result<jlyb::Module, String> {
    const BASE_TYPES: usize = 16;
    const TUPLE_TAG: u32 = 0x8000_0000;

    if mods.len() != import_lists.len() {
        return Err("internal: mods/import_lists length mismatch".to_string());
    }

    let base_types = TypeCtx::new_program_base().types;
    let mut out = jlyb::Module {
        types: base_types,
        sigs: Vec::new(),
        atoms: vec![b"__proto__".to_vec(), b"init".to_vec()],
        const_i64: Vec::new(),
        const_f64: vec![0.5],
        const_bytes: Vec::new(),
        funcs: jlyb::prelude_funcs_for_program(),
        entry: 0,
        prelude_count: jlyb::PRELUDE_FUN_COUNT,
        used_prelude: vec![],
    };

    let mut init_func_indices: Vec<u32> = vec![0; mods.len()];
    let mut atom_ids: HashMap<String, u32> =
        HashMap::from([("__proto__".to_string(), 0u32), ("init".to_string(), 1u32)]);

    for (mi, m) in mods.iter().enumerate() {
        if (m.funcs.len() as usize) < (m.prelude_count as usize) {
            return Err("module missing prelude".to_string());
        }
        if m.types.len() < BASE_TYPES {
            return Err("module missing base types".to_string());
        }
        if m.atoms.len() < 2 {
            return Err("module missing base atoms".to_string());
        }

        let type_extra_base = (out.types.len() - BASE_TYPES) as u32;
        let sig_off = out.sigs.len() as u32;
        let i64_off = out.const_i64.len() as u32;
        let f64_off = out.const_f64.len() as u32;
        let bytes_off = out.const_bytes.len() as u32;
        let func_base = out.funcs.len() as u32;

        let map_tid = |tid: u32| -> u32 {
            if (tid as usize) < BASE_TYPES {
                tid
            } else {
                (BASE_TYPES as u32) + type_extra_base + (tid - (BASE_TYPES as u32))
            }
        };
        let map_sig = |sid: u32| -> u32 { sig_off + sid };
        // Remap artifact func indices to link output. Link output: 0=native, 1..4=prelude, 5+=user.
        // VM logical index = array_index + 1 (0 is native).
        let map_fun = |fid: u32| -> u32 {
            if fid < jlyb::NATIVE_BUILTIN_COUNT {
                // Native builtins (0=math_sqrt, 1=system_exit) stay unchanged.
                fid
            } else if m.prelude_count == 0 {
                // Artifact with no embedded prelude: codegen emits fid = NATIVE_BUILTIN_COUNT + user_idx.
                // Map to VM logical: NATIVE_BUILTIN_COUNT + func_base + user_idx.
                let user_idx = fid - jlyb::NATIVE_BUILTIN_COUNT;
                jlyb::NATIVE_BUILTIN_COUNT + func_base + user_idx
            } else if !m.used_prelude.is_empty() {
                // Variable prelude: codegen emits NATIVE_BUILTIN_COUNT + position for prelude.
                // So fid 2..=NATIVE+used_prelude.len()-1 are prelude; rest are user.
                if fid >= jlyb::NATIVE_BUILTIN_COUNT
                    && (fid as usize) < jlyb::NATIVE_BUILTIN_COUNT as usize + m.used_prelude.len()
                {
                    m.used_prelude[(fid - jlyb::NATIVE_BUILTIN_COUNT) as usize]
                } else {
                    // User funcs: codegen fid = NATIVE + used_prelude.len() + user_idx.
                    // Map to VM logical: NATIVE_BUILTIN_COUNT + func_base + user_idx.
                    let user_idx = fid - jlyb::NATIVE_BUILTIN_COUNT - (m.used_prelude.len() as u32);
                    jlyb::NATIVE_BUILTIN_COUNT + func_base + user_idx
                }
            } else {
                // Full prelude: native+prelude (0..NATIVE+PRELUDE) unchanged; user -> func_base + ...
                let prelude_end = jlyb::NATIVE_BUILTIN_COUNT + jlyb::PRELUDE_FUN_COUNT;
                if fid < prelude_end {
                    fid
                } else {
                    func_base + 1 + (fid - prelude_end)
                }
            }
        };

        // atoms: dedup by string so ObjSetAtom/ObjGetAtom agree across modules.
        let mut atom_map: Vec<u32> = Vec::with_capacity(m.atoms.len());
        for a in &m.atoms {
            let s = std::str::from_utf8(a).map_err(|_| "module atom is not UTF-8".to_string())?;
            if let Some(&id) = atom_ids.get(s) {
                atom_map.push(id);
            } else {
                let id = out.atoms.len() as u32;
                out.atoms.push(a.clone());
                atom_ids.insert(s.to_string(), id);
                atom_map.push(id);
            }
        }
        let map_atom = |aid: u32| -> Result<u32, String> {
            atom_map
                .get(aid as usize)
                .copied()
                .ok_or_else(|| "bad atom id".to_string())
        };

        // signatures
        for s in &m.sigs {
            out.sigs.push(jlyb::FunSig {
                ret_type: map_tid(s.ret_type),
                args: s.args.iter().map(|&a| map_tid(a)).collect(),
            });
        }

        // types (skip base 0..BASE_TYPES)
        for t in m.types.iter().skip(BASE_TYPES) {
            let mut p0 = t.p0;
            match t.kind {
                jlyb::TypeKind::Array | jlyb::TypeKind::List => {
                    p0 = map_tid(p0);
                }
                jlyb::TypeKind::Function => {
                    p0 = map_sig(p0);
                }
                jlyb::TypeKind::Object => {
                    if (p0 & TUPLE_TAG) != 0 {
                        let sid = p0 & !TUPLE_TAG;
                        p0 = TUPLE_TAG | map_sig(sid);
                    }
                }
                _ => {}
            }
            out.types.push(jlyb::TypeEntry { kind: t.kind, p0 });
        }

        // const pools
        for &x in &m.const_i64 {
            out.const_i64.push(x);
        }
        for &x in &m.const_f64 {
            out.const_f64.push(x);
        }
        for b in &m.const_bytes {
            out.const_bytes.push(b.clone());
        }

        // funcs (skip prelude)
        for f in m.funcs.iter().skip(m.prelude_count as usize) {
            let mut reg_types: Vec<u32> = Vec::with_capacity(f.reg_types.len());
            for &rt in &f.reg_types {
                reg_types.push(map_tid(rt));
            }
            let mut insns: Vec<jlyb::Insn> = Vec::with_capacity(f.insns.len());
            for ins in &f.insns {
                let mut outi = *ins;
                let op = outi.op;
                if op == jlyb::Op::ConstI64 as u8 {
                    outi.imm = i64_off + outi.imm;
                } else if op == jlyb::Op::ConstF64 as u8 {
                    outi.imm = f64_off + outi.imm;
                } else if op == jlyb::Op::ConstBytes as u8 {
                    outi.imm = bytes_off + outi.imm;
                } else if op == jlyb::Op::ConstAtom as u8
                    || op == jlyb::Op::ObjHasAtom as u8
                    || op == jlyb::Op::ObjGetAtom as u8
                    || op == jlyb::Op::ObjSetAtom as u8
                {
                    outi.imm = map_atom(outi.imm)?;
                } else if op == jlyb::Op::ConstFun as u8
                    || op == jlyb::Op::Call as u8
                    || op == jlyb::Op::Closure as u8
                {
                    outi.imm = map_fun(outi.imm);
                }
                insns.push(outi);
            }
            out.funcs.push(jlyb::Function {
                reg_types,
                cap_start: f.cap_start,
                insns,
            });
        }

        // Init is the first user func; VM logical index = NATIVE_BUILTIN_COUNT + array_index.
        init_func_indices[mi] = jlyb::NATIVE_BUILTIN_COUNT + func_base;
    }

    // Build final entry wrapper.
    let nmods = mods.len() as u8;
    let max_imports = import_lists.iter().map(|v| v.len()).max().unwrap_or(0) as u8;
    let argwin = (1u8).saturating_add(max_imports);
    let r_dyn = nmods.saturating_add(argwin);
    let r_bytes = r_dyn.saturating_add(1);
    let nregs = (r_bytes as u32) + 1;
    if nregs > 256 {
        return Err("module linker: wrapper register allocation exceeded 256 regs".to_string());
    }

    let mut reg_types: Vec<u32> = Vec::with_capacity(nregs as usize);
    for _ in 0..nmods {
        reg_types.push(T_OBJECT); // Object
    }
    for _ in 0..argwin {
        reg_types.push(T_OBJECT); // Object
    }
    reg_types.push(T_DYNAMIC); // Dynamic
    reg_types.push(T_BYTES); // Bytes

    let mut insns: Vec<jlyb::Insn> = Vec::new();
    // exports objects
    for i in 0..nmods {
        insns.push(jlyb::Insn {
            op: jlyb::Op::ObjNew as u8,
            a: i,
            b: 0,
            c: 0,
            imm: 0,
        });
    }

    let arg_base = nmods;
    for (i, deps) in import_lists.iter().enumerate() {
        // arg0 = exports[i]
        insns.push(jlyb::Insn {
            op: jlyb::Op::Mov as u8,
            a: arg_base,
            b: i as u8,
            c: 0,
            imm: 0,
        });
        for (j, &dep) in deps.iter().enumerate() {
            insns.push(jlyb::Insn {
                op: jlyb::Op::Mov as u8,
                a: arg_base + 1 + (j as u8),
                b: dep as u8,
                c: 0,
                imm: 0,
            });
        }
        let nargs = (1 + deps.len()) as u8;
        // REPL entry may return any type (I32, Bytes, etc.); use r_dyn. Non-REPL entry returns Bytes.
        let dst = if i == entry_idx && !repl_mode { r_bytes } else { r_dyn };
        insns.push(jlyb::Insn {
            op: jlyb::Op::Call as u8,
            a: dst,
            b: arg_base,
            c: nargs,
            imm: init_func_indices[i],
        });
    }

    let ret_reg = if repl_mode { r_dyn } else { r_bytes };
    insns.push(jlyb::Insn {
        op: jlyb::Op::Ret as u8,
        a: ret_reg,
        b: 0,
        c: 0,
        imm: 0,
    });

    let wrapper_index = out.funcs.len() as u32;
    out.funcs.push(jlyb::Function {
        reg_types,
        cap_start: 0,
        insns,
    });
    out.entry = wrapper_index;
    Ok(out)
}

/// Phase 4: Link a single REPL chunk module for execution with pre-bound session_exports.
/// The chunk's entry takes 1 arg (session_exports in reg 0), creates exports in reg 1,
/// and calls init(exports, session_exports). Use jelly_vm_exec_status_chunk to run it.
pub fn link_chunk_for_repl(new_module: &jlyb::Module) -> Result<jlyb::Module, String> {
    use crate::typectx::{T_DYNAMIC, T_OBJECT};

    // Link [new] with no deps - merge logic runs. We'll replace the entry with our chunk wrapper.
    let mut linked = link_modules_and_build_entry(&[new_module.clone()], &[vec![]], 0, true)?;

    // New's init expects (exports, session). Our wrapper: reg 0 = session (arg), reg 1 = exports (ObjNew),
    // reg 2 = session copy (for contiguous Call args), reg 3 = ret.
    // Call uses contiguous regs (first_arg..first_arg+nargs-1), so we need Mov 2, 0 to get init(exports, session).
    let init_idx = jlyb::NATIVE_BUILTIN_COUNT + jlyb::PRELUDE_FUN_COUNT; // first user func in linked output

    let reg_types = vec![T_OBJECT, T_OBJECT, T_OBJECT, T_DYNAMIC]; // reg 0=session, 1=exports, 2=session_copy, 3=ret
    let insns = vec![
        jlyb::Insn {
            op: jlyb::Op::ObjNew as u8,
            a: 1,
            b: 0,
            c: 0,
            imm: 0,
        },
        jlyb::Insn {
            op: jlyb::Op::Mov as u8,
            a: 2,
            b: 0,
            c: 0,
            imm: 0,
        },
        jlyb::Insn {
            op: jlyb::Op::Call as u8,
            a: 3,
            b: 1,
            c: 2,
            imm: init_idx,
        },
        jlyb::Insn {
            op: jlyb::Op::Ret as u8,
            a: 3,
            b: 0,
            c: 0,
            imm: 0,
        },
    ];

    let chunk_wrapper = jlyb::Function {
        reg_types,
        cap_start: 0,
        insns,
    };

    // Replace the last function (the normal wrapper) with our chunk wrapper.
    let wrapper_idx = linked.funcs.len().saturating_sub(1);
    linked.funcs[wrapper_idx] = chunk_wrapper;
    linked.entry = wrapper_idx as u32;
    Ok(linked)
}
