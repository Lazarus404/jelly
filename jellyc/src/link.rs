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

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::ast::{Program, StmtKind};
use crate::error::CompileError;
use crate::jlyb;
use crate::typectx::{T_BYTES, T_DYNAMIC, T_OBJECT};

#[derive(Clone)]
pub enum LoadedFile {
    Source { path: PathBuf, src: String, prog: Program },
    Bytecode { path: PathBuf, module: jlyb::Module, abi: jlyb::ModuleAbi },
}

#[derive(Clone)]
pub struct ModuleNode {
    pub key: String, // dotted module key, or "__entry__"
    pub file: LoadedFile,
    pub import_keys: Vec<String>, // module keys, in init param order
    pub exports: HashMap<String, crate::typectx::TypeRepr>,
}

pub enum ModuleLoadError {
    Io { path: PathBuf, msg: String },
    Compile { path: PathBuf, src: String, err: CompileError },
    Bytecode { path: PathBuf, msg: String },
    NotFound { key: String, tried: Vec<PathBuf> },
    Cycle { key: String },
}

impl ModuleLoadError {
    pub fn render(&self) -> String {
        match self {
            ModuleLoadError::Io { path, msg } => format!("error: failed to read {}: {}", path.display(), msg),
            ModuleLoadError::Compile { path, src, err } => err.render(src, Some(&path.display().to_string())),
            ModuleLoadError::Bytecode { path, msg } => format!("error: failed to load {}: {}", path.display(), msg),
            ModuleLoadError::NotFound { key, tried } => {
                let mut s = format!("name error: module not found '{}'\ntried:\n", key);
                for p in tried {
                    s.push_str(&format!("  {}\n", p.display()));
                }
                s
            }
            ModuleLoadError::Cycle { key } => format!("name error: import cycle involving '{}'", key),
        }
    }
}

pub fn collect_import_keys_from_program(p: &Program) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for s in &p.stmts {
        let key = match &s.node {
            StmtKind::ImportModule { path, .. } => Some(path.join(".")),
            StmtKind::ImportFrom { from, .. } => Some(from.join(".")),
            _ => None,
        };
        if let Some(k) = key {
            if seen.insert(k.clone()) {
                out.push(k);
            }
        }
    }
    out
}

pub fn collect_exports_from_program(p: &Program) -> Result<HashMap<String, crate::typectx::TypeRepr>, CompileError> {
    let mut out: HashMap<String, crate::typectx::TypeRepr> = HashMap::new();
    for s in &p.stmts {
        if let StmtKind::Let {
            exported: true,
            name,
            ty: Some(ty),
            ..
        } = &s.node
        {
            out.insert(name.clone(), crate::typectx::type_repr_from_ty(ty)?);
        }
    }
    Ok(out)
}

pub fn load_module_graph(entry_path: &PathBuf) -> Result<(Vec<ModuleNode>, usize, PathBuf), ModuleLoadError> {
    let root_dir = entry_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();

    let mut nodes: Vec<ModuleNode> = Vec::new();
    let mut ids: HashMap<String, usize> = HashMap::new();
    let mut visiting: HashSet<String> = HashSet::new();

    fn resolve_key_to_file(root: &PathBuf, key: &str) -> Result<(PathBuf, bool), ModuleLoadError> {
        let parts: Vec<&str> = key.split('.').collect();
        let mut rel = PathBuf::new();
        for p in parts {
            rel.push(p);
        }
        let base = root.join(rel);
        let jelly = base.with_extension("jelly");
        let jlyb = base.with_extension("jlyb");
        if std::fs::metadata(&jelly).is_ok() {
            return Ok((jelly, true));
        }
        if std::fs::metadata(&jlyb).is_ok() {
            return Ok((jlyb, false));
        }
        Err(ModuleLoadError::NotFound {
            key: key.to_string(),
            tried: vec![jelly, jlyb],
        })
    }

    fn load_one(
        root: &PathBuf,
        key: String,
        entry_path: &PathBuf,
        nodes: &mut Vec<ModuleNode>,
        ids: &mut HashMap<String, usize>,
        visiting: &mut HashSet<String>,
    ) -> Result<usize, ModuleLoadError> {
        if visiting.contains(&key) {
            return Err(ModuleLoadError::Cycle { key });
        }
        if let Some(&idx) = ids.get(&key) {
            return Ok(idx);
        }
        visiting.insert(key.clone());

        let (path, is_source) = if key == "__entry__" {
            (entry_path.clone(), true)
        } else {
            resolve_key_to_file(root, &key)?
        };

        let (file, import_keys, exports) = if is_source {
            let src = std::fs::read_to_string(&path).map_err(|e| ModuleLoadError::Io {
                path: path.clone(),
                msg: e.to_string(),
            })?;
            let mut prog = crate::parse::parse_program(&src)
                .map_err(|e| ModuleLoadError::Compile { path: path.clone(), src: src.clone(), err: e })?;
            crate::templates::expand_templates(&mut prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
            crate::resolve::resolve_program(&prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
            let import_keys = collect_import_keys_from_program(&prog);
            let exports = collect_exports_from_program(&prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
            (LoadedFile::Source { path: path.clone(), src, prog }, import_keys, exports)
        } else {
            let data = std::fs::read(&path).map_err(|e| ModuleLoadError::Io {
                path: path.clone(),
                msg: e.to_string(),
            })?;
            let mut cur = std::io::Cursor::new(&data);
            let module = jlyb::Module::read_from(&mut cur)
                .map_err(|e| ModuleLoadError::Bytecode { path: path.clone(), msg: e.to_string() })?;
            let abi = jlyb::extract_module_abi(&module)
                .ok_or_else(|| ModuleLoadError::Bytecode { path: path.clone(), msg: "missing module ABI".to_string() })?;
            let mut exports: HashMap<String, crate::typectx::TypeRepr> = HashMap::new();
            for (name, tid) in &abi.exports {
                let tr = crate::typectx::type_repr_from_jlyb(&module, *tid).map_err(|e| ModuleLoadError::Bytecode {
                    path: path.clone(),
                    msg: e.message,
                })?;
                exports.insert(name.clone(), tr);
            }
            (LoadedFile::Bytecode { path: path.clone(), module, abi: abi.clone() }, abi.imports.clone(), exports)
        };

        let idx = nodes.len();
        nodes.push(ModuleNode {
            key: key.clone(),
            file,
            import_keys: import_keys.clone(),
            exports,
        });
        ids.insert(key.clone(), idx);

        // Load dependencies.
        for dep in import_keys {
            let _ = load_one(root, dep, entry_path, nodes, ids, visiting)?;
        }

        visiting.remove(&key);
        Ok(idx)
    }

    let entry_idx = load_one(&root_dir, "__entry__".to_string(), entry_path, &mut nodes, &mut ids, &mut visiting)?;

    // Toposort from entry.
    let mut order: Vec<usize> = Vec::new();
    let mut seen: Vec<bool> = vec![false; nodes.len()];
    fn dfs(i: usize, nodes: &Vec<ModuleNode>, ids: &HashMap<String, usize>, seen: &mut Vec<bool>, out: &mut Vec<usize>) {
        if seen[i] {
            return;
        }
        seen[i] = true;
        for k in &nodes[i].import_keys {
            if let Some(&j) = ids.get(k) {
                dfs(j, nodes, ids, seen, out);
            }
        }
        out.push(i);
    }
    dfs(entry_idx, &nodes, &ids, &mut seen, &mut order);

    // Rebuild nodes in topo order and remap indices.
    let mut remap: HashMap<usize, usize> = HashMap::new();
    for (new_i, &old_i) in order.iter().enumerate() {
        remap.insert(old_i, new_i);
    }
    let mut topo: Vec<ModuleNode> = Vec::with_capacity(order.len());
    for &old_i in &order {
        topo.push(nodes[old_i].clone());
    }
    let entry_new = *remap.get(&entry_idx).expect("entry remap");
    Ok((topo, entry_new, root_dir))
}

pub fn build_module_abi_blob(exports: &HashMap<String, u32>, import_keys: &[String]) -> Vec<u8> {
    fn wr_u32le(out: &mut Vec<u8>, v: u32) {
        out.extend_from_slice(&v.to_le_bytes());
    }

    let mut out: Vec<u8> = Vec::new();
    out.extend_from_slice(b"JLYMODABI1\0");

    let mut names: Vec<(&String, &u32)> = exports.iter().collect();
    names.sort_by(|a, b| a.0.cmp(b.0));
    wr_u32le(&mut out, names.len() as u32);
    for (name, tid) in names {
        wr_u32le(&mut out, name.len() as u32);
        out.extend_from_slice(name.as_bytes());
        wr_u32le(&mut out, *tid);
    }

    wr_u32le(&mut out, import_keys.len() as u32);
    for k in import_keys {
        wr_u32le(&mut out, k.len() as u32);
        out.extend_from_slice(k.as_bytes());
    }

    out
}

pub fn link_modules_and_build_entry(
    mods: &[jlyb::Module],
    import_lists: &[Vec<usize>],
    entry_idx: usize,
) -> Result<jlyb::Module, String> {
    const BASE_TYPES: usize = 16;
    const TUPLE_TAG: u32 = 0x8000_0000;

    if mods.len() != import_lists.len() {
        return Err("internal: mods/import_lists length mismatch".to_string());
    }

    let base_types = crate::typectx::TypeCtx::new_program_base().types;
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
            if fid == 0 {
                0
            } else if !m.used_prelude.is_empty() {
                // Variable prelude: compact 1..=prelude_count -> full prelude; prelude_count+1+ -> user
                if (fid as usize) <= m.used_prelude.len() {
                    m.used_prelude[(fid as usize) - 1]
                } else {
                    func_base + 1 + (fid - m.prelude_count - 1)
                }
            } else {
                // Full prelude: 1..4 unchanged; 5+ -> func_base + 1 + (fid - 5)
                if fid <= jlyb::PRELUDE_FUN_COUNT {
                    fid
                } else {
                    func_base + 1 + (fid - jlyb::PRELUDE_FUN_COUNT - 1)
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
            out.funcs.push(jlyb::Function { reg_types, cap_start: f.cap_start, insns });
        }

        // Init is the first user func; VM logical index = func_base + 1 (0 is native).
        init_func_indices[mi] = func_base + 1;
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
        insns.push(jlyb::Insn { op: jlyb::Op::ObjNew as u8, a: i, b: 0, c: 0, imm: 0 });
    }

    let arg_base = nmods;
    for (i, deps) in import_lists.iter().enumerate() {
        // arg0 = exports[i]
        insns.push(jlyb::Insn { op: jlyb::Op::Mov as u8, a: arg_base, b: i as u8, c: 0, imm: 0 });
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
        let dst = if i == entry_idx { r_bytes } else { r_dyn };
        insns.push(jlyb::Insn {
            op: jlyb::Op::Call as u8,
            a: dst,
            b: arg_base,
            c: nargs,
            imm: init_func_indices[i],
        });
    }

    insns.push(jlyb::Insn { op: jlyb::Op::Ret as u8, a: r_bytes, b: 0, c: 0, imm: 0 });

    let wrapper_index = out.funcs.len() as u32;
    out.funcs.push(jlyb::Function { reg_types, cap_start: 0, insns });
    out.entry = wrapper_index;
    Ok(out)
}
