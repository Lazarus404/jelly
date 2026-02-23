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

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::jlyb;
use crate::parse::parse_program;
use crate::typectx::{type_repr_from_jlyb, TypeRepr};

use super::collect::{collect_exports_from_program, collect_import_keys_from_program};
use super::graph::{LoadedFile, ModuleLoadError, ModuleNode};

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
        let mut prog = parse_program(&src).map_err(|e| ModuleLoadError::Compile {
            path: path.clone(),
            src: src.clone(),
            err: e,
        })?;
        let _prepared =
            crate::frontend::prepare_program(&mut prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
        let import_keys = collect_import_keys_from_program(&prog);
        let exports =
            collect_exports_from_program(&prog).map_err(|e| ModuleLoadError::Compile {
                path: path.clone(),
                src: src.clone(),
                err: e,
            })?;
        (
            LoadedFile::Source {
                path: path.clone(),
                src,
                prog,
            },
            import_keys,
            exports,
        )
    } else {
        let data = std::fs::read(&path).map_err(|e| ModuleLoadError::Io {
            path: path.clone(),
            msg: e.to_string(),
        })?;
        let mut cur = std::io::Cursor::new(&data);
        let module = jlyb::Module::read_from(&mut cur).map_err(|e| ModuleLoadError::Bytecode {
            path: path.clone(),
            msg: e.to_string(),
        })?;
        let abi = jlyb::extract_module_abi(&module).ok_or_else(|| ModuleLoadError::Bytecode {
            path: path.clone(),
            msg: "missing module ABI".to_string(),
        })?;
        let mut exports: HashMap<String, TypeRepr> = HashMap::new();
        for (name, tid) in &abi.exports {
            let tr = type_repr_from_jlyb(&module, *tid).map_err(|e| ModuleLoadError::Bytecode {
                path: path.clone(),
                msg: e.message,
            })?;
            exports.insert(name.clone(), tr);
        }
        (
            LoadedFile::Bytecode {
                path: path.clone(),
                module,
                abi: abi.clone(),
            },
            abi.imports.clone(),
            exports,
        )
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

fn dfs(
    i: usize,
    nodes: &Vec<ModuleNode>,
    ids: &HashMap<String, usize>,
    seen: &mut Vec<bool>,
    out: &mut Vec<usize>,
) {
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

pub fn load_module_graph(
    entry_path: &PathBuf,
) -> Result<(Vec<ModuleNode>, usize, PathBuf), ModuleLoadError> {
    let root_dir = entry_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();

    let mut nodes: Vec<ModuleNode> = Vec::new();
    let mut ids: HashMap<String, usize> = HashMap::new();
    let mut visiting: HashSet<String> = HashSet::new();

    let entry_idx = load_one(
        &root_dir,
        "__entry__".to_string(),
        entry_path,
        &mut nodes,
        &mut ids,
        &mut visiting,
    )?;

    // Toposort from entry.
    let mut order: Vec<usize> = Vec::new();
    let mut seen: Vec<bool> = vec![false; nodes.len()];
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
