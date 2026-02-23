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

// Compilation orchestration: AST and IR backend entry points.

use std::collections::HashMap;
use std::path::Path;

use crate::error::CompileError;
use crate::ir_codegen;
use crate::jlyb::{self, Module};
use crate::link;
use crate::lower;
use crate::opt;
use crate::parse;
use crate::phi;
use crate::resolve;
use crate::semantic;
use crate::templates;
use crate::typectx::TypeRepr;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Backend {
    Ast,
    Ir,
}

/// Unified error type for compilation failures.
pub enum CompileFailure {
    Load(link::ModuleLoadError),
    Compile { err: CompileError, src: String, path: Option<String> },
    Link(String),
}

impl CompileFailure {
    pub fn render(&self) -> String {
        match self {
            CompileFailure::Load(e) => e.render(),
            CompileFailure::Compile { err, src, path } => err.render(src, path.as_deref()),
            CompileFailure::Link(msg) => format!("codegen error: {}", msg),
        }
    }
}

pub fn compile_prelude(out: &Path) -> Result<(), CompileError> {
    let m = jlyb::build_prelude_module();
    let mut f = std::fs::File::create(out).map_err(|e| {
        CompileError::new(crate::error::ErrorKind::Codegen, crate::ast::Span::point(0), format!("{}", e))
    })?;
    m.write_to(&mut f).map_err(|e| {
        CompileError::new(crate::error::ErrorKind::Codegen, crate::ast::Span::point(0), format!("{}", e))
    })?;
    Ok(())
}

pub fn compile_file_ast(input: &Path) -> Result<Module, CompileError> {
    let src = std::fs::read_to_string(input)
        .map_err(|e| CompileError::new(crate::error::ErrorKind::Codegen, crate::ast::Span::point(0), format!("{}", e)))?;
    let mut prog = parse::parse_program(&src)?;
    templates::expand_templates(&mut prog)?;
    resolve::resolve_program(&prog)?;
    let (hir, _info) = semantic::analyze_program(&prog)?;
    jlyb::build_program_module(&hir.program)
}

pub fn compile_file_ir(input: &Path) -> Result<Module, CompileFailure> {
    let path_buf = input.to_path_buf();
    let (nodes, entry_idx, _root_dir) =
        link::load_module_graph(&path_buf).map_err(CompileFailure::Load)?;

    let mut key_to_index: HashMap<String, usize> = HashMap::new();
    for (i, n) in nodes.iter().enumerate() {
        key_to_index.insert(n.key.clone(), i);
    }

    let mut artifacts: Vec<jlyb::Module> = Vec::with_capacity(nodes.len());
    let mut import_lists: Vec<Vec<usize>> = Vec::with_capacity(nodes.len());

    for (i, n) in nodes.iter().enumerate() {
        let deps: Vec<usize> = n
            .import_keys
            .iter()
            .map(|k| *key_to_index.get(k).expect("dep in graph"))
            .collect();
        import_lists.push(deps);

        match &n.file {
            link::LoadedFile::Source { path, src, prog } => {
                let mut import_exports: HashMap<String, HashMap<String, TypeRepr>> = HashMap::new();
                for k in &n.import_keys {
                    let di = *key_to_index.get(k).expect("dep index");
                    import_exports.insert(k.clone(), nodes[di].exports.clone());
                }

                let (hir, _info) = semantic::analyze_module_init(&n.key, prog, i == entry_idx, &import_exports).map_err(|err| {
                    CompileFailure::Compile {
                        err,
                        src: src.clone(),
                        path: Some(path.display().to_string()),
                    }
                })?;
                let lowered = lower::lower_module_init_to_ir(&n.key, &hir.program, i == entry_idx, &import_exports)
                    .map_err(|err| CompileFailure::Compile {
                        err,
                        src: src.clone(),
                        path: Some(path.display().to_string()),
                    })?;
                for w in &lowered.warnings {
                    eprintln!("{}", w.render(src, Some(&path.display().to_string())));
                }
                let mut irm = lowered.ir;
                phi::eliminate_phis(&mut irm).map_err(|err| CompileFailure::Compile {
                    err,
                    src: src.clone(),
                    path: Some(path.display().to_string()),
                })?;
                opt::run_passes(&mut irm).map_err(|err| CompileFailure::Compile {
                    err,
                    src: src.clone(),
                    path: Some(path.display().to_string()),
                })?;
                let mut bc = ir_codegen::emit_ir_module(&irm).map_err(|err| CompileFailure::Compile {
                    err,
                    src: src.clone(),
                    path: Some(path.display().to_string()),
                })?;
                let abi_blob = link::build_module_abi_blob(&lowered.exports, &lowered.import_keys);
                bc.const_bytes.push(abi_blob);
                artifacts.push(bc);
            }
            link::LoadedFile::Bytecode { module, .. } => {
                artifacts.push(module.clone());
            }
        }
    }

    link::link_modules_and_build_entry(&artifacts, &import_lists, entry_idx)
        .map_err(CompileFailure::Link)
}
