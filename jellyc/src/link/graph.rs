use std::collections::HashMap;
use std::path::PathBuf;

use crate::ast::Program;
use crate::error::CompileError;
use crate::jlyb;
use crate::typectx::TypeRepr;

#[derive(Clone)]
pub enum LoadedFile {
    Source {
        path: PathBuf,
        src: String,
        prog: Program,
    },
    Bytecode {
        #[allow(dead_code)]
        path: PathBuf,
        module: jlyb::Module,
        #[allow(dead_code)]
        abi: jlyb::ModuleAbi,
    },
}

#[derive(Clone)]
pub struct ModuleNode {
    pub key: String, // dotted module key, or "__entry__"
    pub file: LoadedFile,
    pub import_keys: Vec<String>, // module keys, in init param order
    pub exports: HashMap<String, TypeRepr>,
}

#[derive(Debug)]
pub enum ModuleLoadError {
    Io {
        path: PathBuf,
        msg: String,
    },
    Compile {
        path: PathBuf,
        src: String,
        err: CompileError,
    },
    Bytecode {
        path: PathBuf,
        msg: String,
    },
    NotFound {
        key: String,
        tried: Vec<PathBuf>,
    },
    Cycle {
        key: String,
    },
}

impl ModuleLoadError {
    pub fn render(&self) -> String {
        match self {
            ModuleLoadError::Io { path, msg } => {
                format!("error: failed to read {}: {}", path.display(), msg)
            }
            ModuleLoadError::Compile { path, src, err } => {
                err.render(src, Some(&path.display().to_string()))
            }
            ModuleLoadError::Bytecode { path, msg } => {
                format!("error: failed to load {}: {}", path.display(), msg)
            }
            ModuleLoadError::NotFound { key, tried } => {
                let mut s = format!("name error: module not found '{}'\ntried:\n", key);
                for p in tried {
                    s.push_str(&format!("  {}\n", p.display()));
                }
                s
            }
            ModuleLoadError::Cycle { key } => {
                format!("name error: import cycle involving '{}'", key)
            }
        }
    }
}
