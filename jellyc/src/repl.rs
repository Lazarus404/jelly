//! REPL (Read–Eval–Print Loop) for interactive Jelly execution.
//!
//! Compiles user input to bytecode and executes via the jellyvm CLI.
//! Exits when the user's code calls `System.exit()`.

use std::collections::HashMap;
use std::io::{self, BufRead, IsTerminal, Write};
use std::path::Path;
#[cfg(not(feature = "embed-vm"))]
use std::process::Command;

use rustyline::error::ReadlineError;

use crate::compile;
use crate::jlyb::Module;
use crate::link;
use crate::typectx::TypeRepr;

/// REPL-only preamble: injects `exit` as a callable that invokes System.exit().
/// Not used when compiling .jelly files.
const REPL_EXIT_PREAMBLE: &str = "let exit = fn() { System.exit(); \"\" }; ";

const REPL_HELP: &str = r#"Up arrow – Previous command
Down arrow – Next command
Left/Right – Move cursor
Ctrl+C – Cancel current line
Ctrl+R – Reverse search history
History is stored in ~/.jelly_history and persists across sessions."#;

/// Import key for the prior REPL session module (incremental compile).
const REPL_SESSION_KEY: &str = "__repl_session__";

/// Session context for incremental REPL: accumulated source, prior bindings, and prior artifact.
#[derive(Default)]
pub struct ReplSession {
    pub accumulated: String,
    pub bindings: HashMap<String, TypeRepr>,
    /// Unlinked artifact for the accumulated module; used for incremental linking.
    pub prior_artifact: Option<Module>,
}

/// Compile REPL source to a linked module (prelude + user code).
/// When `repl_mode` is true, prepends the exit preamble so `exit()` works.
/// Returns (linked_module, artifact, bindings). The artifact is the unlinked module for incremental.
pub fn compile_repl_source(
    src: &str,
    repl_mode: bool,
) -> Result<(Module, Module, HashMap<String, TypeRepr>), compile::CompileFailure> {
    let src = if repl_mode {
        format!("{}{}", REPL_EXIT_PREAMBLE, src)
    } else {
        src.to_string()
    };
    let mut prog = crate::parse::parse_program(&src).map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: src.clone(),
        path: Some("<repl>".to_string()),
    })?;

    let prepared = crate::frontend::prepare_program(&mut prog).map_err(|e| {
        compile::CompileFailure::Compile {
            err: e,
            src: src.clone(),
            path: Some("<repl>".to_string()),
        }
    })?;

    let import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
        HashMap::new();
    let (hir, info) = crate::semantic::analyze_prepared_module_init(
        "__repl__",
        prepared,
        true,
        true, // is_repl: allow any type for final expression
        &import_exports,
    )
    .map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: src.clone(),
        path: Some("<repl>".to_string()),
    })?;

    let lowered = crate::lower::lower_module_init_to_ir(
        "__repl__",
        &hir.program,
        &info,
        true,  // is_entry
        true,  // is_repl
        &import_exports,
    )
    .map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: src.clone(),
        path: Some("<repl>".to_string()),
    })?;

    let mut irm = lowered.ir;
    crate::opt::run_passes(&mut irm).map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: src.clone(),
        path: Some("<repl>".to_string()),
    })?;
    crate::phi::eliminate_phis(&mut irm).map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: src.clone(),
        path: Some("<repl>".to_string()),
    })?;
    crate::opt::run_post_phi_cleanup(&mut irm);

    let mut bc = crate::codegen::emit_ir_module(&irm).map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: src.clone(),
        path: Some("<repl>".to_string()),
    })?;

    if let Err(msg) = crate::jlyb::validate_module(&bc) {
        return Err(compile::CompileFailure::Compile {
            err: crate::error::CompileError::new(
                crate::error::ErrorKind::Codegen,
                crate::ast::Span::point(0),
                format!("bytecode validation failed: {msg}"),
            ),
            src: src.clone(),
            path: Some("<repl>".to_string()),
        });
    }

    let abi_blob = crate::link::build_module_abi_blob(&lowered.exports, &lowered.import_keys);
    bc.const_bytes.push(abi_blob);

    let mut artifacts = vec![bc];
    let import_lists = vec![vec![]];
    let linked = crate::link::link_modules_and_build_entry(&artifacts, &import_lists, 0, true)
        .map_err(compile::CompileFailure::Link)?;

    if let Err(msg) = crate::jlyb::validate_module(&linked) {
        return Err(compile::CompileFailure::Link(format!(
            "linked bytecode validation failed: {msg}"
        )));
    }

    let bindings = link::collect_repl_bindings(&hir.program, &info).unwrap_or_default();
    let artifact = artifacts.pop().expect("one artifact");
    Ok((linked, artifact, bindings))
}

/// Compile only the new line, importing prior bindings from the session.
/// Returns (linked_module, new_bindings) when successful.
/// Falls back to full compile if incremental fails (e.g. parse error, missing binding).
pub fn compile_repl_incremental(
    new_line: &str,
    session: &ReplSession,
) -> Result<(Module, HashMap<String, TypeRepr>), compile::CompileFailure> {
    let prior = session
        .prior_artifact
        .as_ref()
        .ok_or_else(|| compile::CompileFailure::Compile {
            err: crate::error::CompileError::new(
                crate::error::ErrorKind::Internal,
                crate::ast::Span::point(0),
                "no prior artifact for incremental",
            ),
            src: String::new(),
            path: Some("<repl>".to_string()),
        })?;

    let prog = crate::parse::parse_program(new_line).map_err(|e| {
        compile::CompileFailure::Compile {
            err: e,
            src: new_line.to_string(),
            path: Some("<repl>".to_string()),
        }
    })?;

    let free = link::collect_free_vars_in_program(&prog);
    let to_import: Vec<String> = free
        .into_iter()
        .filter(|n| session.bindings.contains_key(n))
        .collect();

    let import_stmt = if to_import.is_empty() {
        String::new()
    } else {
        let items: String = to_import
            .iter()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        format!("import {{ {} }} from {}; ", items, REPL_SESSION_KEY)
    };

    let full_src = format!(
        "{}{}{}",
        REPL_EXIT_PREAMBLE,
        import_stmt,
        new_line
    );

    let mut prog = crate::parse::parse_program(&full_src).map_err(|e| {
        compile::CompileFailure::Compile {
            err: e,
            src: full_src.clone(),
            path: Some("<repl>".to_string()),
        }
    })?;

    let prepared = crate::frontend::prepare_program(&mut prog).map_err(|e| {
        compile::CompileFailure::Compile {
            err: e,
            src: full_src.clone(),
            path: Some("<repl>".to_string()),
        }
    })?;

    let mut import_exports: HashMap<String, HashMap<String, crate::typectx::TypeRepr>> =
        HashMap::new();
    if !to_import.is_empty() {
        let mut ex = HashMap::new();
        for n in &to_import {
            if let Some(tr) = session.bindings.get(n) {
                ex.insert(n.clone(), tr.clone());
            }
        }
        import_exports.insert(REPL_SESSION_KEY.to_string(), ex);
    }

    let (hir, info) = crate::semantic::analyze_prepared_module_init(
        "__repl_new__",
        prepared,
        true,
        true,
        &import_exports,
    )
    .map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: full_src.clone(),
        path: Some("<repl>".to_string()),
    })?;

    let lowered = crate::lower::lower_module_init_to_ir(
        "__repl_new__",
        &hir.program,
        &info,
        true,
        true,
        &import_exports,
    )
    .map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: full_src.clone(),
        path: Some("<repl>".to_string()),
    })?;

    let mut irm = lowered.ir;
    crate::opt::run_passes(&mut irm).map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: full_src.clone(),
        path: Some("<repl>".to_string()),
    })?;
    crate::phi::eliminate_phis(&mut irm).map_err(|e| compile::CompileFailure::Compile {
        err: e,
        src: full_src.clone(),
        path: Some("<repl>".to_string()),
    })?;
    crate::opt::run_post_phi_cleanup(&mut irm);

    let mut new_bc = crate::codegen::emit_ir_module(&irm).map_err(|e| {
        compile::CompileFailure::Compile {
            err: e,
            src: full_src.clone(),
            path: Some("<repl>".to_string()),
        }
    })?;

    if let Err(msg) = crate::jlyb::validate_module(&new_bc) {
        return Err(compile::CompileFailure::Compile {
            err: crate::error::CompileError::new(
                crate::error::ErrorKind::Codegen,
                crate::ast::Span::point(0),
                format!("bytecode validation failed: {msg}"),
            ),
            src: full_src.clone(),
            path: Some("<repl>".to_string()),
        });
    }

    let abi_blob =
        crate::link::build_module_abi_blob(&lowered.exports, &lowered.import_keys);
    new_bc.const_bytes.push(abi_blob);

    let artifacts = vec![prior.clone(), new_bc];
    let import_lists = if to_import.is_empty() {
        vec![vec![], vec![]]
    } else {
        vec![vec![], vec![0]]
    };
    let linked = crate::link::link_modules_and_build_entry(&artifacts, &import_lists, 1, true)
        .map_err(compile::CompileFailure::Link)?;

    if let Err(msg) = crate::jlyb::validate_module(&linked) {
        return Err(compile::CompileFailure::Link(format!(
            "linked bytecode validation failed: {msg}"
        )));
    }

    let new_bindings = link::collect_repl_bindings(&hir.program, &info).unwrap_or_default();
    let mut bindings = session.bindings.clone();
    for (k, v) in new_bindings {
        bindings.insert(k, v);
    }
    Ok((linked, bindings))
}

/// Resolve the jellyvm binary path (used when embed-vm is disabled).
#[cfg(not(feature = "embed-vm"))]
fn find_jellyvm() -> Option<std::path::PathBuf> {
    if let Ok(p) = std::env::var("JELLYVM") {
        let pb = std::path::PathBuf::from(&p);
        if pb.is_file() {
            return Some(pb);
        }
    }

    // Build candidate list: exe-relative paths and cwd-relative (when run from project root)
    let exe = std::env::current_exe().ok()?;
    let jellyc_dir = exe.parent()?;
    let cwd = std::env::current_dir().ok();

    let mut candidates: Vec<std::path::PathBuf> = vec![
        jellyc_dir.join("jellyvm"),
        jellyc_dir.join("jellyvm_cli"),
        jellyc_dir.join("../../build/bin/jellyvm"),
        jellyc_dir.join("../../../build/bin/jellyvm"),
        jellyc_dir.join("../../build/bin/jellyvm_cli"),
        jellyc_dir.join("../../../build/bin/jellyvm_cli"),
        jellyc_dir.join("../../vm/build/bin/jellyvm"),
        jellyc_dir.join("../../../vm/build/bin/jellyvm"),
    ];
    if let Some(ref c) = cwd {
        candidates.push(c.join("build/bin/jellyvm"));
        candidates.push(c.join("build/bin/jellyvm_cli"));
    }

    for p in &candidates {
        if p.is_file() {
            return Some(p.canonicalize().unwrap_or_else(|_| p.clone()));
        }
    }

    // Try "jellyvm" from PATH (running with no args exits 2 but proves it exists)
    if Command::new("jellyvm").output().is_ok() {
        return Some(std::path::PathBuf::from("jellyvm"));
    }
    None
}

/// Run the REPL. Returns when the user's code calls `System.exit()`.
/// CTRL+C cancels the current input and shows a new prompt; it does not exit.
pub fn run(preload_module: Option<&Path>) -> Result<(), String> {
    #[cfg(not(feature = "embed-vm"))]
    let jellyvm = find_jellyvm().ok_or_else(|| {
        "jellyvm not found. Build the project (cmake -S . -B build && cmake --build build) or set JELLYVM env var.".to_string()
    })?;

    #[cfg(feature = "embed-vm")]
    let mut repl_vm = match crate::vm::ReplVm::new() {
        Ok(v) => v,
        Err(e) => return Err(e),
    };

    let mut session = ReplSession::default();
    let mut preload_printed = false;

    // Preload: compile and run the file once to seed the environment.
    if let Some(path) = preload_module {
        let src = std::fs::read_to_string(path).map_err(|e| {
            format!("failed to read {}: {}", path.display(), e)
        })?;
        match compile_repl_source(&src, false) {
            Ok((m, artifact, bindings)) => {
                session.accumulated = src;
                session.bindings = bindings;
                session.prior_artifact = Some(artifact);
                let mut bytecode = Vec::new();
                if m.write_to(&mut bytecode).is_ok() {
                    #[cfg(feature = "embed-vm")]
                    match repl_vm.exec(&bytecode) {
                        Ok(out) => {
                            if !out.trim().is_empty() {
                                print!("{}", out.trim_end_matches('\n'));
                                preload_printed = true;
                            }
                        }
                        Err(e) => eprintln!("{}", e),
                    }
                    #[cfg(not(feature = "embed-vm"))]
                    {
                        let mut tmp = std::env::temp_dir();
                        tmp.push("jelly_repl_preload.jlyb");
                        std::fs::write(&tmp, &bytecode).map_err(|e| e.to_string())?;
                        let out = Command::new(&jellyvm).arg(&tmp).output();
                        if let Ok(o) = out {
                            if !o.status.success() {
                                eprintln!("{}", String::from_utf8_lossy(&o.stderr));
                            } else {
                                let s = String::from_utf8_lossy(&o.stdout);
                                if !s.trim().is_empty() {
                                    print!("{}", s.trim_end_matches('\n'));
                                    preload_printed = true;
                                }
                            }
                        }
                        let _ = std::fs::remove_file(&tmp);
                    }
                }
            }
            Err(f) => eprintln!("preload compile error: {}", f.render()),
        }
    }

    if preload_printed {
        println!();
    }
    if let Err(e) = ctrlc::set_handler(|| {
        // CTRL+C: do nothing, let the read return. We'll catch SIGINT.
        // The actual behavior: on Unix, a blocked read returns EINTR.
        // We want to clear the line and show a new prompt. The ctrlc handler
        // runs in a signal context; we can't do much. We'll rely on the
        // read returning and the loop continuing.
    }) {
        eprintln!("warning: could not set CTRL+C handler: {}", e);
    }

    println!("Jelly REPL by Jahred Love\n");
    println!(
        "Interactive Jelly ({}) - type `exit()` to exit.",
        env!("CARGO_PKG_VERSION")
    );

    let result = if io::stdin().is_terminal() {
        #[cfg(feature = "embed-vm")]
        {
            run_repl_with_history(&mut repl_vm, &mut session)
        }
        #[cfg(not(feature = "embed-vm"))]
        {
            run_repl_with_history(&jellyvm, &mut session)
        }
    } else {
        #[cfg(feature = "embed-vm")]
        {
            run_repl_piped(&mut repl_vm, &mut session)
        }
        #[cfg(not(feature = "embed-vm"))]
        {
            run_repl_piped(&jellyvm, &mut session)
        }
    };
    result
}

/// History file path (~/.jelly_history or .jelly_history in cwd).
fn history_path() -> std::path::PathBuf {
    std::env::var("HOME")
        .or_else(|_| std::env::var("USERPROFILE"))
        .map(|h| std::path::PathBuf::from(h).join(".jelly_history"))
        .unwrap_or_else(|_| std::path::PathBuf::from(".jelly_history"))
}

#[cfg(feature = "embed-vm")]
fn run_repl_with_history(
    repl_vm: &mut crate::vm::ReplVm,
    session: &mut ReplSession,
) -> Result<(), String> {
    let mut rl = rustyline::DefaultEditor::new().map_err(|e| e.to_string())?;
    let history_path = history_path();
    let _ = rl.load_history(&history_path);

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    let _ = e;
                }
                match process_line(trimmed, session, repl_vm) {
                    Ok(true) => break,
                    Ok(false) => {}
                    Err(e) => return Err(e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!();
                continue;
            }
            Err(ReadlineError::Eof) => break,
            Err(e) => return Err(e.to_string()),
        }
    }

    let _ = rl.save_history(&history_path);
    Ok(())
}

#[cfg(not(feature = "embed-vm"))]
fn run_repl_with_history(
    jellyvm: &std::path::PathBuf,
    session: &mut ReplSession,
) -> Result<(), String> {
    let mut rl = rustyline::DefaultEditor::new().map_err(|e| e.to_string())?;
    let history_path = history_path();
    let _ = rl.load_history(&history_path);

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    let _ = e;
                }
                match process_line_subprocess(trimmed, session, jellyvm) {
                    Ok(true) => break,
                    Ok(false) => {}
                    Err(e) => return Err(e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!();
                continue;
            }
            Err(ReadlineError::Eof) => break,
            Err(e) => return Err(e.to_string()),
        }
    }

    let _ = rl.save_history(&history_path);
    Ok(())
}

#[cfg(feature = "embed-vm")]
fn run_repl_piped(
    repl_vm: &mut crate::vm::ReplVm,
    session: &mut ReplSession,
) -> Result<(), String> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("> ");
        stdout.flush().map_err(|e| e.to_string())?;

        let mut line = String::new();
        loop {
            let mut buf = String::new();
            match stdin.lock().read_line(&mut buf) {
                Ok(0) => return Ok(()),
                Ok(_) => {
                    line.push_str(&buf);
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    break;
                }
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                    line.clear();
                    println!();
                    print!("> ");
                    stdout.flush().map_err(|e| e.to_string())?;
                    continue;
                }
                Err(e) => return Err(e.to_string()),
            }
        }

        let trimmed = line.trim();
        match process_line(trimmed, session, repl_vm) {
            Ok(true) => return Ok(()),
            Ok(false) => {}
            Err(e) => return Err(e),
        }
    }
}

#[cfg(not(feature = "embed-vm"))]
fn run_repl_piped(
    jellyvm: &std::path::PathBuf,
    session: &mut ReplSession,
) -> Result<(), String> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("> ");
        stdout.flush().map_err(|e| e.to_string())?;

        let mut line = String::new();
        loop {
            let mut buf = String::new();
            match stdin.lock().read_line(&mut buf) {
                Ok(0) => return Ok(()),
                Ok(_) => {
                    line.push_str(&buf);
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        continue;
                    }
                    break;
                }
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {
                    line.clear();
                    println!();
                    print!("> ");
                    stdout.flush().map_err(|e| e.to_string())?;
                    continue;
                }
                Err(e) => return Err(e.to_string()),
            }
        }

        let trimmed = line.trim();
        match process_line_subprocess(trimmed, session, jellyvm) {
            Ok(true) => return Ok(()),
            Ok(false) => {}
            Err(e) => return Err(e),
        }
    }
}

/// Process one REPL line. Returns Ok(true) to exit, Ok(false) to continue, Err on fatal error.
#[cfg(feature = "embed-vm")]
fn process_line(
    trimmed: &str,
    session: &mut ReplSession,
    repl_vm: &mut crate::vm::ReplVm,
) -> Result<bool, String> {
    if trimmed == "exit()" || trimmed == "exit();" {
        return Ok(true);
    }
    if trimmed == "help()" || trimmed == "help();" {
        println!("{}", REPL_HELP);
        return Ok(false);
    }

    let full_src = if session.accumulated.is_empty() {
        trimmed.to_string()
    } else {
        format!("{}\n{}", session.accumulated, trimmed)
    };

    let compile_result = if session.prior_artifact.is_some() {
        compile_repl_incremental(trimmed, session)
            .map(|(linked, bindings)| (linked, None, bindings))
            .or_else(|_| {
                compile_repl_source(&full_src, true)
                    .map(|(linked, artifact, bindings)| (linked, Some(artifact), bindings))
            })
    } else {
        compile_repl_source(&full_src, true)
            .map(|(linked, artifact, bindings)| (linked, Some(artifact), bindings))
    };

    match compile_result {
        Ok((module, next_artifact, bindings)) => {
            let bytecode = {
                let mut buf = Vec::new();
                module.write_to(&mut buf).map_err(|e| e.to_string())?;
                buf
            };

            match repl_vm.exec(&bytecode) {
                Ok(out) => {
                    session.accumulated = full_src.clone();
                    session.bindings = bindings.clone();
                    session.prior_artifact = next_artifact;
                    let out = out.trim_end_matches('\n');
                    if !out.is_empty() {
                        println!("{}", out);
                    }
                }
                Err(e) => eprintln!("{}", e),
            }
        }
        Err(f) => eprintln!("{}", f.render()),
    }
    Ok(false)
}

/// Process one REPL line (subprocess mode). Returns Ok(true) to exit, Ok(false) to continue.
#[cfg(not(feature = "embed-vm"))]
fn process_line_subprocess(
    trimmed: &str,
    session: &mut ReplSession,
    jellyvm: &std::path::PathBuf,
) -> Result<bool, String> {
    if trimmed == "exit()" || trimmed == "exit();" {
        return Ok(true);
    }
    if trimmed == "help()" || trimmed == "help();" {
        println!("{}", REPL_HELP);
        return Ok(false);
    }

    let full_src = if session.accumulated.is_empty() {
        trimmed.to_string()
    } else {
        format!("{}\n{}", session.accumulated, trimmed)
    };

    let compile_result = if session.prior_artifact.is_some() {
        compile_repl_incremental(trimmed, session)
            .map(|(linked, bindings)| (linked, None, bindings))
            .or_else(|_| {
                compile_repl_source(&full_src, true)
                    .map(|(linked, artifact, bindings)| (linked, Some(artifact), bindings))
            })
    } else {
        compile_repl_source(&full_src, true)
            .map(|(linked, artifact, bindings)| (linked, Some(artifact), bindings))
    };

    match compile_result {
        Ok((module, next_artifact, bindings)) => {
            let bytecode = {
                let mut buf = Vec::new();
                module.write_to(&mut buf).map_err(|e| e.to_string())?;
                buf
            };

            let tmp = std::env::temp_dir().join(format!(
                "jelly_repl_{}_{:x}.jlyb",
                std::process::id(),
                std::time::SystemTime::now()
                    .elapsed()
                    .unwrap_or_default()
                    .as_nanos()
            ));
            std::fs::write(&tmp, &bytecode).map_err(|e| e.to_string())?;

            let output = Command::new(jellyvm).arg(&tmp).output();
            let _ = std::fs::remove_file(&tmp);

            match output {
                Ok(o) => {
                    if o.status.code() == Some(123) {
                        return Ok(true);
                    }
                    if o.status.success() {
                        session.accumulated = full_src.clone();
                        session.bindings = bindings.clone();
                        session.prior_artifact = next_artifact;
                        let out = String::from_utf8_lossy(&o.stdout);
                        let out = out.trim_end_matches('\n');
                        if !out.is_empty() {
                            println!("{}", out);
                        }
                    } else {
                        eprintln!("{}", String::from_utf8_lossy(&o.stderr));
                    }
                }
                Err(e) => eprintln!("error: failed to run jellyvm: {}", e),
            }
        }
        Err(f) => eprintln!("{}", f.render()),
    }
    Ok(false)
}
