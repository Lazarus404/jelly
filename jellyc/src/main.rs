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

use std::path::PathBuf;

mod ast;
mod compile;
mod error;
mod ir;
mod ir_codegen;
mod jlyb;
mod lex;
mod link;
mod lower;
mod opt;
mod parse;
mod peephole;
mod phi;
mod regalloc;
mod resolve;
mod source;
mod templates;
mod token;
mod typectx;

fn usage() -> ! {
    eprintln!(
        "usage:\n  jellyc prelude --out <prelude.jlyb>\n  jellyc <input.jelly> [--out <output.jlyb>] [--backend ast|ir]"
    );
    std::process::exit(2);
}

fn read_to_string(path: &PathBuf) -> String {
    std::fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("error: failed to read {}: {}", path.display(), e);
        std::process::exit(2);
    })
}

#[cfg(test)]
mod tests;

fn main() {
    let mut args = std::env::args().skip(1);
    let first = match args.next() {
        Some(a) => a,
        None => usage(),
    };

    // Compile the prelude
    if first == "prelude" {
        let mut out: Option<PathBuf> = None;
        while let Some(a) = args.next() {
            match a.as_str() {
                "--out" => {
                    let p = args.next().unwrap_or_else(|| usage());
                    out = Some(PathBuf::from(p));
                }
                "--help" | "-h" => usage(),
                _ => usage(),
            }
        }
        let out = out.unwrap_or_else(|| usage());
        if let Err(e) = compile::compile_prelude(&out) {
            eprintln!("{}", e.render("", None));
            std::process::exit(1);
        }
        return;
    }

    // Compile the input file
    let input = if first.starts_with('-') {
        usage()
    } else {
        PathBuf::from(first)
    };

    // Parse the command line arguments
    let mut out: Option<PathBuf> = None;
    let mut backend = compile::Backend::Ast;
    while let Some(a) = args.next() {
        match a.as_str() {
            "--out" => {
                let p = args.next().unwrap_or_else(|| usage());
                out = Some(PathBuf::from(p));
            }
            "--backend" => {
                let b = args.next().unwrap_or_else(|| usage());
                backend = match b.as_str() {
                    "ast" => compile::Backend::Ast,
                    "ir" => compile::Backend::Ir,
                    _ => usage(),
                };
            }
            "--help" | "-h" => usage(),
            _ => usage(),
        }
    }

    // Set the output file name
    let out = out.unwrap_or_else(|| {
        let mut p = input.clone();
        p.set_extension("jlyb");
        p
    });

    // Compile the input file
    let m = match backend {
        compile::Backend::Ast => compile::compile_file_ast(&input).unwrap_or_else(|e| {
            let src = read_to_string(&input);
            eprintln!("{}", e.render(&src, Some(&input.display().to_string())));
            std::process::exit(1);
        }),
        compile::Backend::Ir => compile::compile_file_ir(&input).unwrap_or_else(|f| {
            eprintln!("{}", f.render());
            std::process::exit(1);
        }),
    };

    // Write the output file
    let mut f = std::fs::File::create(&out).unwrap_or_else(|e| {
        eprintln!("error: failed to create {}: {}", out.display(), e);
        std::process::exit(2);
    });
    m.write_to(&mut f).unwrap_or_else(|e| {
        eprintln!("error: failed to write {}: {}", out.display(), e);
        std::process::exit(2);
    });
}

