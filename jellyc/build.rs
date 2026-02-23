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

// Build script: compile the jelly VM as a static library and link it for in-process REPL.

fn main() {
    // Only build/link VM when the "embed-vm" feature is enabled (default for REPL).
    #[cfg(feature = "embed-vm")]
    {
        embed_vm_build();
    }

    // Re-run if VM sources change (when feature is enabled)
    #[cfg(feature = "embed-vm")]
    {
        let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
        let vm_dir = std::path::Path::new(&manifest).parent().unwrap().join("vm");
        println!("cargo:rerun-if-changed={}", vm_dir.join("src").display());
    }
}

#[cfg(feature = "embed-vm")]
fn embed_vm_build() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let project_root = std::path::Path::new(&manifest)
        .parent()
        .expect("jellyc must be under project root");

    // Configure and build via cmake from project root
    let dst = cmake::Config::new(project_root)
        .define("BUILD_SHARED_LIBS", "OFF")
        .define("JELLYVM_BUILD_TESTS", "OFF")
        .build_target("jellyvm")
        .build();

    // jellyvm is built into build/lib/ (CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
    let lib_dir = dst.join("build").join("lib");
    if !lib_dir.exists() {
        // Some cmake layouts put lib in build/<target>/ or build/
        let alt = dst.join("lib");
        let search = if alt.exists() { alt } else { lib_dir };
        println!("cargo:rustc-link-search=native={}", search.display());
    } else {
        println!("cargo:rustc-link-search=native={}", lib_dir.display());
    }

    println!("cargo:rustc-link-lib=static=jellyvm");

    // jellyvm may need pthread on Unix
    #[cfg(target_family = "unix")]
    {
        println!("cargo:rustc-link-lib=pthread");
    }
}
