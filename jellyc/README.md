# `jellyc`

Jelly’s reference compiler, used to **prove** the VM end-to-end by compiling a small, statically typed, inference-friendly language to Jelly bytecode.

Updated to Rust

## Compiler architecture (high level)

`jellyc` targets a **register-based VM with statically typed registers** and a **boxed-only spill stack**. The compiler is structured to keep the IR and bytecode aligned with those VM invariants:

- **Frontend**: lex/parse to an AST, then desugar/template expansion.
- **Semantic analysis**: name resolution + capture analysis + type checking / inference.
- **Lowering**: AST/semantic info → typed, VM-oriented IR (`IrModule`).
  - **Dynamic boundaries are explicit**: boxing/unboxing (`ToDyn` / `FromDyn*`) only at true boundaries (assignments/calls/throws/returns), not throughout the program.
  - **Spill policy**: only `Dynamic` vregs may spill; typed register pressure is a compile-time error.
- **IR passes**: phi elimination + local optimizations (CSE, copy-prop, dynamic-boundary simplification, DCE).
- **Codegen**: type-aware LSRA to ≤256 regs/function, call/capture ABI pinning, bytecode emission + peephole cleanup.

Key references:

- VM ISA and bytecode rules: `docs/ISA.md`, `docs/vm_architecture.md`, `docs/performance.md`
- Call/capture ABI contract: `docs/call_abi.md`
- Compiler design rules (Cursor): `.cursor/rules/compiler/read_first.md`, `.cursor/rules/compiler/ir.md`, `.cursor/rules/compiler/pipeline.md`

## Build

```bash
cargo build --manifest-path jellyc/Cargo.toml
```

## Run (placeholder)

```bash
cargo run --manifest-path jellyc/Cargo.toml -- --help
```
