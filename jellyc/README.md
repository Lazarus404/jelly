# `jellyc`

Jelly’s reference compiler, used to **prove** the VM end-to-end by compiling a small, statically typed, inference-friendly language to Jelly bytecode.

## Build

```bash
cargo build --manifest-path jellyc/Cargo.toml
```

## Run (placeholder)

```bash
cargo run --manifest-path jellyc/Cargo.toml -- --help
```

## Rules

See:

- `.cursor/rules/compiler/read_first.md`
- `.cursor/rules/compiler/pipeline.md`
- `.cursor/rules/compiler/types.md`
- `.cursor/rules/compiler/ir.md`
