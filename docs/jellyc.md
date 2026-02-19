## `jellyc`

`jellyc` is Jelly’s reference compiler. It compiles a small, statically typed language to Jelly VM bytecode modules (`.jlyb`), primarily to prove the VM end-to-end.

### Goals / non-goals

- **Goals**
  - Correct, simple compilation to valid Jelly bytecode.
  - A clean, explicit pipeline (parse → analyze → lower → allocate → emit).
  - Compiler is small and easy to extend.
- **Non-goals (today)**
  - Full JS/TS/Haxe compatibility.
  - A large dependency stack.

### Build

From repo root:

```bash
cargo build --manifest-path jellyc/Cargo.toml
```

### Run

Compile a source file:

```bash
cargo run --manifest-path jellyc/Cargo.toml -- path/to/input.jelly --out path/to/output.jlyb
```

If `--out` is omitted, the output path defaults to `<input>.jlyb`.

For compiler development, you can select the backend pipeline:

```bash
cargo run --manifest-path jellyc/Cargo.toml -- path/to/input.jelly --backend ast --out out.jlyb
cargo run --manifest-path jellyc/Cargo.toml -- path/to/input.jelly --backend ir  --out out.jlyb
```

Emit the prelude-only module (currently used for VM bring-up and experiments):

```bash
cargo run --manifest-path jellyc/Cargo.toml -- prelude --out prelude.jlyb
```

### Tests

`jellyc` has:

- **Rust unit tests** (compiler-internal): run with:

```bash
cargo test --manifest-path jellyc/Cargo.toml
```

- **End-to-end CTests** (compile + run via VM): from repo root:

```bash
cmake -S . -B build
cmake --build build -j
ctest --test-dir build
```

To run only compiler-related tests, use `ctest -R jellyc` (or the specific test name patterns defined in `ctest/CMakeLists.txt`).

### Project layout (compiler)

- `jellyc/src/main.rs`: hand-written lexer/parser + CLI entrypoint.
- `jellyc/src/ast.rs`: AST types for the current language subset.
- `jellyc/src/jlyb.rs`: type checking + lowering + register allocation orchestration + `.jlyb` emission.
- `jellyc/src/regalloc.rs`: linear-scan register allocation (type-aware).

### Output format

The compiler emits `.jlyb` modules containing:

- a **type table** and **signature table**
- an **atom table** (interned property keys)
- an optional **bytes constant pool**
- a **function table**, where each function includes:
  - `reg_types`: the type id for each physical register
  - `insns`: bytecode instructions

See `docs/ISA.md` for instruction semantics and `docs/codegen.md` for the compiler-side emission rules.

### Roadmap

The ordered compiler plan lives in `.cursor/rules/compiler/plan.md`. Keep changes aligned with the phase boundaries and invariants described in `.cursor/rules/compiler/*`.
