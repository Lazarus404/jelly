# REPL Incremental Execution Plan

## Problem

The current REPL is inefficient:

1. **Full recompilation** – Each line recompiles the entire accumulated source (parse → semantic → lower → opt → phi → codegen).
2. **Subprocess spawn** – Each line spawns `jellyvm`, loads bytecode, runs, exits.
3. **No state reuse** – The VM starts fresh every time; all prior work is re-executed.

## Goal

- **Embed the VM** – jellyc links against the VM static lib and runs it in-process.
- **Incremental execution** – The VM maintains environment state; only new code is compiled and executed.
- **Bit-by-bit bytecode** – VM can load and run additional bytecode chunks against an existing environment.

---

## Architecture Overview

### Current (Inefficient)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│  jellyc REPL loop                                                                 │
│    ┌──────────────┐     ┌──────────────┐     ┌──────────────┐                    │
│    │  Read line   │ ──► │  Compile     │ ──► │  Spawn       │                    │
│    │  "i"         │     │  full source │     │  jellyvm     │                    │
│    └──────────────┘     │  (all lines) │     │  subprocess  │                    │
│                         └──────────────┘     └──────┬───────┘                    │
│                                │                    │                            │
│                                │                    ▼                            │
│                                │             ┌──────────────┐                    │
│                                │             │  VM: load    │                    │
│                                │             │  run once    │                    │
│                                │             │  exit        │                    │
│                                │             └──────────────┘                    │
└─────────────────────────────────────────────────────────────────────────────────┘
```

### Target (Incremental)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│  jellyc (single process, links libjellyvm)                                        │
│                                                                                   │
│    ┌──────────────┐     ┌──────────────┐     ┌──────────────────────────────────┐│
│    │  Read line   │ ──► │  Compile     │ ──► │  VM (persistent)                  ││
│    │  "i"         │     │  only new   │     │    - Globals heap already has i   ││
│    └──────────────┘     │  input       │     │    - Load new bytecode chunk     ││
│                         └──────────────┘     │    - Execute new chunk only      ││
│                                │             │    - Return result                ││
│                                │             └──────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## Phases

### Phase 1: VM Static Library + In-Process Execution ✅

**Goal**: Replace subprocess spawn with in-process VM calls.

**Implemented** (2025-03): jellyc links `libjellyvm` via `embed-vm` feature (default). REPL runs VM in-process; no subprocess. Use `--no-default-features` to fall back to subprocess (requires jellyvm binary).

| Step | Task                            | Notes                                                                                 |
| ---- | ------------------------------- | ------------------------------------------------------------------------------------- |
| 1.1  | Build VM as static lib          | CMake target for `libjellyvm.a`; ensure jellyc build system can link it               |
| 1.2  | Rust FFI bindings               | `jelly_vm_new`, `jelly_vm_load`, `jelly_vm_exec_status`, `jelly_value`, trap handling |
| 1.3  | Prelude loading                 | Compile prelude at build time or load `prelude.jlyb`; pass to VM once at REPL start   |
| 1.4  | Replace `Command::new(jellyvm)` | Call VM APIs directly from `repl.rs`                                                  |
| 1.5  | Result printing                 | Convert `jelly_value` to string in Rust (or call VM helper)                           |

**Deliverable**: REPL runs VM in-process; no subprocess. Still recompiles full source each line.

---

### Phase 2: VM State Persistence

**Goal**: VM keeps running; globals and heap persist across inputs.

| Step | Task                                         | Notes                                                                                                                               |
| ---- | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| 2.1  | VM: support "resumable" execution            | After `jelly_vm_exec_status` returns, VM retains heap, globals, module registry                                                     |
| 2.2  | VM: `jelly_vm_load_incremental` (or similar) | Load a new module into existing VM without resetting state                                                                          |
| 2.3  | Module linking for incremental load          | New module imports prior REPL "session" module; linker produces module that extends existing env                                    |
| 2.4  | REPL: create VM once at start                | Reuse same VM instance for all inputs ✅ **Done**: `ReplVm` created once, reused; heap cleared via `jelly_gc_shutdown` between runs |

**Design question**: Does the new module _replace_ the entry, or _extend_ the environment? Options:

- **A) Extend**: New module has access to prior globals; its entry runs and returns. Prior state unchanged.
- **B) Merge**: New module is linked with prior module; single combined module. More complex linking.

**Recommendation**: Start with (A) – prior module stays loaded; new module imports it and runs.

---

### Phase 3: Incremental Compilation

**Goal**: Compile only the new line, not the full accumulated source.

| Step | Task                                       | Notes                                                                                                                                 |
| ---- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------- |
| 3.1  | Parse new input as statement or expression | ✅ **Done**: `compile_repl_incremental` parses new line only                                                                          |
| 3.2  | Semantic context from prior run            | ✅ **Done**: `ReplSession` holds `bindings: HashMap<String, TypeRepr>`; `collect_repl_bindings` extracts from program + semantic info |
| 3.3  | Lower only new AST                         | ✅ **Done**: New line compiled to IR; `collect_free_vars_in_program` finds names to import                                            |
| 3.4  | Codegen + link                             | ✅ **Done**: Link `[prior_artifact, new_artifact]`; new imports `__repl_session__`                                                    |
| 3.5  | Export new bindings                        | ✅ **Done**: REPL exports all lets; session bindings merged after each run                                                            |

**Challenges**:

- **Scope**: Prior bindings must be visible. Compiler needs a "session context" (names + types) from the VM or from a side table.
- **Imports**: New module must import the prior REPL module to access `i`, `j`, etc.
- **Entry point**: New module's entry evaluates the new expression and returns.

---

### Phase 4: Bytecode Chunk Format (Optional)

**Goal**: VM can load and run a minimal "chunk" (single expression/statement) without full module linking.

| Step | Task                             | Notes                                                                   |
| ---- | -------------------------------- | ----------------------------------------------------------------------- |
| 4.1  | Define chunk format              | Minimal bytecode: no prelude, no full module header; just funcs + entry |
| 4.2  | VM: load chunk into existing env | Chunk references globals by name or slot; VM resolves at load time      |
| 4.3  | Compiler: emit chunk             | For simple expressions, emit chunk instead of full module               |

**Trade-off**: Chunk format adds complexity. Phase 2+3 may suffice with full modules that import prior session.

---

## Dependency Graph

```
Phase 1 (VM in-process)
    │
    └──► Phase 2 (VM state persistence)
              │
              └──► Phase 3 (Incremental compilation)
                        │
                        └──► Phase 4 (Chunk format) [optional]
```

---

## Open Questions (Resolved)

1. **Session context storage**: ✅ Compiler maintains `ReplSession` with `bindings: HashMap<String, TypeRepr>`.
2. **Module identity**: ✅ For incremental compile, we use `prior_artifact` (linked output) and link `[prior, new]`.
3. **Error recovery**: ✅ If incremental fails, we fall back to full compile; prior state stays intact.
4. **Preload (`--repl file.jelly`)**: Preload runs once; session is seeded with its artifact and bindings; REPL extends that env.
5. **Exit**: ✅ Handled: REPL checks for `exit()` / `exit();` before running; in-process avoids subprocess kill.

## Known Issues

- **Export all lets**: ✅ Lowering emits `ObjSetAtom` for all REPL lets (`export_all_lets`). `let i = 2` then `i` returns `2`.

---

## Success Criteria

- [x] REPL runs VM in-process (no subprocess)
- [ ] VM state persists across lines (globals, heap) — Phase 2.1–2.3 deferred
- [x] Only new input is compiled (no full recompilation) — Phase 3 incremental compile
- [x] `let i = 1 + 1` then `i` returns `2` — VM func-index validation fixed; export_all_lets stores prior bindings
- [x] Startup time and per-line latency measurable — `bench/repl_bench.py` (startup ~8–20ms, single line ~20ms)
