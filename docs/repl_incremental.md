# REPL Incremental Execution

This document describes how the Jelly REPL works and the plan for further improvements.

---

## How the REPL Works Now

### Entry Points

- **`jellyc`** (no args) or **`jellyc --repl`** — Start REPL with no preload
- **`jellyc --repl <file.jelly>`** — Start REPL with `<file.jelly>` preloaded and executed once

### Execution Modes

| Mode                   | When                       | Behavior                                                   |
| ---------------------- | -------------------------- | ---------------------------------------------------------- |
| **Embed VM** (default) | `embed-vm` feature enabled | jellyc links `libjellyvm`; runs VM in-process via `ReplVm` |
| **Subprocess**         | `--no-default-features`    | jellyc spawns `jellyvm` subprocess per line                |

### Per-Line Flow

1. **Read line** (e.g. `let i = 1` or `i`)
2. **Compile**:
   - If `prior_artifact` exists: try `compile_repl_incremental(new_line)`; on failure, fall back to `compile_repl_source(accumulated + new_line)`
   - Else: `compile_repl_source(accumulated + new_line)`
3. **Execute**: `repl_vm.exec(bytecode)` (embed) or spawn jellyvm (subprocess)
4. **Update session**: `accumulated`, `bindings`, `prior_artifact`

### Session State (`ReplSession`)

| Field             | Purpose                                                               |
| ----------------- | --------------------------------------------------------------------- |
| `accumulated`     | Full source of all prior lines (for fallback full compile)            |
| `bindings`        | `HashMap<String, TypeRepr>` — names and types of prior `let` bindings |
| `prior_artifact`  | Unlinked bytecode module from last successful run (for incremental)   |
| `session_exports` | Phase 2: exports object from last run (JellyValue; for future use)    |

### Incremental Compilation (`compile_repl_incremental`)

When `prior_artifact` exists:

1. Parse the new line as a program.
2. Collect free variables (`collect_free_vars_in_program`) — names used but not defined.
3. Filter to session bindings — only names in `session.bindings` need importing.
4. Inject `import { a, b, ... } from __repl_session__;` if any free vars are in scope.
5. Prepend exit preamble: `let exit = fn() { System.exit(); "" };`
6. Compile: parse → frontend → semantic → lower → opt → phi → codegen.
7. Link `[prior_artifact, new_module]` — new module imports `__repl_session__` (prior); linker merges into one module.
8. Merge new bindings into session.
9. Store the linked module as `prior_artifact` for the next line (sustained incremental).

### Full Compilation (`compile_repl_source`)

Used for the first line, when incremental fails, or when `prior_artifact` is absent. Runs full pipeline; returns `(linked, artifact, bindings)` — `artifact` is the unlinked module for future incremental.

### VM Behavior

- **ReplVm** (embed): Created once, reused for every line.
- **Heap**: Phase 2: heap persists across runs (no `jelly_gc_shutdown` between runs).
- **Exports capture**: `jelly_vm_exec_status_exports` captures the entry module's exports object on top-level return; stored in `session.session_exports` for future incremental exec.
- **State**: Prior bindings live in the **linked bytecode**; each run loads a fresh linked module that merges prior + new.

### Special Inputs

- **`exit()`** / **`exit();`** — Checked before compile; REPL exits.
- **`help()`** / **`help();`** — Prints REPL help.
- **Ctrl+C** — Cancels input; does not exit.

### Preload

With `--repl file.jelly`, the file is compiled and run once before the prompt. Its bindings and artifact seed the session.

---

## Architecture Diagrams

### Current (Embed VM + Incremental)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│  jellyc (single process, links libjellyvm)                                      │
│                                                                                 │
│   ┌──────────────┐     ┌──────────────────────┐     ┌──────────────────────────┐│
│   │  Read line   │ ──► │  Compile             │ ──► │  ReplVm.exec(bytecode)   ││
│   │  "i"         │     │  incremental or full │     │  (in-process)            ││
│   └──────────────┘     │  link [prior, new]   │     │  heap persists (Phase 2) ││
│                        └──────────────────────┘     └──────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────────┘
```

### Legacy (Subprocess)

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│  jellyc REPL loop (--no-default-features)                                       │
│    ┌──────────────┐     ┌──────────────┐     ┌─────────────┐                    │
│    │  Read line   │ ──► │ Compile      │ ──► │  Spawn      │                    │
│    │              │     │ (incremental │     │  jellyvm    │                    │
│    └──────────────┘     │  or full)    │     │  subprocess │                    │
│                         └──────────────┘     └──────┬──────┘                    │
│                                                     │  VM: load, run, exit      │
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

**Deliverable**: REPL runs VM in-process; no subprocess. Incremental compile (Phase 3) compiles only new lines when possible.

---

### Phase 2: VM State Persistence ✅

**Goal**: VM keeps running; globals and heap persist across inputs.

| Step | Task                               | Notes                                                                                            |
| ---- | ---------------------------------- | ------------------------------------------------------------------------------------------------ |
| 2.1  | VM: support "resumable" execution  | ✅ **Done**: Heap persists; `jelly_gc_shutdown` no longer called between runs                    |
| 2.2  | VM: `jelly_vm_exec_status_exports` | ✅ **Done**: Captures entry module's exports on top-level return; `entry_module_idx` 0 or 1      |
| 2.3  | REPL: capture and store exports    | ✅ **Done**: `session.session_exports` stores exports; `entry_module_idx` 0 for full, 1 for incr |
| 2.4  | REPL: create VM once at start      | ✅ **Done**: `ReplVm` created once, reused; heap persists across runs                            |

**Current state**: Heap persists. Exports are captured and stored in `session.session_exports` for future use (e.g. Phase 3 incremental exec). Prior bindings still live in the **linked bytecode**; each run loads a fresh linked module that merges prior + new.

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

### Phase 4: Bytecode Chunk Format ✅

**Goal**: VM can load and run a minimal "chunk" (single expression/statement) without full module linking.

| Step | Task                             | Notes                                                                         |
| ---- | -------------------------------- | ----------------------------------------------------------------------------- |
| 4.1  | VM: exec with pre-bound args     | ✅ **Done**: `jelly_vm_exec_status_chunk`, `vm_push_frame_from_values`        |
| 4.2  | VM: load chunk into existing env | ✅ **Done**: Chunk entry takes (session_exports); runs init(exports, session) |
| 4.3  | Compiler: emit chunk             | ✅ **Done**: `link_chunk_for_repl`; REPL uses chunk when session_exports set  |

**Implementation**: Chunk uses the same JLYB format. `link_chunk_for_repl` produces a module whose entry takes 1 arg (session_exports), creates exports, and calls init(exports, session_exports). When `session_exports` is set and incremental succeeds, REPL uses `exec_chunk` instead of full linked exec.

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
2. **Module identity**: ✅ For incremental compile, we use `prior_artifact` (unlinked prior module) and link `[prior, new]`.
3. **Error recovery**: ✅ If incremental fails, we fall back to full compile; prior state stays intact.
4. **Preload (`--repl file.jelly`)**: Preload runs once; session is seeded with its artifact and bindings; REPL extends that env.
5. **Exit**: ✅ Handled: REPL checks for `exit()` / `exit();` before running; in-process avoids subprocess kill.

## Key Files

| Purpose                      | Path                                                                                   |
| ---------------------------- | -------------------------------------------------------------------------------------- |
| REPL entry, session, compile | `jellyc/src/repl.rs`                                                                   |
| In-process VM (embed-vm)     | `jellyc/src/vm.rs`                                                                     |
| REPL binding collection      | `jellyc/src/link/collect.rs` (`collect_repl_bindings`, `collect_free_vars_in_program`) |

## Known Issues

- **Export all lets**: ✅ Lowering emits `ObjSetAtom` for all REPL lets (`export_all_lets`). `let i = 2` then `i` returns `2`.

---

## Success Criteria

- [x] REPL runs VM in-process (no subprocess)
- [x] VM state persists across lines (globals, heap) — Phase 2: heap persists; exports captured
- [x] Only new input is compiled (no full recompilation) — Phase 3 incremental compile
- [x] `let i = 1 + 1` then `i` returns `2` — VM func-index validation fixed; export_all_lets stores prior bindings
- [x] Startup time and per-line latency measurable — `bench/repl_bench.py` (startup ~8–20ms, single line ~20ms)
