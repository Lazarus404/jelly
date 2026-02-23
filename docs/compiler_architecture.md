# Jelly Compiler Architecture

This document describes the structure of the Jelly compiler (`jellyc`) and how its components relate. The compiler is written in Rust and produces Jelly bytecode (`.jlyb`) for the Jelly VM.

---

## 1. Pipeline Overview

The compilation pipeline for source modules proceeds as follows:

```
Source (.jelly)
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  link::load_module_graph                                                    │
│  - Parses entry file, resolves imports (DFS), loads source or bytecode      │
│  - For each source module: frontend::prepare_program during load            │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼  (per source module)
┌─────────────────────────────────────────────────────────────────────────────┐
│  frontend::prepared(prog)  (proof: already prepared during load)            │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  semantic::analyze_prepared_module_init                                     │
│  - Typecheck + capture analysis → (HirProgram, SemanticInfo)                │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  lower::lower_module_init_to_ir                                             │
│  - HIR → SSA-style IR (IrModule)                                            │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  opt::run_passes                                                            │
│  - cfg_simplify, ssa, phi_prune, boundary_*, copy_prop, phi_simplify,       │
│    gvn, dce, cse, const_prop (fixpoint)                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  phi::eliminate_phis                                                        │
│  - Removes phi nodes via parallel moves                                     │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  opt::run_post_phi_cleanup                                                  │
│  - cfg_simplify, boundary_*, copy_prop, dce, cse, const_prop                │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  codegen::emit_ir_module                                                    │
│  - vstream → allocate → finalize → jlyb::Module                             │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  jlyb::validate_module                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│  link::link_modules_and_build_entry                                         │
│  - Merges modules, resolves types/atoms/consts/funcs, builds entry          │
└─────────────────────────────────────────────────────────────────────────────┘
    │
    ▼
Bytecode (.jlyb)
```

**Key invariant:** All semantic and lowering entry points operate on programs that have already been prepared via the frontend. This happens during module-graph loading and is represented by `frontend::PreparedProgram`.

---

## 2. Module Layout

### 2.1 Top-Level Structure

| Module      | Purpose                                                                  |
| ----------- | ------------------------------------------------------------------------ |
| `ast`       | Abstract syntax tree: `Program`, `Stmt`, `Expr`, `Pattern`, `Ty`, `Span` |
| `token`     | Token definitions (`Token`, `TokenKind`)                                 |
| `lex`       | Lexer: source → tokens                                                   |
| `parse`     | Parser: tokens → AST (recursive descent)                                 |
| `frontend`  | Frontend pipeline: templates → normalize → resolve                       |
| `templates` | Template expansion (e.g. string interpolation)                           |
| `visit`     | AST visitors (walk, Visitor, VisitorMut)                                 |
| `hir`       | High-level IR: AST + semantic side tables                                |
| `semantic`  | Typecheck, capture analysis                                              |
| `lower`     | HIR → IR lowering                                                        |
| `ir`        | SSA-style intermediate representation                                    |
| `opt`       | IR optimization passes                                                   |
| `phi`       | Phi elimination                                                          |
| `codegen`   | IR → bytecode (vstream, allocate, finalize)                              |
| `regalloc`  | Register allocation (LSRA, spill, parallel moves)                        |
| `peephole`  | Peephole optimization on bytecode                                        |
| `jlyb`      | Bytecode format, prelude, validation                                     |
| `link`      | Module loading, graph, linking                                           |
| `typectx`   | Type context, `TypeRepr`, `TypeCtx`                                      |
| `error`     | `CompileError`, `ErrorKind`                                              |
| `compile`   | Orchestration entry points                                               |

---

## 3. Frontend

### 3.1 Lexer (`lex/`)

- **core.rs:** Main lex loop, token production
- **keywords.rs:** Keyword recognition (`let`, `if`, `else`, `match`, `with`, etc.)
- **numbers.rs, strings.rs, escape.rs:** Literal parsing

### 3.2 Parser (`parse/`)

- **Entry:** `parse_program(src)` → `Program`
- **token_p.rs:** `P` (token parser) with `expect`, `eat`, `peek`, etc.
- **expr/:** Expression parsing (recursive descent)
  - `entry.rs` — dispatch to `control`, `op`, `lit`, `access`, etc.
  - `control.rs` — `if`, `match`, `with`, `let`, etc.
  - `pattern.rs` — pattern parsing
- **stmt/:** Statement parsing
- **ty/:** Type parsing

### 3.3 Frontend Pipeline (`frontend.rs`)

```
prepare_program(p: &mut Program)
    │
    ├─► templates::expand_templates(p)
    │       - Expands string interpolation, etc.
    │
    ├─► normalize_truthiness_program(p)
    │       - Normalizes truthiness for typecheck
    │
    └─► resolve_program(p)
            - Name resolution (imports, exports, locals)
```

`PreparedProgram<'a>` is a proof type: it guarantees the AST has been expanded, normalized, and resolved.

---

## 4. Semantic Analysis

### 4.1 Entrypoints (`semantic/entrypoint.rs`)

- `analyze_prepared_module_init` — main path for module compilation
- `analyze_program` — typecheck + capture on raw `Program`
- `analyze_prepared_program` — typecheck + capture on `PreparedProgram`

### 4.2 Typecheck (`semantic/typecheck/`)

- **checker.rs:** `TypeChecker`, `TypecheckInputs`; drives the typecheck
- **module_init.rs:** `typecheck_module_init` — module-level typecheck with imports
- **expr*let.rs, control.rs, match*.rs, fn\_.rs, op.rs, lit.rs, ...** — per-node typecheck

The typechecker produces `SemanticInfo`:

- `expr_types`, `binding_types` — `NodeId` → `TypeId`
- `const_inits` — compile-time constant values
- `captures` — closure capture info
- `type_ctx` — `TypeCtx` with interned types

### 4.3 Capture Analysis (`semantic/entrypoint/capture.rs`)

Computes which variables are captured by closures, keyed by `NodeId`.

### 4.4 HIR (`hir/`)

- **HirProgram:** Wraps `Program`; HIR is AST-shaped with semantic-normalized nodes
- **SemanticInfo:** Side tables (expr_types, binding_types, const_inits, captures, type_ctx)
- **NodeId:** `Span` used as node identifier
- **render.rs, type_name.rs:** HIR rendering utilities

---

## 5. Lowering (HIR → IR)

### 5.1 Entry (`lower/entry.rs`)

- `lower_module_init_to_ir` — main entry; builds `LowerCtx`, lowers stmts + expr
- `intern_type_repr` — maps `TypeRepr` → `TypeId` in IR type context

### 5.2 Context (`lower/`)

- **LowerCtx:** Holds `type_ctx`, const pools, atoms, `env_stack`, `loop_stack`, `fn_stack`, semantic tables, etc.
- **env:** Variable lookup, binding
- **atoms, base_types:** Atom/type helpers

### 5.3 Expression Lowering (`lower/expr/`)

| Submodule  | Handles                                              |
| ---------- | ---------------------------------------------------- |
| `access`   | Member access, indexing                              |
| `builtins` | Builtin calls                                        |
| `call`     | Function calls                                       |
| `control`  | `if`, `loop`, `break`, `continue`                    |
| `dispatch` | Method dispatch                                      |
| `fn_`      | Function literals, closures                          |
| `lit`      | Literals                                             |
| `match`    | `match` and `with` (match driver, check, bind, with) |
| `new_`     | Object construction                                  |
| `numeric`  | Numeric ops, coercions                               |
| `op`       | Binary/unary operators                               |
| `truthy`   | Truthiness checks                                    |
| `tuple_eq` | Tuple equality                                       |

### 5.4 Statement Lowering (`lower/stmt/`)

Lowers `Stmt` (e.g. `Let`, `Import`) into IR blocks and instructions.

---

## 6. IR (Intermediate Representation)

### 6.1 Model (`ir/model.rs`)

- **IrModule:** `types`, `sigs`, `const_i64`, `const_f64`, `const_bytes`, `atoms`, `funcs`, `entry`
- **IrFunction:** `name`, `param_count`, `cap_vregs`, `entry` (BlockId), `blocks`, `vreg_types`
- **IrBlock:** `label`, `insns`, `term`
- **IrInsn:** `span`, `op`

### 6.2 Operations (`ir/op.rs`)

`IrOp` includes:

- Constants: `ConstI32`, `ConstI64`, `ConstF64`, `ConstBytes`, `ConstAtom`, `ConstFun`, `Mov`, ...
- Arithmetic: `AddI32`, `SubI32`, `MulI32`, `EqI32`, `LtI32`, ... (and I64, F16, F32, F64)
- Conversions: `SextI64`, `TruncI8`, `F32FromF64`, ...
- Bytes: `BytesNew`, `BytesLen`, `BytesGetU8`, `BytesConcat2`, ...
- Lists, objects, calls, closures, phi nodes, terminators

### 6.3 Terminators (`ir/terminator.rs`)

`IrTerminator`: `Jump`, `Branch`, `Return`, `Unreachable`, `Phi` (phi node for SSA)

### 6.4 IDs (`ir/ids.rs`)

`BlockId`, `TypeId`, `VRegId` — newtype indices

---

## 7. Optimization

### 7.1 Main Passes (`opt/run_passes`)

Fixpoint loop:

1. `cfg_simplify` — simplify control flow
2. `ssa::convert_to_ssa` — one-time SSA conversion
3. `phi_prune` — prune phi incomings
4. `boundary_simplify` — simplify dynamic boundaries
5. `boundary_fold` — fold local dynamic boundaries
6. `copy_prop` — copy propagation
7. `phi_simplify` — simplify phis
8. `gvn` — global value numbering
9. `dce` — dead code elimination
10. `cse` — common subexpression elimination
11. `const_prop` — constant propagation

### 7.2 Post-Phi Cleanup (`opt/run_post_phi_cleanup`)

After phi elimination: `cfg_simplify`, `boundary_simplify`, `boundary_fold`, `copy_prop`, `dce`, `cse`, `const_prop` (no SSA, no phi passes).

---

## 8. Phi Elimination

- **phi/eliminate.rs:** Removes phi nodes via parallel moves
- **phi/cfg.rs:** CFG utilities for phi elimination

---

## 9. Codegen

### 9.1 Phases

| Phase        | Module                | Purpose                                                       |
| ------------ | --------------------- | ------------------------------------------------------------- |
| **vstream**  | `codegen/vstream/`    | IR → `VirtualStream` (virtual instructions, no physical regs) |
| **allocate** | `codegen/allocate/`   | Register allocation (LSRA) → `Allocation`                     |
| **finalize** | `codegen/finalize.rs` | Spill insertion + peephole → bytecode `Insn` list             |

### 9.2 VStream (`codegen/vstream/`)

- **entry.rs:** `build_virtual_stream` — drives the conversion
- **emit/:** Emits virtual instructions per `IrOp`
- **layout.rs, liveness.rs, term.rs:** Layout and liveness for codegen
- **const_fun_phi.rs:** ConstFun phi handling

### 9.3 Allocate (`codegen/allocate/`)

- **alloc.rs:** LSRA-based register allocation
- **call_windows.rs:** Call ABI windows
- **type_class.rs:** Type classification for allocation

### 9.4 Finalize (`codegen/finalize.rs`)

- Inserts spill/reload ops via `regalloc::spill::insert_spill_ops`
- Runs `peephole::peephole` on the final instruction list

### 9.5 ABI / Layout

- **call_abi.rs:** Pins call arg blocks, validates layout
- **model.rs:** `VirtualStream`, `Allocation`, `reg` helper
- **cfg.rs:** `block_order_rpo`, `block_successors`, `blk_pc`, `term_size`, etc.

---

## 10. Register Allocation (`regalloc/`)

- **lsra.rs:** Linear scan register allocation
- **spill.rs:** Spill insertion (boxed Dynamic only)
- **parallel_moves.rs:** `schedule_parallel_moves` for phi elimination
- **live.rs, types.rs:** Liveness and allocation types

---

## 11. Linking

### 11.1 Load (`link/load.rs`)

- `load_module_graph` — DFS load of modules from entry path
- For source: parse → `frontend::prepare_program` → collect imports/exports
- For bytecode: read `.jlyb`, extract ABI

### 11.2 Graph (`link/graph.rs`)

- `ModuleNode`: `key`, `file` (`LoadedFile`), `import_keys`, `exports`
- `LoadedFile`: `Source { path, src, prog }` or `Bytecode { path, module }`

### 11.3 Linker (`link/linker.rs`)

- `link_modules_and_build_entry` — merges modules, maps types/atoms/consts/funcs, builds final entry module

### 11.4 ABI (`link/abi.rs`)

- `build_module_abi_blob` — encodes export/import info for module ABI

---

## 12. Data Flow Summary

```
Source
  → lex → tokens
  → parse → AST (Program)
  → frontend (templates, normalize, resolve) → PreparedProgram
  → semantic (typecheck, capture) → (HirProgram, SemanticInfo)
  → lower → IrModule (SSA IR)
  → opt (fixpoint) → optimized IR
  → phi::eliminate_phis → IR without phis
  → opt::run_post_phi_cleanup → cleaned IR
  → codegen (vstream → allocate → finalize) → jlyb::Module
  → link → final bytecode
```

---

## 13. Key Files Reference

| Purpose                   | Path                                            |
| ------------------------- | ----------------------------------------------- |
| Compilation orchestration | `jellyc/src/compile.rs`                         |
| Frontend pipeline         | `jellyc/src/frontend.rs`                        |
| Parse entry               | `jellyc/src/parse/mod.rs`                       |
| Semantic entry            | `jellyc/src/semantic/entrypoint/mod.rs`         |
| Typecheck entry           | `jellyc/src/semantic/typecheck/mod.rs`          |
| Lower entry               | `jellyc/src/lower/entry.rs`                     |
| IR model                  | `jellyc/src/ir/model.rs`, `jellyc/src/ir/op.rs` |
| Opt pipeline              | `jellyc/src/opt/mod.rs`                         |
| Codegen entry             | `jellyc/src/codegen/entry.rs`                   |
| VStream                   | `jellyc/src/codegen/vstream.rs`                 |
| Link load                 | `jellyc/src/link/load.rs`                       |
| Linker                    | `jellyc/src/link/linker.rs`                     |
