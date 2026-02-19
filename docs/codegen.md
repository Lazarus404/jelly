## Code generation (`jellyc` → `.jlyb`)

This document describes what `jellyc` currently emits and the invariants it enforces while producing Jelly VM bytecode.

For VM instruction semantics, see `docs/ISA.md`.

### Module structure

`jellyc` emits a `.jlyb` module containing:

- header (magic/version/features + table counts)
- **type table** (`TypeEntry`)
- **signature table** (`FunSig`)
- **atoms section** (interned property keys)
- optional **bytes constant pool**
- **function table** (`Function`), each containing:
  - `reg_types`: type id per physical register
  - `insns`: bytecode

### Type ids (program modules)

In `build_program_module`, the initial type table uses fixed indices:

- `0`: `bytes`
- `1`: `bool`
- `2`: `i32`
- `3`: `dynamic` (`Any`)
- `4`: `Array<i32>`
- `5`: `Array<bytes>`
- `6`: `object`

Additional type entries are appended for function types as they are resolved (each is `TypeKind::Function { p0 = sig_id }`).

### Signature table

Function types are structural. When the compiler resolves a function type `(arg_types...) -> ret_type` it:

- interns a signature entry `{ ret_type, args[] }` into the module signature table (deduped by `(ret,args...)`)
- creates (or reuses) a corresponding `TypeKind::Function` type entry that points at that signature id

### Atoms

Object literal keys are interned into `atoms`:

- each new key bytestring is assigned the next atom id
- atom ids are referenced from bytecode (e.g. `OBJ_SET_ATOM`)

Atom ordering is currently “first occurrence during compilation”.

### Bytes constant pool

String literals (`'...'` / `"..."`) compile to `bytes` values and are emitted into a bytes constant pool:

- each unique literal is appended to `const_bytes`
- bytecode uses `CONST_BYTES` with the pool index

### Functions and entrypoint

The module function table is:

- function 0: the compiled top-level program
- function 1..N: nested functions produced by function literals

`entry` is currently `0`.

### Expression lowering (high level)

Selected rules (current implementation):

- **`if (cond) {a} else {b}`**
  - `cond` must be `bool`
  - both branches must produce the same type
  - lowering uses `JMP_IF` + `JMP` plus a join destination and explicit `MOV` on each path
- **`try {a} catch (e)? {b}`**
  - emits `TRY` with a destination `dynamic` register for the caught payload
  - both bodies must produce the same type; result is merged via explicit `MOV`
  - emits `ENDTRY` on the non-exceptional path
- **`throw x;`**
  - if `x` is not already `dynamic`, it is boxed via `TO_DYN`
  - emits `THROW` with the payload
- **Arrays**
  - array literals are allocated with `ARRAY_NEW` then populated with `ARRAY_SET`
  - indexing uses `ARRAY_GET`
  - currently only `Array<i32>` and `Array<bytes>` are supported
- **Bytes**
  - indexing uses `BYTES_GET_U8` / `BYTES_SET_U8`
  - concatenation uses `BYTES_CONCAT2` for 2 terms
  - long `+` chains on `bytes` are flattened into `BYTES_CONCAT_MANY` by building an `Array<bytes>` of pieces
- **Objects**
  - object literals allocate `OBJ_NEW` then assign fields using `OBJ_SET_ATOM`

### Calls (and late lowering)

`CALLR` requires its arguments in a contiguous physical register window (`imm..imm+nargs`).

In the IR backend, lowering materializes each call’s arguments into a contiguous vreg window.
During final emission, those vregs are **pinned** to a per-signature contiguous “arg block” appended to `reg_types`,
and `CALLR` is emitted with `imm = arg_block_start`.

Because calls no longer expand into extra instructions, jump/try relative deltas do not need recomputation at emission time.

### Current top-level constraint

The top-level program expression must evaluate to `bytes`; otherwise `jellyc` reports a type error.
