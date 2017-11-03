## Jelly VM error model

Jelly’s interpreter distinguishes between:

- **internal invariants** (bugs / corrupted state): handled by `panic()` / abort (MVP)
- **runtime failures** (valid bytecode, invalid runtime value or bounds): handled by **traps**

This keeps the hot path small while giving embedders a non-crashing failure mode.

### API surface

Use `jelly_vm_exec_status()` to execute bytecode with an explicit status:

- **`JELLY_EXEC_OK`**: execution completed successfully; output value is written to `*out`.
- **`JELLY_EXEC_TRAP`**: a runtime failure occurred; `*out` is unspecified and the VM records a trap.

Trap information is stored in the VM instance:

- `jelly_vm_last_trap_code(vm)`
- `jelly_vm_last_trap_msg(vm)`
- `jelly_vm_clear_trap(vm)` resets trap state

The legacy `jelly_vm_exec()` API remains and will **abort** on traps (useful for early bring-up and tests that assume no traps).

### Trap codes

Current `jelly_trap_code` values:

- **`JELLY_TRAP_TYPE_MISMATCH`**: a checked conversion failed (e.g. `FromDyn_*` with wrong payload)
- **`JELLY_TRAP_BOUNDS`**: container bounds / range failures (array/bytes index out of range, invalid byte value)
- **`JELLY_TRAP_NULL_DEREF`**: invalid dereference of a null/nil pointer (e.g. `list_head` on nil)
- **`JELLY_TRAP_THROWN`**: an exception was thrown but not caught (VM-level `throw`)

Trap messages are **static strings** (no heap allocation in the error path for MVP).

### What is trapped today

The interpreter traps (instead of aborting) for:

- **`FromDyn_*` mismatches**:
  - `FromDyn_I32/I64/F64/Bool/Atom/Ptr`
- **container errors**:
  - `Array`: `get/set/len` null deref, `get/set` index out of bounds
  - `Bytes`: `get/set/len` null deref, `get/set` index out of bounds, `set_u8` value out of range
- **list errors**:
  - `head/tail` on `nil`

### Exceptions / try-catch / throw

Jelly VM provides a minimal exception handler stack in bytecode:

- `TRY` / `ENDTRY` manage handlers
- `THROW` raises an `Any` payload (boxed `Dynamic`)

**Catching traps**: runtime traps (bounds/null/type mismatch/etc.) are raised through the same mechanism when a handler exists.
When a trap is caught, the catch payload is a boxed `i32` trap code (the `jelly_trap_code` value).

If an exception is not caught, execution returns `JELLY_EXEC_TRAP` and the VM records `JELLY_TRAP_THROWN`.

### GC safety on trap paths

Traps are returned from the interpreter loop and cleanup:

- unwinds all active call frames (freeing frame memory)
- leaves the GC heap owned by the VM, to be reclaimed via `jelly_vm_destroy()` / GC sweep rules

Temporary GC roots must still be balanced within instructions that allocate; the current MVP uses explicit push/pop rooting around multi-allocation sequences.
