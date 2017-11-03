# Jelly VM Architecture

The Jelly VM is a register-based, typed interpreter that executes validated bytecode modules. This document describes how the VM is structured and how execution works.

## Overview

The VM:

- Loads and validates bytecode (`.jlyb` format)
- Executes via a fetch–decode–dispatch–execute loop
- Maintains typed register frames; every register has a static type from the bytecode
- Uses a mark-sweep GC for heap objects
- Supports boxing/unboxing at the `Dynamic` boundary

Bytecode format and opcode semantics are defined in `docs/ISA.md`. The public API is in `vm/include/jelly.h`.

## Execution Pipeline

| Stage | Input               | Output                | Module(s)                    |
| ----- | ------------------- | --------------------- | ----------------------------- |
| 0     | CLI / embedder API  | Task dispatch         | `main.c`                      |
| 1     | Raw bytes           | Loaded module         | `bytecode/loader.c`           |
| 2     | Loaded module       | Validated module      | `bytecode/check.c`            |
| 3     | Validated module    | Execution result      | `vm/interp.c` + `vm/*.c`      |
| 4     | Allocation requests | GC-managed objects    | `gc.c`                        |
| 5     | Object operations   | Values / side effects | `types/*.c`                   |

### Main Loop

1. **Fetch** — read `insns[pc]` (fixed 8-byte `jelly_insn`)
2. **Decode** — op, a, b, c, imm (fixed layout)
3. **Dispatch** — branch to opcode handler (switch or computed goto)
4. **Execute** — handler reads/writes registers, may call runtime, push/pop frames
5. **Advance** — `pc++` or jump; check for traps and exceptions

The VM’s interpreter loop is implemented as `vm_exec_loop` (`vm/src/vm/ops/exec_loop.c`). It inlines a small hot subset of opcodes and falls back to the canonical dispatcher (`op_dispatch`) for everything else. For conformance/debugging, the VM can be built in a reference mode that routes all opcodes through `op_dispatch` (see `docs/vm_exec_loop.md`).

## Module Layout

```
vm/
├── src/
│   ├── main.c              # CLI, embedder entry
│   ├── vm.c                # jelly_vm_create, jelly_vm_destroy, exec API
│   ├── vm/
│   │   ├── interp.c        # Main loop: fetch, dispatch, exec_entry
│   │   ├── dispatch.c      # Opcode handler table
│   │   ├── frame.c         # reg_frame, call_frame, push/pop
│   │   ├── reg.c           # load/store for typed slots (u32, i64, f64, f16, etc.)
│   │   ├── box.c           # box_from_typed, store_from_boxed (typed↔Dynamic)
│   │   ├── spill.c         # spill_push, spill_pop (spill stack)
│   │   ├── exc.c           # Exception handling, unwind
│   │   ├── call.c          # Call setup, arg copying
│   │   └── ops/            # Opcode implementations by category
│   ├── gc.c                # Mark-sweep GC, root scanning
│   ├── bytecode/
│   │   ├── loader.c        # jelly_bc_read, format parsing
│   │   └── check.c         # Validation (reg bounds, types, jump targets)
│   └── types/              # Object implementations (bytes, list, array, object, etc.)
└── include/
    ├── jelly.h             # Public API (opaque types, embedder surface)
    └── jelly/
        └── internal.h      # Internal structures (reg_frame, bytecode layout)
```

## Typed Registers

Every register has a **static type** from the function’s `reg_types[]` table. The compiler guarantees correct types; the bytecode validator enforces them. Handlers never store a value of the wrong type into a register.

- **Typed regs** — unboxed; type comes from `(function, reg_id) → type_id`
- **Dynamic regs** — store boxed `jelly_value` (tagged or pointer)
- **Spill stack** — holds boxed values only; used when register pressure is high

## Frame and Register Access

- **reg_frame** — per-call frame with typed slots; layout computed from `reg_types`
- **reg_ptr(rf, r)** — pointer to slot for register `r`
- **load/store** — typed access (u32, i64, f64, f16, ptr) via `reg.c`

## Boxing and Unboxing

- **TO_DYN** — boxes a typed register into a `Dynamic` register
- **FROM_DYN_*** — unboxes with a type check; traps on mismatch
- Boxing allocates for I64, F64, F32, F16; I8/I16/I32/Bool/Atom use tagged immediates

See `docs/boxing.md` for details.

## GC and Roots

The GC is precise and scans:

- Spill stack
- `gc_roots`
- `const_fun_cache`
- Call frames (typed slots; only pointer slots are traced)

`scan_typed_frames` interprets `reg_types` to find pointer slots. All roots must be visible during collection.

## Exception Handling

- **TRY/ENDTRY** — push/pop handler
- **THROW** — set `exc_pending`, `exc_payload`; goto exception dispatch
- **Unwind** — pop frames above handler, restore `pc`, store payload in catch register

Handler stack and call stack must stay consistent during unwind.

## Embedding

Embedders need only `#include <jelly.h>`:

```c
jelly_vm* vm = jelly_vm_create();
jelly_bc_module* m = NULL;
jelly_bc_result r = jelly_bc_read(data, size, &m);
if (r.err == JELLY_BC_OK) {
  jelly_value out;
  jelly_exec_status st = jelly_vm_exec_status(vm, m, &out);
  if (st == JELLY_EXEC_OK) { /* use out */ }
}
jelly_bc_free(m);
jelly_vm_destroy(vm);
```

Internal structures (`reg_frame`, `call_frame`, bytecode layout) are not exposed.
