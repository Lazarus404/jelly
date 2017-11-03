## Boxing in Jelly (MVP)

Jelly is a **typed VM** with an explicit boundary between:

- **typed registers** (unboxed, type known from bytecode), and
- **`Dynamic` values** (boxed/tagged `jelly_value`).

This document defines the rules for boxing/unboxing so the VM stays **small**, **fast**, and **correct**.

### Key rules

- **Typed regs are not self-describing**: their type comes from `(function, reg_id) -> type_id`.
- **`Dynamic` is self-describing** via `jelly_value` tagging + heap object headers.
- **Boxing/unboxing is explicit in bytecode**:
  - `JOP_TO_DYN` boxes a typed register into a `Dynamic` register.
  - `JOP_FROM_DYN_*` unboxes a `Dynamic` register into a typed register with checks.
- **Spill is boxed-only**: `JOP_SPILL_PUSH/POP` operate on `Dynamic` regs only.

### `jelly_value` tags (Jello-style)

`jelly_value` is a `uintptr_t` using low-bit tags:

- **`null`**, **`bool`**, **`i32`** etc., **`atom`**: immediate tagged values
- **pointers**: heap objects (tag `PTR`), identified by `jelly_obj_header.kind`

Wide numerics are boxed as heap objects so tagging stays portable:

- `I64` -> `JELLY_OBJ_BOX_I64`
- `F64` -> `JELLY_OBJ_BOX_F64`
- `F32` -> `JELLY_OBJ_BOX_F32`

### Boxing behavior (`JOP_TO_DYN`)

`JOP_TO_DYN` boxes based on the **static type of the source register**:

- `I8/I16/I32` -> `i32` immediate (range-constrained in compiler)
- `Bool` -> `bool` immediate
- `Atom` -> `atom` immediate
- `I64/F64/F32` -> boxed heap object
- pointer kinds (`Bytes/Function/List/Array/Object/Abstract`) -> `null` or pointer `jelly_value`
- `Dynamic` -> copy

### Unboxing behavior (`JOP_FROM_DYN_*`)

Unboxing is **checked**:

- `JOP_FROM_DYN_I32`: requires `i32` immediate
- `JOP_FROM_DYN_I64`: requires `i32` immediate (promotes) or `BOX_I64`
- `JOP_FROM_DYN_F64`: requires `i32` immediate (promotes) or `BOX_F64`
- `JOP_FROM_DYN_BOOL`: requires `bool` immediate
- `JOP_FROM_DYN_ATOM`: requires `atom` immediate
- `JOP_FROM_DYN_PTR`: destination is a pointer-kind typed reg; requires:
  - `null` -> store `NULL`
  - pointer -> heap object kind matches destination kind

Mismatch behavior is currently a VM panic; it will become a structured trap/error in the “error model” stage.

### GC interaction

Because boxing allocates (e.g. `I64/F64/F32` boxes), instructions that allocate multiple objects must protect intermediates using the temp root stack:

- `jelly_gc_push_root(vm, value)`
- `jelly_gc_pop_roots(vm, n)`

This is required whenever a boxed value must survive across additional allocations before being stored into a traced location.
