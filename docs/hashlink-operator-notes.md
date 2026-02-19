## Hashlink operator notes (for Jelly reference)

This document captures **how Hashlink handles `+` and unary operators**, primarily for **numeric** and **string** behavior, so we can compare/contrast while keeping Jelly’s ISA small and correct.

### `+` on numbers

- **Typed code**: Hashlink’s bytecode uses a typed binary op (`OAdd`) and the compiler ensures operands are already in a compatible type (inserting conversions as needed).
  - The JIT selects integer vs float machine ops based on register type (e.g. float add vs int add).

- **Dynamic values**: Hashlink exposes `hl_dyn_op(op, a, b)` for dynamic arithmetic/bitwise operations.
  - For `+` specifically, if both values are numeric (or null) it **casts to double** and returns a **dynamic f64** result.
  - For non-numeric dynamic operands, it errors (i.e. dynamic `+` is not “string concat”).

### `+` on strings

Hashlink’s VM bytecode has a distinct string constant opcode (`OString`), but `+` string concatenation is not implemented as a generic “dynamic add”.

In practice (via Haxe semantics), string concatenation is typically lowered to **buffer-based building**:

- allocate a buffer
- append values with `hl_buffer_val` / `hl_buffer_str` (which use per-type formatting / `toString` hooks)
- extract a final string from the buffer

This means “string + anything” is handled by **explicit lowering** to runtime calls rather than a polymorphic VM add instruction.

### Unary operators

Hashlink has explicit unary opcodes:

- `ONeg` for numeric negation (int/float; i64 depending on platform/JIT support)
- `ONot` for boolean not
- plus increment/decrement (`OIncr` / `ODecr`) in the opcode set

### Jelly implications (guidance)

- Keep Jelly **monomorphic** in the interpreter: `ADD_*` ops should remain typed.
- If/when Jelly adds a String type, model string concatenation as either:
  - a dedicated `STR_CAT` runtime helper op, or
  - compiler-lowered calls to a buffer builder (Hashlink-style),
  rather than overloading numeric `ADD`.

