## Jelly ISA (MVP)

This is the canonical reference for Jelly’s bytecode container format and opcode semantics.

- **Goal**: keep the ISA **small**, **fast**, and **very correct**.
- **Typed VM rule**: every register has a **static type** (module type table + per-function `reg_types[]`).
- **No implicit numeric coercions in arithmetic**: the compiler emits explicit conversions; arithmetic ops are monomorphic.
- **Dynamic rule**: `Dynamic` registers store boxed `jelly_value` (Jello-style tagging); typed registers are unboxed.

### Bytecode container format (`JLYB`, version 1)

All integers are **little-endian**.

#### Header

- `u32 magic` = `'JLYB'` (`0x4A4C5942`)
- `u32 version` = `1`
- `u32 features` = bitset (`jelly_bc_feature_flags`)
- `u32 ntypes`
- `u32 nsigs`
- `u32 natoms`
- `u32 nfuncs`
- `u32 entry` (function index)
- If `features & JELLY_BC_FEAT_CONST64`:
  - `u32 nconst_i64`
  - `u32 nconst_f64`

#### Type table

`ntypes * (u8 kind, u8 pad, u16 pad, u32 p0, u32 p1)`

`kind` is `jelly_type_kind` (see `vm/include/jelly/type.h`).

- `LIST/ARRAY`: `p0 = elem_type_id`
- `FUNCTION`: `p0 = sig_id`
- `ABSTRACT`: `p0 = abstract_id`
- otherwise: `p0/p1` unused

#### Signature table

For each signature:

- `u32 ret_type`
- `u16 nargs`
- `u16 pad`
- `u32 arg_type[nargs]`

#### Atom section (MVP)

For each atom:

- `u32 utf8_len`
- `u8 bytes[utf8_len]`

Atoms are stored as UTF-8, NUL-terminated strings and become available as stable IDs `0..natoms-1`.

#### Optional 64-bit const pools (`JELLY_BC_FEAT_CONST64`)

These appear after atoms and before the function table.

- `i64 const_i64[nconst_i64]` (encoded as `u32 lo, u32 hi`)
- `f64 const_f64[nconst_f64]` (encoded as `u32 lo, u32 hi` bit pattern)

#### Optional bytes const pool (`JELLY_BC_FEAT_CONSTBYTES`)

Appears after atoms and after any `CONST64` pools, and before the function table.

For each entry:

- `u32 len`
- `u8 bytes[len]`

#### Function table

For each function:

- `u32 nregs` (<= 256)
- `u32 ninsns`
- `u32 reg_type[nregs]` (type_id)
- `insn[ninsns]`, each:
  - `u8 op`
  - `u8 a`
  - `u8 b`
  - `u8 c`
  - `u32 imm`

### Execution model (interpreter MVP)

- Register-based, typed frames.
- Spill stack exists for register pressure, but **spill holds boxed `jelly_value` only**.
- GC is precise and scans typed frames using `reg_types[]`.

### Opcode reference

Notation:

- `rX:T` means register `X` with static type `T` (as per `reg_types[]`).
- Unless stated otherwise, indices must be in range and types must match.

#### Control / misc

- **`JOP_NOP`**: no-op
- **`JOP_RET a`**: return `box(rA)` as the function result (boxing per type)

#### Moves

- **`JOP_MOV a b`**: copy `rB` to `rA` (types must match)

#### Calls / closures

- **`JOP_CALL a b c imm`**:
  - calls function `imm` (direct by index)
  - args are `rB..r(B+c-1)` copied into callee `r0..`
  - result stored into `rA` in caller (boxed/unboxed as per destination type)

- **`JOP_CALLR a b c imm`**:
  - calls closure/function object in `rB:function`
  - args base is `imm`, nargs is `c` (args in `rImm..r(imm+c-1)`)
  - if closure has `bound_this`, it is injected as callee arg0
  - captures are copied into callee’s trailing registers

- **`JOP_CONST_FUN a imm`**:
  - `rA:function = fun(func_index=imm)`

- **`JOP_CLOSURE a b c imm`**:
  - `rA:function = closure(func_index=imm, captures=rB..r(B+c-1))`
  - captures are boxed at capture-time

- **`JOP_BIND_THIS a b c`**:
  - `rA:function = bind_this(rB:function, this=rC)`

#### Typed constants

- **`JOP_CONST_I32 a imm`**:
  - stores `imm` into `rA` if `rA` is `I8/I16/I32` (stored as 32-bit slot for MVP)

- **`JOP_CONST_BOOL a c`**:
  - stores `(c & 1)` into `rA:Bool`

- **`JOP_CONST_NULL a`**:
  - stores `null` into `rA:Dynamic`

- **`JOP_CONST_ATOM a imm`**:
  - stores `imm` into `rA:Atom`

- **`JOP_CONST_F32 a imm`**:
  - stores `bitcast_f32(imm)` into `rA:F32`

- **`JOP_CONST_I64 a imm`**:
  - stores `const_i64[imm]` into `rA:I64`
  - requires `features & JELLY_BC_FEAT_CONST64`

- **`JOP_CONST_F64 a imm`**:
  - stores `const_f64[imm]` into `rA:F64`
  - requires `features & JELLY_BC_FEAT_CONST64`

- **`JOP_CONST_BYTES a imm`**:
  - allocates a new `bytes` object with payload `const_bytes[imm]`
  - requires `features & JELLY_BC_FEAT_CONSTBYTES`

#### Bytes helpers

- **`JOP_BYTES_CONCAT2 a b c`**:
  - `rA:bytes = concat2(rB:bytes, rC:bytes)`
  - traps on null inputs
  - traps if the resulting length exceeds `u32`

- **`JOP_BYTES_CONCAT_MANY a b`**:
  - `rA:bytes = concat_many(rB:Array<bytes>)`
  - traps on null array or null/non-bytes elements
  - traps if the resulting length exceeds `u32`

#### Control flow

Jumps use signed relative deltas in `imm`, relative to the next pc.

- **`JOP_JMP imm`**: `pc += (int32)imm`
- **`JOP_JMP_IF a imm`**: if `rA:Bool != 0`, `pc += (int32)imm`

#### Exceptions (VM-level)

- **`JOP_TRY a imm`**:
  - pushes an exception handler
  - `rA` must be `Dynamic`; on catch, the payload is stored into `rA`
  - `imm` is a signed relative delta to the catch pc, relative to the next pc
- **`JOP_ENDTRY`**:
  - pops the top handler for the current frame
- **`JOP_THROW a`**:
  - raises the `Dynamic` value in `rA`

Traps (bounds/null/type mismatch) are raised through the same facility when a handler exists.
When a trap is caught, the catch payload is a boxed `i32` equal to the `jelly_trap_code`.

#### Typed arithmetic (monomorphic)

- **`JOP_ADD_I32 a b c`**, **`JOP_SUB_I32`**, **`JOP_MUL_I32`**
  - `rA:I32 = rB:I32 op rC:I32`
  - integer arithmetic is **defined to wrap modulo 2^32**

- **`JOP_ADD_I64 a b c`**, **`JOP_SUB_I64`**, **`JOP_MUL_I64`**
  - `rA:I64 = rB:I64 op rC:I64`
  - integer arithmetic is **defined to wrap modulo 2^64**

- **`JOP_ADD_F32 a b c`**, **`JOP_SUB_F32`**, **`JOP_MUL_F32`**
  - `rA:F32 = rB:F32 op rC:F32`

- **`JOP_ADD_F64 a b c`**, **`JOP_SUB_F64`**, **`JOP_MUL_F64`**
  - `rA:F64 = rB:F64 op rC:F64`

#### Typed conversions

- **`JOP_SEXT_I64 a b`**: `rA:I64 = sext(rB:I32)`
- **`JOP_I32_FROM_I64 a b`**: `rA:I32 = (int32)rB:I64` (trunc)
- **`JOP_F64_FROM_I32 a b`**: `rA:F64 = (double)rB:I32`
- **`JOP_I32_FROM_F64 a b`**: `rA:I32 = (int32)rB:F64` (trunc toward 0)
  - traps if input is NaN/Inf or outside the representable i32 range
- **`JOP_F64_FROM_I64 a b`**: `rA:F64 = (double)rB:I64`
- **`JOP_I64_FROM_F64 a b`**: `rA:I64 = (int64)rB:F64` (trunc toward 0)
  - traps if input is NaN/Inf or outside the representable i64 range
- **`JOP_F32_FROM_I32 a b`**: `rA:F32 = (float)rB:I32`
- **`JOP_I32_FROM_F32 a b`**: `rA:I32 = (int32)rB:F32` (trunc toward 0)
  - traps if input is NaN/Inf or outside the representable i32 range

#### Comparisons

- **`JOP_EQ_I32 a b c`**: `rA:Bool = (rB:I32 == rC:I32)`
- **`JOP_LT_I32 a b c`**: `rA:Bool = (rB:I32 < rC:I32)` (signed)
- **`JOP_EQ_I64 a b c`**: `rA:Bool = (rB:I64 == rC:I64)`
- **`JOP_EQ_F32 a b c`**: `rA:Bool = (rB:F32 == rC:F32)`
- **`JOP_EQ_F64 a b c`**: `rA:Bool = (rB:F64 == rC:F64)`

#### Boxing boundary ops

- **`JOP_TO_DYN a b`**:
  - `rA:Dynamic = box(rB)` (boxing depends on `rB`’s static type)

- **`JOP_FROM_DYN_I32 a b`**:
  - `rA:I32 = unbox_i32(rB:Dynamic)`

- **`JOP_FROM_DYN_I64 a b`**:
  - `rA:I64 = unbox_i64(rB:Dynamic)`

- **`JOP_FROM_DYN_F64 a b`**:
  - `rA:F64 = unbox_f64(rB:Dynamic)`

- **`JOP_FROM_DYN_BOOL a b`**:
  - `rA:Bool = unbox_bool(rB:Dynamic)`

- **`JOP_FROM_DYN_ATOM a b`**:
  - `rA:Atom = unbox_atom(rB:Dynamic)`

- **`JOP_FROM_DYN_PTR a b`**:
  - `rA:<pointer-kind> = unbox_ptrkind(rB:Dynamic)`
  - permitted destination kinds: `Bytes/Function/List/Array/Object/Abstract`
  - if `rB` is `null`, store `NULL` pointer into `rA`
  - otherwise require the heap object kind matches the destination register kind

#### Spill (boxed only)

- **`JOP_SPILL_PUSH a`**: push `rA:Dynamic` onto spill stack
- **`JOP_SPILL_POP a`**: pop into `rA:Dynamic`

#### Unary ops

- **`JOP_NEG_I32 a b`**: `rA:I32 = -rB:I32`
- **`JOP_NEG_I64 a b`**: `rA:I64 = -rB:I64`
- **`JOP_NEG_F32 a b`**: `rA:F32 = -rB:F32`
- **`JOP_NEG_F64 a b`**: `rA:F64 = -rB:F64`
- **`JOP_NOT_BOOL a b`**: `rA:Bool = !rB:Bool`

#### Identity

- **`JOP_PHYSEQ a b c`**:
  - `rA:Bool = (box(rB) == box(rC))` (physical equality on boxed form)

#### Match / type introspection

- **`JOP_KINDOF a b`**:
  - `rA:I32 = kindof(rB:Dynamic)`
  - kind codes (stable for compiler lowering):
    - `0` = null
    - `1` = bool
    - `2` = atom
    - `3` = i8
    - `4` = i16
    - `5` = i32
    - `6` = i64
    - `7` = f16
    - `8` = f32
    - `9` = f64
    - `10` = bytes
    - `11` = list
    - `12` = array
    - `13` = object
    - `14` = function
    - `15` = abstract
    - `16` = dynamic
    - `0x7fffffff` = unknown pointer-kind (reserved)

- **`JOP_SWITCH_KIND a b imm`**:
  - Jump-table helper specialized for dispatching on `KINDOF` kind codes.
  - Reads `rA:I32` (a kind code) and jumps to the first matching case entry.
  - `b` is `ncases` (0–255).
  - The _next_ `ncases` instructions must be `JOP_CASE_KIND` entries (data-only).
  - All deltas are signed `imm32` values **relative to the pc after the case table** (post-fetch semantics).

- **`JOP_CASE_KIND a imm`**:
  - Data-only case-table entry for `JOP_SWITCH_KIND`.
  - `a` is the `kind` code (u8).
  - `imm` is the relative delta **from the end of the case table** to the case target.

#### Containers

List (immutable):

- `JOP_LIST_NIL`: `rA:List<T> = nil`
- `JOP_LIST_CONS`: `rA:List<T> = cons(rB:T, rC:List<T>)`
- `JOP_LIST_HEAD`: `rA:T = head(rB:List<T>)`
- `JOP_LIST_TAIL`: `rA:List<T> = tail(rB:List<T>)`
- `JOP_LIST_IS_NIL`: `rA:Bool = is_nil(rB:List<T>)`

Array:

- `JOP_ARRAY_NEW`: `rA:Array<T> = new(len=rB:I32)`
- `JOP_ARRAY_LEN`: `rA:I32 = len(rB:Array<T>)`
- `JOP_ARRAY_GET`: `rA:T = rB[rC:I32]`
- `JOP_ARRAY_SET`: `rB[rC:I32] = rA:T`

Bytes:

- `JOP_BYTES_NEW`: `rA:bytes = new(len=rB:I32)`
- `JOP_BYTES_LEN`: `rA:I32 = len(rB:bytes)`
- `JOP_BYTES_GET_U8`: `rA:I32 = get_u8(rB:bytes, rC:I32)`
- `JOP_BYTES_SET_U8`: `set_u8(rB:bytes, rC:I32, rA:I32)`

Object (atom-keyed map):

- `JOP_OBJ_NEW`: `rA:Object = new()`
- `JOP_OBJ_HAS_ATOM`: `rA:Bool = has(rB:Object, atom_id=imm)`
- `JOP_OBJ_GET_ATOM`: `rA:T = get(rB:Object, atom_id=imm)`
- `JOP_OBJ_SET_ATOM`: `set(rB:Object, atom_id=imm, rA:T)`

#### Atom table (module header)

Bytecode modules include an atom table of length `natoms`. Atom IDs are stable indices `0..natoms-1`.

On disk (in `jelly_bc_read` format), atoms are encoded as:

- `u32 utf8_len`
- `u8 utf8_bytes[utf8_len]` (not NUL-terminated on disk)

In memory, the loader stores atoms as UTF-8 NUL-terminated strings in `m.atoms[]`.
