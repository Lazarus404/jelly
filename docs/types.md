## Types (current `jellyc` subset)

This describes the type syntax and the set of types that `jellyc` currently understands.

### Type syntax

#### Named types

`Name`

Examples: `i32`, `bytes`, `bool`, `object`, `Any`

#### Generic types

`Base<T0, T1, ...>`

Examples: `Array<i32>`, `Array<bytes>`

#### Function types

Function types are written using `->` and are **right-associative**:

- `A -> B -> C` means `A -> (B -> C)`

To write a multi-argument function type, use parentheses:

- `(A, B) -> C`

### Built-in types

`jellyc` currently resolves the following built-in names (case-insensitive for these specific spellings):

- `bytes` (`Bytes`)
- `bool` (`Bool`)
- `i32` (`I32`)
- `object` (`Object`)
- `Any` / `dynamic` / `Dynamic`

Notes:

- Canonical spelling in docs/examples is **lowercase** (`i32`, `bool`, `bytes`, `object`, `dynamic`), but the compiler currently accepts the alternative spellings above for compatibility.
- The language concept of “string” is currently represented as `bytes`; there is **no `string` type name** yet.
- `Any`/`dynamic` is the dynamic top type (VM `Dynamic`) and is used for exception payloads.

### Nominal prototype types (Object-kind)

In addition to the built-in type names above, `jellyc` also supports **nominal object subtypes** derived from prototype names.

- A `prototype` declaration introduces both:
  - a **value** (the prototype object), and
  - a **type constructor** with the same name.

Examples:

- `prototype P { ... };` introduces the nominal type `P`.
- `prototype Box<T> { ... };` introduces the nominal generic type `Box<T>`.

Nominal prototype types are **Object-kind**:

- They behave like `object` at runtime (no VM distinction).
- The compiler may still track their type ids for checking/ABI, but it is valid to use a nominal object where an `object` is expected.

### Arrays

Arrays are `Array<T>`, but the current compiler only supports:

- `Array<i32>`
- `Array<bytes>`

Array literals require all elements to have the same type. Empty array literals (`[]`) require an expected element type; the compiler currently supports them only when initializing a typed `let` (e.g. `let xs: Array<i32> = [];`).

### Function values (MVP)

`jellyc` supports first-class function values via a structural function type:

- each distinct `(arg_types...) -> ret_type` produces a signature table entry
- `fun`-typed values carry a signature id (via the module’s type table)

Backend notes (important for correctness):

- In the **AST backend** (`--backend ast`), a function literal expression (`fn(...) { ... }`) is only compiled when it appears in a **typed `let`**:

  - `let f: (i32) -> i32 = fn(x: i32) { return x; };`

- In the AST backend, the function body is restricted:
  - only `return <expr>;` statements are allowed in the body (at most one)
  - or a single tail expression (no statements)
- In the **IR backend** (`--backend ir`), function bodies support normal statements and locals, and closures/captures are supported.

### Inference (today)

Inference is currently “expression-directed” and local:

- `let name = expr;` uses the type of `expr`.
- `let name: Type = expr;` requires `expr` to match `Type` exactly (no implicit conversions).

Broader inference policy and annotation requirements are planned and tracked in `.cursor/rules/compiler/plan.md`.

