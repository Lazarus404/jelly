## Jelly source language (current `jellyc` subset)

This document describes what `jellyc` **currently parses and compiles**. Anything not listed here should be considered “not implemented yet”, even if it exists in planning docs.

### File structure

A file is:

- zero or more **statements** (each must end with `;`)
- followed by one final **expression** (no trailing `;`)

The file’s final expression is the program result. In the current compiler, the program result must be `bytes`.

### Lexical notes

- Source is treated as **UTF-8**.
- Whitespace is spaces/tabs/newlines and is otherwise insignificant.
- There are **no comments** yet.
- Identifiers:
  - start: `_` or ASCII letter
  - continue: `_` or ASCII letter/digit

### Literals

- **Integers**: decimal `0..9+` (currently constrained to non-negative values that fit in `i32`)
  - Negative integers must be written with unary `-` (note: `-2147483648` is not representable yet due to literal range checking).
- **Booleans**: `true`, `false`
- **Null**: `null`
- **Strings / bytes**: `'...'` or `"..."` produce a `bytes` value (UTF-8 bytes)
  - newlines are not allowed inside a string literal
  - supported escapes: `\\`, `\n`, `\r`, `\t`, `\'`, `\"`, `\uXXXX`, `\u{X...}` (1–6 hex digits)

### Types (syntax only)

Type syntax is described in `docs/types.md`. In this language subset, type annotations appear:

- on `let` bindings: `let name: Type = expr;`
- on function parameters: `fn(x: Type, y: Type) { ... }` (parameter annotations are optional syntactically; typing rules vary by context)

### Expressions

#### Variables

- `name`

#### Calls

- `callee(arg0, arg1, ...)`

#### Arrays

- Literal: `[e0, e1, ...]`
- Indexing: `base[index]`

Notes:

- Empty arrays (`[]`) require an expected type; they are currently only supported when initializing a typed `let` (e.g. `let xs: Array<i32> = [];`).

#### Objects

Object literals use identifier keys:

- Empty object: `{}`
- Non-empty: `{a: expr, b: expr}`

Notes:

- Keys are **identifiers**, not strings.
- No trailing comma is supported.
- `{ ... }` is ambiguous with a block expression; it is parsed as an object literal only when it starts with `ident :`.

#### Prototypes and `new`

Prototypes are declared with a `prototype` statement:

- `prototype Name { field0: expr0; field1: expr1; ... };`
- `prototype Name<T, U> { ... };` (templated prototype)

Notes:

- A prototype declaration creates an object value named `Name` (it can be used where an `object` is expected).
- A prototype name can also be used as a **type** (see `docs/types.md`).

Instantiation uses `new`:

- `new Proto(args...)`

Semantics (MVP):

- Allocate a new object
- Set `__proto__` to `Proto`
- If `Proto` has an `init` field, call it as `init(self, ...args)`
- Result is the new object

#### Templates (compile-time)

Templates are compile-time only and are expanded before name resolution/lowering.

- Template definitions:
  - `let Name<T>: Type = expr;`
  - `prototype Name<T> { ... };` (syntax sugar to a templated `let` with an object literal)
- Type application at use-sites:
  - `Name<I32>` (value position)
  - `f<I32>(args...)` (calls)

`new` can be used with templated prototypes:

- `new Box<I32>(...)` (explicit)
- `new Box(x)` (inferred in some cases; see template tests under `ctest/object/`)

#### Blocks

Block expressions are:

`{ stmt* expr }`

The final expression is **required** (a block cannot end with `}` immediately, and cannot contain only statements).

#### `if/else` expressions

`if (cond) { then_expr } else { else_expr }`

Both branches are block expressions, so they must end in a value.

#### `try/catch` expressions

`try { body_expr } catch (name)? { catch_expr }`

- The `catch` binding is optional: `catch (e) { ... }` or `catch { ... }`.
- Both bodies are block expressions, so they must end in a value.

#### Function literals (MVP)

`fn(param0, param1, ...) { stmt* expr? }`

Notes (current compiler behavior):

- Function literals are currently only compiled in a restricted form (see `docs/types.md` and `docs/codegen.md`).
- A function body may end with an optional tail expression.

#### `match` expressions

Syntax:

`match (subject) { arm (, arm)* (,)? }`

Each arm is:

`pattern (when(<expr>))? => { stmt* expr? }`

Notes:

- Match is currently implemented in the **IR backend** (`--backend ir`) only.
- The subject expression is evaluated once.
- Arms are checked in source order.
- An arm’s `when(...)` guard is evaluated only after its pattern matches.
- **Fall-through**: if an arm body has **no** trailing expression value, control jumps to the next arm’s pattern checks.
- For now, match expressions must be **exhaustive**:
  - the final arm must be `_`
  - the final arm cannot have a `when(...)`
  - the final arm must produce a value (have a trailing expression)

##### Patterns

Supported pattern forms (current):

- **Wildcard**: `_`
- **Literals**: `true`, `false`, `123`, `-7`
- **Binding**: `name`
  - Introduces a new name scoped to the arm body/guard.
- **Pin**: `^name`
  - Matches against the value of an _existing_ name (does not bind).
- **Object destructure**: `{a: pat, b: pat, ...}`
  - Keys are identifiers (atoms).
  - A binding pattern like `{b: x}` binds `x` to the field value.
  - A pin pattern like `{b: ^x}` requires the field value to equal `x`.
- **Array/List head-tail**: `[head | rest]`
  - Matches a non-empty sequence.
  - `head` may be a binding, pin, wildcard, or literal pattern.
  - `rest` is a fresh binding name for the tail:
    - for `Array<T>`, `rest` is a new `Array<T>` containing elements `1..len-1`
    - for `List<T>`, `rest` is the tail list
- **Array/List prefix-rest**: `[p0, p1, ..., ...rest]`
  - Matches a sequence with at least `n` elements (where `n` is the prefix length).
  - `rest` binds to:
    - for `Array<T>`, a new `Array<T>` containing elements `n..len-1`
    - for `List<T>`, the list after dropping `n` heads

### Statements

#### `let`

`let name(: Type)? = expr;`

Scope rules (current):

- `let` introduces a lexical binding scoped to the nearest surrounding block (including `if`/`try` blocks).
- **Shadowing is currently rejected**: a `let` name must be unique across all active scopes.
- If a type annotation is present, the compiler checks that the initializer’s type matches it.

#### Modules (IR backend)

Modules are currently implemented in the **IR backend** (`--backend ir`).

- Import a module’s `exports` object:
  - `import some.package.module as Mod;`
- Import specific exports:
  - `import {add, sub} from math;`
  - `import {x as y} from consts;`
- Export a binding (must have a type annotation):
  - `export let name: Type = expr;`

#### Assignment

- `name = expr;`
- `name[index] = expr;` (index assignment is only supported when the base is a variable name)

#### `while`

`while (cond) { stmt* }`

The loop body is a statement block (no trailing expression value) and introduces a `let` scope: bindings declared inside the loop body do not escape the loop.

#### Control flow

- `break;`
- `continue;`
- `throw expr;`
- `return;`
- `return expr;`

### Operators and precedence

From highest to lowest:

- unary: `!e`, `-e`
- additive: `e + e`, `e - e`
- relational: `e < e`, `e <= e`, `e > e`, `e >= e`
- equality: `e == e`, `e != e`
- logical and/or: `e && e`, `e || e`

### Not implemented yet (examples)

- Ternary `?:`
- `do..while`
- Comments
- `match` in the AST backend (`--backend ast`)
