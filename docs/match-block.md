## Match blocks (Jelly lowering + semantics)

Jelly match blocks are designed to be **compiler-lowered** onto a small core ISA:

- control flow: `JMP`, `JMP_IF`
- dynamic introspection: `KINDOF` (available; not used by the current compiler yet)
- object/map queries: `OBJ_HAS_ATOM`, `OBJ_GET_ATOM`
- list ops: `LIST_IS_NIL`, `LIST_HEAD`, `LIST_TAIL`
- array ops: `ARRAY_LEN`, `ARRAY_GET`
- comparisons: `EQ_*`
- conversions: `FromDyn_*` (checked)

This keeps the VM surface area small while supporting correct match semantics.

### Supported arm features

Each arm may include:

- **pattern checks** (literals, container shape checks, property checks)
- **when clause** (a boolean guard)
- **body** (a statement block and optionally a final expression)

### Execution model

- The **subject expression is evaluated once** and stored in a register (`subject`).
- Arms are evaluated in source order.
- An arm is selected only if **all** of its checks pass:
  - pattern checks
  - when clause (if present)
- If an arm’s body evaluates a **final expression** (match used as expression), codegen must jump to the match end after storing the result.

### Fall-through rule (Jelly extension)

Jelly supports **fall-through** when an arm’s body does **not** end in an expression/expression-block.

Lowering rule:

- If the arm body has **no final value**, the compiler emits a jump to the **next arm’s checks** (fall-through).
- If the arm body **does** produce a value (match-expression), the compiler emits a jump to the **match end** after producing the result.

This rule is purely a *lowering convention*; the VM only executes the control flow.

### Dynamic subjects (future optimization)

The VM provides `KINDOF` + `SWITCH_KIND` as a fast dispatch helper for matches on `Dynamic`.
The current compiler primarily lowers match checks linearly and relies on static subject types where possible.

When/if we add match dispatch on `Dynamic` subjects, `KINDOF`/`SWITCH_KIND` should be preferred where it’s a clear win.

Kind codes are documented in `docs/ISA.md` under `JOP_KINDOF`.

### Property checks (Object subject)

For an Object subject, property checks use atom ids (interned keys):

- `OBJ_HAS_ATOM out_bool, obj, atom_id`
  - if false: jump to next arm

For value checks:

- `OBJ_GET_ATOM out_T, obj, atom_id`
- compare `out_T` against the pattern value (e.g. `EQ_I32`)
  - if false: jump to next arm

Property bindings (captures) are just `OBJ_GET_ATOM` results stored in registers for the arm body.

### Sequence checks (List / Array subjects)

Jelly match blocks can destructure **lists** and **arrays** using the core container ops.

List pattern building blocks:

- **nil check**: `LIST_IS_NIL`
- **head**: `LIST_HEAD` (only valid when not nil)
- **tail**: `LIST_TAIL` (only valid when not nil)

Array pattern building blocks:

- **length**: `ARRAY_LEN`
- **index access**: `ARRAY_GET`

Lowering rule (important for correctness):

- `LIST_HEAD`/`LIST_TAIL` must be guarded by a preceding `LIST_IS_NIL == false` check.
- `ARRAY_GET` must be guarded by checks that ensure the index is in-bounds (typically via `ARRAY_LEN` plus comparisons), otherwise it will trap (`BOUNDS`) under the VM’s error model.

### When clause

The when clause is lowered as a boolean expression into a `Bool` typed register, then:

- `JMP_IF when_bool, +to_body`
- `JMP +to_next_arm`

### “Switch / jump-table” note

For correctness, a linear arm chain is sufficient and is what tests use.
For performance, Jelly also provides a small jump-table helper (`SWITCH_KIND` + `CASE_KIND`) to dispatch efficiently on `KINDOF` results without changing the semantics above.

