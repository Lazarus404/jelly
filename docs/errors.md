## Errors (compiler + runtime)

This document defines how errors are currently surfaced by `jellyc`, and what constraints we intend to preserve as error reporting improves.

For the VM trap/exception model, see `docs/error-model.md`.

### CLI exit codes (`jellyc`)

`jellyc` uses:

- `1`: compile error (parse/type/codegen)
- `2`: usage error or IO failure (read/write)

### Compile errors (today)

Errors are currently represented as plain strings. Parser errors include a **byte offset** into the UTF-8 source text (not line/column yet).

Common categories emitted by `jellyc`:

- **Parse errors**
  - unexpected EOF / unterminated literals
  - unexpected trailing input
  - “expected X” token errors (e.g. missing `;`, `)`, `}` or missing `else` / `catch`)
  - invalid escapes / invalid Unicode scalar values in string literals
- **Name / scope errors**
  - duplicate `let` bindings in the same scope
  - assignment to an unknown variable
  - `break` / `continue` outside of `while`
- **Type errors**
  - branch type mismatches (`if`, `try/catch`)
  - invalid operator operand types (`+`, `-`, comparisons, `!`)
  - array literal element mismatches
  - indexing/index-assign type mismatches
  - call target is not a function; arity mismatch; argument type mismatch
  - unsupported type shapes (e.g. generics other than `Array<T>`, unsupported `Array<T>`)
- **Feature-limit errors**
  - function literals and `return` are currently supported only in a restricted form
  - empty array literal typing is not implemented yet

### Runtime failures

Runtime failures (bounds, null deref, type mismatch at dynamic boundaries, uncaught throw) are VM traps/exceptions, not compiler errors.

`jellyc`’s responsibilities for runtime correctness are:

- emit bytecode that satisfies the VM’s validation and typing invariants
- insert explicit boxing (`TO_DYN`) where required (e.g. throw payloads)
- keep register types consistent with the VM type table

### Planned: spans and better diagnostics

The compiler roadmap includes attaching spans to AST nodes and reporting:

- line/column
- a short source excerpt
- stable, categorized error codes (parse/name/type/lowering/codegen)

Until then, compiler errors should remain:

- deterministic
- specific (no generic “compile failed”)
- small strings (no huge dumps in normal mode)

