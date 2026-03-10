# Jelly Language Coverage Checklist

This document describes the Jelly language and maps each feature to test coverage. Use it as both a language reference and a checklist to ensure sufficient tests for a usable implementation.

---

## 1. Language Overview

Jelly is a **statically typed**, **prototypal** language with **type inference** (Haxe-like ergonomics) and **ECMA-like syntax**. It targets a typed bytecode VM with explicit boxing at the `Dynamic` boundary.

- **Paradigm**: Functional bias; first-class functions and closures; prototypal objects with correct `this` binding
- **Source encoding**: UTF-8
- **Test convention**: Programs return `"ok"` on success; `System.assert(cond)` for checks; negative tests expect compile or runtime failure

---

## 2. Type System

### 2.1 Primitives

| Type | Description | Tests |
|------|-------------|-------|
| `i8`, `i16`, `i32`, `i64` | Signed integers | `numeric/small_type_arith.jelly`, `numeric/literal_suffixes.jelly` |
| `f16`, `f32`, `f64` | Floats | `numeric/div_float_implicit.jelly`, `numeric/div_i32_f32.jelly` |
| `bool` | Boolean | `bool/basic.jelly`, `bool/eq_truthy.jelly`, `bool/and_or.jelly` |
| `atom` | Symbol/atom | `atom/literal_get.jelly` |

**Rules**: No implicit narrowing; widening allowed. Integer overflow wraps. Float→int is checked (trap on NaN/Inf/out-of-range).

### 2.2 Heap Types

| Type | Description | Tests |
|------|-------------|-------|
| `bytes` | UTF-8 string (alias `string`) | `bytes/hello.jelly`, `bytes/concat_many.jelly`, `bytes/slice.jelly` |
| `list<T>` | Linked list | `match/list_head_tail.jelly` |
| `array<T>` | Contiguous array | `array/index_sugar.jelly`, `array/bytes_ops.jelly` |
| `object` | Prototypal object | `object/literal.jelly`, `object/get.jelly`, `object/assign.jelly` |
| `fun(...) -> ...` | Function type | `fn/infer_ackerman.jelly`, `closure_call.jelly` |
| `Any` | Dynamic top type | `object/dynamic_access.jelly` |

### 2.3 Inference

| Feature | Description | Tests |
|---------|-------------|-------|
| Local inference | Locals can be inferred | `fn/infer_param_numeric_add.jelly`, `fn/infer_param_bytes_concat.jelly` |
| Param inference | Params inferred from usage | `fn/infer_ackerman.jelly`, `fn/infer_nested_fn_capture_and_param.jelly` |
| Widening at call site | Arg widened to match param | `fn/infer_outer_typed_fun_call_widen_arg.jelly` |

**Policy**: No global HM; bidirectional typing; inference at module boundaries is restricted.

---

## 3. Expressions

### 3.1 Literals

| Literal | Form | Tests |
|---------|------|-------|
| Integer | `42`, `42i8`, `42i64`, etc. | `numeric/literal_suffixes.jelly` |
| Float | `3.14`, `3.14f32`, `3.14f64` | `numeric/div_float_implicit.jelly` |
| Bool | `true`, `false` | `bool/basic.jelly` |
| Bytes/string | `'...'`, `"..."` | `bytes/hello.jelly` |
| Interpolated string | `` `... ${expr} ...` `` | `bytes/interp_basic.jelly`, `bytes/interp_many.jelly`, `bytes/interp_order.jelly` |
| Atom | `#atom` | `atom/literal_get.jelly` |
| Object | `{a: 1, b: "hi"}` | `object/literal.jelly` |
| Tuple | `(a, b)` | `tuple/basic.jelly`, `tuple/eq.jelly` |

### 3.2 Operators

| Category | Operators | Rules | Tests |
|----------|-----------|-------|-------|
| Arithmetic | `+ - * /` unary `-` | Numeric promotion; no implicit narrowing | `numeric/arith_interchangeable.jelly`, `numeric/mod_basic.jelly`, `numeric/shift_basic.jelly` |
| Comparison | `== != < <= > >=` | Value-based for numerics; cross-type allowed | `numeric/chained_eq_numeric.jelly`, `i32/relops.jelly`, `i32/lt.jelly` |
| Logical | `&& \|\|` | Short-circuit | `bool/shortcircuit_minimal.jelly`, `bool/shortcircuit_trap.jelly` |
| Bytes concat | `+` | `bytes + bytes -> bytes` only | `bytes/hello.jelly`, `bytes/concat_many.jelly` |

### 3.3 String Escapes

| Escape | Description | Tests |
|--------|-------------|-------|
| `\\`, `\'`, `\"`, `` \` ``, `\n`, `\r`, `\t`, `\0` | Basic | `bytes/hello.jelly` |
| `\uXXXX` | 4 hex digits | `bytes/unicode_escape.jelly` |
| `\u{X...}` | 1–6 hex digits | `bytes/unicode_escape.jelly` |

**Validity**: Reject surrogates `U+D800..U+DFFF` and `> U+10FFFF`.

---

## 4. Control Flow

### 4.1 Conditionals

| Construct | Syntax | Tests |
|-----------|--------|-------|
| `if/else` | `if (cond) { ... } else { ... }` | `if/basic.jelly`, `if/join.jelly` |
| Ternary | `cond ? a : b` (desugared to if) | (via `if/`) |
| Short-circuit | `&&`, `\|\|` | `bool/shortcircuit_*.jelly`, `bool/let_if_and.jelly` |

### 4.2 Loops

| Construct | Syntax | Tests |
|-----------|--------|-------|
| `while` | `while (cond) { ... }` | `while/countdown.jelly` |
| `do..while` | `do { ... } while (cond)` | `while/do_while.jelly`, `while/do_while_break_continue.jelly` (break) |
| `break` / `continue` | Target nearest loop | `while/break_continue.jelly` |

### 4.3 Match

| Feature | Description | Tests |
|---------|-------------|-------|
| Scalar arms | `1 => {...}`, `2 => {...}` | `match/basic.jelly` |
| Wildcard | `_ => {...}` (required last) | `match/basic.jelly` |
| Bind | `x => {...}` binds subject | — |
| Pin | `^x` matches existing var | `match/object_pin.jelly` |
| Object | `{a: p, b: p}` | `match/object_wildcard.jelly`, `match/object_own_only_proto.jelly` |
| Tuple | `(p0, p1)` | `match/tuple_basic.jelly`, `match/tuple_wildcard.jelly` |
| Array exact | `[p0, p1]` | — |
| Array head/tail | `[h \| rest]` | `match/array_head_tail.jelly` |
| Array prefix/rest | `[p0, p1, ...rest]` | `match/array_prefix_rest.jelly` |
| When guard | `pat when (cond) => {...}` | `match/fallthrough_when.jelly`, `match/fallthrough_when_no_expr.jelly` |
| Fallthrough | `pat,` (no `=>`) | `match/fallthrough_when.jelly` |
| Dynamic scalar | Match on `Any` numeric/bool | `match/dynamic_scalar.jelly` |

### 4.4 With (Elixir-style)

| Feature | Description | Tests |
|---------|-------------|-------|
| All match | `with p1 <- e1, p2 <- e2 { body }` | `with/basic.jelly` |
| First fails | Else runs | `with/first_fails.jelly` |
| Second fails | Else runs | `with/second_fails.jelly` |
| No else | Returns failure value | `with/no_else.jelly` |
| Exhaustive else | Last arm `_ => ...` | `with/exhaustive_else.jelly` |

---

## 5. Functions and Closures

### 5.1 Functions

| Feature | Description | Tests |
|---------|-------------|-------|
| Function expr | `fn(x) { ... }` | `fn/infer_param_numeric_add.jelly` |
| Typed params | `fn(x: I32) { ... }` | `closure_call.jelly` |
| Return | `return expr;` | `function/body_locals.jelly` |
| Tail expr | Block tail as return | `function/tail_expr.jelly` |
| Void return | No value | `function/return_void.jelly` |

### 5.2 Closures

| Feature | Description | Tests |
|---------|-------------|-------|
| Capture | Free vars in closure | `fn/capture_no_params.jelly`, `fn/capture_raw_caps.jelly`, `function/closure_capture.jelly` |
| Nested | Closure in closure | `fn/infer_nested_fn_capture_and_param.jelly` |
| Call through var | `let f = fn(...){...}; f(x)` | `closure_call.jelly` |

### 5.3 Calls

| Feature | Description | Tests |
|---------|-------------|-------|
| Direct | `f(x)` | `call/basic.jelly` |
| Callee expr | `(choose())(x)` | `call/callee_expr.jelly` |
| In control flow | Call in `if`, `while` | `call/simple_if.jelly`, `call/in_while.jelly` |
| Large arity | Many args | `call/large_arity.jelly` |

---

## 6. Objects and Prototypes

### 6.1 Object Basics

| Feature | Description | Tests |
|---------|-------------|-------|
| Literal | `{a: 1, b: "hi"}` | `object/literal.jelly` |
| Get | `obj.field` | `object/get.jelly` |
| Assign | `obj.field = v` | `object/assign.jelly` |
| Add member | Dynamic add | `object/add_member.jelly` |
| Dynamic access | `obj[expr]` on `Any` | `object/dynamic_access.jelly` |

### 6.2 Prototypes

| Feature | Description | Tests |
|---------|-------------|-------|
| Prototype syntax | `proto { m() { ... } }` | `object/prototype_syntax_basic.jelly`, `object/prototype_syntax_template.jelly` |
| Prototype sugar | `proto { m() { ... } }` | `object/prototype_sugar_basic.jelly`, `object/prototype_sugar_template.jelly` |
| Delegation | Property lookup via proto | `object/prototype_delegation.jelly` |
| As type | `proto` as type | `object/prototype_as_type.jelly` |

### 6.3 Methods and `this`

| Feature | Description | Tests |
|---------|-------------|-------|
| Method call | `obj.m()` binds `this` | `object/method_call_this.jelly` |
| Const method | `const m = fn() { ... }` | `object/method_const_fun.jelly` |
| Across fn | Method passed to fn | `object/across_fn.jelly` |

### 6.4 Instantiation

| Feature | Description | Tests |
|---------|-------------|-------|
| `new` | `new Proto()` | `object/new_calls_init.jelly` |
| `new` with args | `new Proto(a, b)` | `object/new_calls_init_args.jelly` |
| Template proto | `new Proto<T>()` | `object/new_template_proto_init.jelly`, `object/new_template_proto_infer.jelly` |
| Template ops | Operations on template types | `object/template_ops.jelly` |
| Runtime metadata | Template at runtime | `object/template_runtime_metadata.jelly` |

---

## 7. Modules and Imports

| Feature | Description | Tests |
|---------|-------------|-------|
| Main | Entry module | `module/main.jelly` |
| Consts | Module-level const | `module/consts.jelly` |
| Import | `import "path"` | `module/math.jelly` |
| Named imports | `import { a, b } from "path"` | `module/named_imports.jelly` |

---

## 8. Exceptions

| Feature | Description | Tests |
|---------|-------------|-------|
| `try/catch` | Catch throws and traps | `try-catch/basic.jelly`, `try-catch/minimal.jelly` |
| `throw` | Explicit throw | `assert/caught.jelly` |
| Uncaught | No handler | `assert/uncaught.jelly` (expect fail) |

---

## 9. Const and Static

| Feature | Description | Tests |
|---------|-------------|-------|
| `const` | Immutable binding | `const/basic_fold.jelly`, `const/bytes_alias.jelly` |
| Assign to const fails | Compile error | `const/assign_fails.jelly` (expect fail) |

---

## 10. Blocks and Scoping

| Feature | Description | Tests |
|---------|-------------|-------|
| Block expr | `{ stmt* expr }` | `block/expr.jelly` |
| Let in block | `let x = e` in scope | `expr/let_in_if.jelly` |

---

## 11. Indexing

| Feature | Description | Tests |
|---------|-------------|-------|
| Array `[i]` | Index by `i32` | `array/index_sugar.jelly` |
| Array `[i]` i8/i16 | Index by smaller int | `array/index_sugar_i8_index.jelly`, `array/index_sugar_i16_index.jelly` |
| Bytes `[i]` | Index bytes | `bytes/index_sugar_i16_index.jelly` |

---

## 12. Recursion

| Feature | Description | Tests |
|---------|-------------|-------|
| Basic | Self-call | `recursion/basic.jelly` |
| Tail | Tail call | `recursion/tail.jelly` |
| Mutual | A calls B, B calls A | `recursion/tail_mutual.jelly` |

---

## 13. Coverage Gaps (Resolved)

| Area | Tests | Notes |
|------|-------|-------|
| `do..while` | `while/do_while.jelly` | Body runs at least once |
| Match bind | `match/bind.jelly` | `y => { use y }` binds subject |
| Match array exact | `match/array_exact.jelly` | `[a, b] => {...}` |
| List patterns | `match/list_head_tail.jelly` | `[h \| t]` on list |
| Negative type | `errors/implicit_narrow_i32_to_i8.jelly` | Compiler rejects i32→i8 (expect fail) |
| `Any` coercion | `bytes/to_bytes_builtin.jelly` | `Integer.to_bytes`, `Float.to_bytes` |
| Module re-export | `module/reexport.jelly` | Export imported binding |
| REPL | `jellyc/tests/integration/repl.rs` | Incremental parse/exec |

**Deferred**: Float→int NaN/Inf trap — VM asserts on NaN; needs VM trap path.

---

## 14. Test Execution

- **jellyc tests**: Compile `.jelly` → `.jlyb`, run on VM, expect `"ok"` return.
- **Negative tests**: `assert/uncaught.jelly`, `const/assign_fails.jelly`, `errors/implicit_narrow_i32_to_i8.jelly` expect failure.
- **Run**: `ctest --test-dir build` (or `-R jellyc` for jellyc-only).
- **Add test**: Place `.jelly` in `ctest/<feature>/`; CMake auto-registers via `file(GLOB_RECURSE ...)`.

---

## 15. Summary

Jelly is a **statically typed prototypal language** with:

- **Types**: Primitives (`i8`–`i64`, `f16`–`f64`, `bool`, `atom`), heap types (`bytes`, `list`, `array`, `object`, `fun`), and `Any`
- **Inference**: Locals and params; bidirectional; no global HM
- **Objects**: Prototypal with `this`; `new Proto()`; templates
- **Control**: `if`, `while`, `do..while`, `break`/`continue`, `match`, `with`
- **Functions**: First-class; closures; tail recursion
- **Strings**: UTF-8 `bytes`; interpolation; Unicode escapes
- **Exceptions**: `try/catch`, `throw`; VM traps caught

The `ctest/` suite covers most features. Use this checklist to identify gaps and to describe the language when adapting external tests or onboarding contributors.
