### `new` Stemming from Objects (Prototypal Purity)

This document is exploratory. Below is a **proposed direction** that tries to keep the model:
simple to implement, fast in the VM, and consistent with the current compiler constraints.

#### MVP constraints (current compiler reality)

As of the current `jellyc` subset:

- There are **no comments** in `.jelly` yet.
- Strings are `bytes` (there is no distinct `string` type).
- There is method-call sugar (`obj.m(args...)`) which binds `this` via `BIND_THIS` (but `new` still uses an explicit `self` parameter for `init`).
- There is no `super` yet.
- Objects are an **atom→value map** with `OBJ_HAS_ATOM` / `OBJ_GET_ATOM` / `OBJ_SET_ATOM`, and `OBJ_GET_ATOM` traverses the `__proto__` chain.

So the initial “prototyping” design should avoid depending on `super` or new surface forms like `prototype Foo { ... }` until we decide on a clean, typed story for templates.

#### Suggested MVP (simpler + VM-friendly)

**Keep prototypes as plain objects**, and standardize two reserved atoms:

- `__proto__`: optional prototype link (an `Object` or `null`)
- `init`: optional initializer function

Then define `new` as sugar:

- `new P(a, b, c)`:
  1. allocate `self = {}` (fresh object)
  2. set `self.__proto__ = P`
  3. if `P.init` exists, call it as a normal function with an **explicit self parameter**:
     - `P.init(self, a, b, c)`
  4. return `self`

This avoids needing a special `this` binding mechanism in the MVP while still giving constructor-like ergonomics.

**Key VM rule (recommended):** `OBJ_GET_ATOM` should traverse the `__proto__` chain when a property is missing on the object itself, while `OBJ_HAS_ATOM` should remain **own-only**. This keeps:

- property access (`obj.field`) prototypal
- pattern matching checks (which use `OBJ_HAS_ATOM`) stable and predictable

Example in the current style (`bytes` strings, `init(self, ...)`):

```jelly
let Animal = {
    init: fn(self: Object, name: Bytes) { self.name = name; self },
    walk: fn(self: Object) { self.name + " walks" },
};

let Rabbit = {
    __proto__: Animal,
    hop: fn(self: Object) { self.name + " hops" },
};

let b = new Rabbit("Bugs");
// `walk` is inherited from Animal via `__proto__`.
Animal.walk(b); // or later: b.walk()
```

In JavaScript (ECMAScript), the `new` operator is tightly coupled to **functions** acting as constructors. When you do `new Foo(args)`, the runtime:

- Creates a fresh empty object `{}`.
- Sets its internal prototype (`[[Prototype]]` or `__proto__`) to `Foo.prototype` (which is typically an object holding shared methods/properties).
- Binds `this` to the new object and invokes `Foo` as a function to initialize instance-specific state (e.g., `this.name = args[0]`).
- Returns the new object (unless the function returns something else explicitly).

This makes functions feel "special" in inheritance chains—`Foo` itself isn't the prototype; `Foo.prototype` is. It's a historical artifact that blends functional and object-oriented paradigms, but it can obscure the pure prototypal model where **everything is an object**, and inheritance is just delegation via prototype links.

In your language, to emphasize **true prototypal inheritance** (inspired by Self or early JS but without the function-centric baggage), `new` stems directly from an **object** that serves as the prototype template. This object (let's call it a "prototype object" or "archetype") is the blueprint:

- It holds shared properties/methods (like JS's `Foo.prototype`).
- It may include an optional **initializer** (a method or block run on instantiation, similar to a constructor but not a standalone function).
- `new PrototypeObj(args)` creates a new instance by:
  1. Creating a fresh object.
  2. Setting its internal prototype to `PrototypeObj` itself (not `PrototypeObj.prototype`—no extra layer).
  3. Running the initializer (if defined) with `this` bound to the new instance and passing `args`.
  4. Returning the new instance.

This design:

- Keeps the language more object-centric: No need for functions to masquerade as constructors. Prototypes are just enhanced objects.
- Avoids JS pitfalls like forgetting `new` (leading to global pollution) or the `Foo.prototype.constructor` dance.
- Aligns better with your no-classes rule—everything is delegating to objects, not invoking functions.
- Still feels ECMA-like: `new` syntax is familiar, but the operand is an object reference (e.g., a variable holding the prototype object).

If instantiation doesn't need args or custom init, you could even skip `new` and use `Object.create(prototypeObj)` for pure cloning, but keeping `new` provides sugar for common cases.

#### Syntactic Examples

Building on our earlier options, here's how this looks in practice. I'll use the "prototype block" for clarity (as it's type-friendly and supports generics), but it could work with object literals too.

```jelly
// Define a prototype object via a block (creates an object named Animal)
prototype Animal {
    eats: bool = true;  // Shared default (delegated if not overridden)

    init(name: string) {  // Optional initializer (like constructor body)
        this.name = name; // Instance-specific
    }

    walk(): void {
        console.log(`${this.name} walks`);  // Shared method
    }
}

// Inheritance: Rabbit's prototype is set to Animal (or a clone of it)
prototype Rabbit : Animal {  // : means "prototype = Object.create(Animal)"
    jumps: bool = true;

    init(name: string) {
        super.init(name);  // Delegate to parent's init (sugar for Animal.init.call(this, name))
        this.jumps = true; // Or override fully
    }

    hop(): void {
        console.log(`${this.name} hops!`);
    }
}

// Instantiation: new stems from the object 'Rabbit'
let bunny = new Rabbit("Bugs");  // Runs init with args, sets [[Prototype]] = Rabbit
bunny.hop();                     // Delegates to Rabbit.hop
bunny.walk();                    // Delegates to Animal.walk via chain

// Without init args
let genericAnimal = new Animal();  // Default init (if none, just empty or inferred)
```

If using **object literals** (more minimal):

```jelly
let animal = {
    eats: true,
    init(name: string) { this.name = name; },
    walk() { console.log(`${this.name} walks`); }
};

let rabbit = {
    prototype: animal,  // Or from: animal
    jumps: true,
    init(name: string) { super.init(name); },  // super delegates up the chain
    hop() { console.log(`${this.name} hops!`); }
};

let bunny = new rabbit("Bugs");  // new from the object 'rabbit'
```

Under the hood (VM level):

- The prototype object (e.g., `Rabbit`) has an internal flag or slot indicating it's instantiable (e.g., via having an `init` method).
- No separate `.prototype` property— the object _is_ the prototype.
- This simplifies the register-based VM: Fewer indirections, easier optimization (e.g., inline delegation).

Differences from JS:

- JS: `new` requires a function; here, an object.
- JS: Prototype chain starts from `Func.prototype`; here, directly from the object.
- JS: Constructors can be called without `new` (risky); here, `new` is mandatory for init, reinforcing safety.
- Pros: More intuitive for prototypal mental model; easier typing (see below).
- Cons: Less backward-compatible with JS code, but since your VM is custom, that's fine.

### Templating (Parametric Polymorphism / Generics)

Your language supports **templates** like `List<T>`, which is parametric polymorphism—code that works over arbitrary types `T` without knowing them in advance. This is compile-time (static) generics, similar to TypeScript/C++/Rust, but adapted to a prototypal world.

#### How It Works

- **Declaration**: Use angle brackets `<T>` (or multiple `<T, U>`) to parameterize types, functions, or prototype objects.
- At definition, `T` is a placeholder (type variable).
- At usage/instantiation, you specialize it (e.g., `List<string>`), generating a concrete type (monomorphization in the VM—either at compile-time or JIT).
- Supports bounds/constraints if needed (e.g., `<T: Numeric>`), but keep it simple if your type system is gradual.

Syntax examples:

```jelly
// Generic prototype (object-based)
prototype Stack<T> {
    items: List<T> = List<T>();  // Uses built-in List<T>

    init(initial: List<T>?) {
        if (initial) this.items = initial;
    }

    push(item: T): void {
        this.items.add(item);
    }

    pop(): T? {  // Optional return for empty
        return this.items.removeLast();
    }
}

// Instantiation with specialization
let intStack = new Stack<int>([1, 2, 3]);  // T = int
intStack.push(4);                         // OK
intStack.push("hello");                   // Type error (static if checked)

// Inheritance with generics
prototype PriorityQueue<T : Comparable> : Stack<T> {  // Constraint: T must implement Comparable
    // ...
}
```

- **Built-ins**: Assume primitives like `List<T>`, `Map<K, V>` are provided by the runtime, implemented efficiently in the VM (e.g., as register-optimized arrays).
- **Inference**: See typing section below.
- **VM Implications**: In a register-based VM, generics might expand to specialized bytecode (e.g., `List<int>` uses int-specific ops), or use dynamic dispatch if gradual typing allows runtime flexibility.
- Differs from JS: JS has no native generics (TS adds them overlays), but here it's baked-in for performance/safety.

Advanced: Support variance (e.g., `List<out T>` for covariance), but start minimal.

### Typing System (Static/Gradual with Inference)

Your language is **typed**—variables/expressions have types—but **gradual**: You can mix static (compile-time checked) and dynamic (runtime-checked) typing. This is like TypeScript: Optional annotations, with inference filling gaps.

#### Key Features

- **Static Typing**: Where possible, check at compile-time (e.g., `let x: int = "foo";` errors immediately).
- **Gradual**: Omit types for dynamic feel (e.g., `let x = 5;`), but runtime checks prevent mismatches (e.g., adding string to int throws).
- **Inference**: Compiler deduces types from context, reducing boilerplate.
- **Structural Typing**: Types are based on shape (duck typing with checks), fitting prototypal delegation. E.g., if an object has `{name: string, walk(): void}`, it satisfies `Animal` without explicit declaration.
- **Union/Intersection**: `string | int`, `Animal & Jumper`.
- **Nullability**: `T?` for optional (like TS `T | null`).

#### Inference in Action

- **Local Variables**: `let x = 5;` infers `int`.
- **Functions**: `func add(a, b) { return a + b; }` infers `(a: infer, b: infer) -> infer`, but specializes on call (e.g., `add(1, 2)` -> int).
- **Generics**: Heavy inference—`let stack = new Stack([1, 2]);` infers `Stack<int>`.
- **Prototypes**: In blocks/literals, infer from assignments: `prototype Animal { eats: true }` infers `{eats: bool}`.

Full example with inference:

```jelly
prototype Animal {  // Inferred type: {eats: bool, init(name: string): void, walk(): void}
    eats = true;    // Inferred bool

    init(name) {    // name inferred string (from usage? Or require annotation for params)
        this.name = name;  // this.name inferred string
    }

    walk() {        // Inferred (): void
        console.log(this.name);  // Uses inferred this: Animal
    }
}

let bunny = new Rabbit("Bugs");  // Inferred Rabbit
let nameless = new Animal();     // Inferred Animal, init args optional? -> runtime check or default
```

- **Error Handling**: Static errors for mismatches; runtime for dynamic parts (e.g., if inferred wrong due to branches).
- **VM Integration**: Registers can be typed (e.g., int reg vs. obj reg) for optimization; gradual allows fallback to boxed any.

This setup keeps the language flexible (prototypal, ECMA-like) while adding safety/performance via types and templates.
