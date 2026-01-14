# Lina

An ML-family language compiling to Lua. Written in OCaml.

Lina combines the safety and expressiveness of ML-style static typing with Lua's lightweight runtime, making it ideal for scripting, game development, and embedded systems.

## Quick Start

```bash
# Build
dune build

# Compile and run
dune exec linac -- file.lina | luajit

# Run tests
dune test
./test/integration/run_tests.sh
```

## Example

```lina
(* Define a polymorphic list type *)
type 'a list = Nil | Cons of 'a * 'a list

(* Pattern matching with exhaustiveness checking *)
let rec length xs = match xs with
  | Nil -> 0
  | Cons (_, rest) -> 1 + length rest

(* Modules with abstract types *)
module Stack = struct
  type 'a t = 'a list
  let empty = Nil
  let push x s = Cons (x, s)
  let pop s = match s with
    | Nil -> None
    | Cons (x, rest) -> Some (x, rest)
end

let main =
  let s = Stack.push 1 (Stack.push 2 Stack.empty) in
  print (length s)
```

---

## Type System Comparison with OCaml

Lina's type system is designed to be compatible with OCaml's. The following table shows feature-by-feature comparison, verified by our [OCaml compatibility test suite](test/ocaml_compatibility/) (57 tests, all passing).

### Legend

| Symbol | Meaning |
|--------|---------|
| :white_check_mark: | Fully implemented and tested |
| :large_orange_diamond: | Partially implemented |
| :x: | Not implemented |
| :star: | Lina extension (not in OCaml) |

---

### Core Type System

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Hindley-Milner inference** | :white_check_mark: | :white_check_mark: | Algorithm W with principal types |
| **Let-polymorphism** | :white_check_mark: | :white_check_mark: | Generalization at `let` bindings |
| **Level-based generalization** | :white_check_mark: | :white_check_mark: | Efficient O(1) generalization check |
| **Type annotations** | :white_check_mark: | :white_check_mark: | Optional, never required |
| **Recursive types** | :white_check_mark: | :white_check_mark: | `type 'a list = Nil \| Cons of 'a * 'a list` |
| **Mutually recursive types** | :white_check_mark: | :white_check_mark: | `type a = ... and b = ...` |
| **Type aliases** | :white_check_mark: | :white_check_mark: | `type int_pair = int * int` |
| **Parameterized types** | :white_check_mark: | :white_check_mark: | `type 'a option`, `type ('a, 'b) either` |

### Value Restriction

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Wright's value restriction** | :white_check_mark: | :white_check_mark: | Only syntactic values generalized |
| **Relaxed value restriction** | :white_check_mark: | :white_check_mark: | Garrigue 2004: covariant positions generalize |
| **Weak type variables** | :white_check_mark: | :white_check_mark: | `'_a` for non-generalizable variables |
| **Lambda as value** | :white_check_mark: | :white_check_mark: | `fun x -> x` is a value |
| **Application as non-value** | :white_check_mark: | :white_check_mark: | `f x` blocks generalization |
| **Constructor as value** | :white_check_mark: | :white_check_mark: | `Some x` is value if `x` is value |
| **Tuple as value** | :white_check_mark: | :white_check_mark: | `(a, b)` is value if components are values |
| **If/match as non-value** | :white_check_mark: | :white_check_mark: | Control flow blocks generalization |

### Variance

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Variance inference** | :white_check_mark: | :white_check_mark: | Automatic for type declarations |
| **Explicit variance annotations** | :white_check_mark: | :white_check_mark: | `type +'a t`, `type -'a t` |
| **Covariant positions** | :white_check_mark: | :white_check_mark: | Function results, tuple elements |
| **Contravariant positions** | :white_check_mark: | :white_check_mark: | Function arguments |
| **Invariant (ref)** | :white_check_mark: | :white_check_mark: | `'a ref` is invariant |
| **Bivariant (phantom)** | :white_check_mark: | :white_check_mark: | Unused type parameters |
| **Mutable field invariance** | :white_check_mark: | :white_check_mark: | `mutable` fields are invariant |

### GADTs (Generalized Algebraic Data Types)

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **GADT syntax** | :white_check_mark: | :white_check_mark: | `Int : int -> int expr` |
| **Type refinement in branches** | :white_check_mark: | :white_check_mark: | Match refines type variables |
| **Type equation extraction** | :white_check_mark: | :white_check_mark: | `a = int` from `Int` pattern |
| **Polymorphic recursion** | :white_check_mark: | :white_check_mark: | `let rec f : type a. a t -> a = ...` |
| **Locally abstract types** | :white_check_mark: | :white_check_mark: | `(type a)` in function arguments |
| **Rigid type variables** | :white_check_mark: | :white_check_mark: | Prevent unification with concrete types |
| **GADT exhaustiveness** | :white_check_mark: | :white_check_mark: | Filters unreachable constructors |
| **Existential types** | :white_check_mark: | :white_check_mark: | Types bound in constructor args |
| **Existential escape check** | :white_check_mark: | :white_check_mark: | Prevents existentials from escaping |
| **Nested GADT patterns** | :white_check_mark: | :white_check_mark: | `Pair (Int n, Bool b)` |
| **Type equality witness** | :white_check_mark: | :white_check_mark: | `type (_, _) eq = Refl : ('a, 'a) eq` |
| **First-class existentials** | :x: | :white_check_mark: | `type any = Any : 'a -> any` (runtime packing) |

### Records

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Record types** | :white_check_mark: | :white_check_mark: | `type t = { x: int; y: int }` |
| **Record creation** | :white_check_mark: | :white_check_mark: | `{ x = 1; y = 2 }` |
| **Field access** | :white_check_mark: | :white_check_mark: | `r.x` |
| **Field punning** | :white_check_mark: | :white_check_mark: | `{ x; y }` shorthand |
| **Mutable fields** | :white_check_mark: | :white_check_mark: | `mutable x : int` |
| **Row polymorphism** | :star: | :x: | `{ x: int; .. }` - structural subtyping |
| **Nominal records** | :x: | :white_check_mark: | OCaml infers record type from field names |
| **Record update** | :x: | :white_check_mark: | `{ r with x = 1 }` |

### Polymorphic Variants

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Polymorphic variants** | :white_check_mark: | :white_check_mark: | `` `Tag`` and `` `Tag of int`` |
| **Open variants** | :white_check_mark: | :white_check_mark: | `[> \`A \| \`B]` |
| **Closed variants** | :white_check_mark: | :white_check_mark: | `[< \`A \| \`B]` |
| **Exact variants** | :white_check_mark: | :white_check_mark: | `[\`A \| \`B]` |
| **Variant subtyping** | :white_check_mark: | :white_check_mark: | Structural tag compatibility |

### Pattern Matching

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Constructor patterns** | :white_check_mark: | :white_check_mark: | `Some x`, `None` |
| **Tuple patterns** | :white_check_mark: | :white_check_mark: | `(a, b, c)` |
| **Record patterns** | :white_check_mark: | :white_check_mark: | `{ x; y }` |
| **Nested patterns** | :white_check_mark: | :white_check_mark: | `Some (x, y)` |
| **Or-patterns** | :white_check_mark: | :white_check_mark: | `A \| B -> ...` |
| **Guard clauses** | :white_check_mark: | :white_check_mark: | `when condition` |
| **Exhaustiveness checking** | :white_check_mark: | :white_check_mark: | Warns on missing cases |
| **Redundancy checking** | :white_check_mark: | :white_check_mark: | Warns on unreachable patterns |
| **GADT-aware exhaustiveness** | :white_check_mark: | :white_check_mark: | Filters by type reachability |
| **Alias patterns** | :x: | :white_check_mark: | `x as y` |
| **Lazy patterns** | :x: | :white_check_mark: | `lazy p` |
| **Exception patterns** | :x: | :white_check_mark: | `exception e` in match |

### Module System

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Structures** | :white_check_mark: | :white_check_mark: | `module M = struct ... end` |
| **Signatures** | :white_check_mark: | :white_check_mark: | `module type S = sig ... end` |
| **Abstract types** | :white_check_mark: | :white_check_mark: | `type t` in signature |
| **Module constraints** | :white_check_mark: | :white_check_mark: | `M : S` |
| **Functors** | :white_check_mark: | :white_check_mark: | `module F(X: S) = struct ... end` |
| **Functor application** | :white_check_mark: | :white_check_mark: | `F(M)` |
| **Applicative functors** | :white_check_mark: | :white_check_mark: | `F(M).t = F(M).t` |
| **Module strengthening** | :white_check_mark: | :white_check_mark: | `module N = M` preserves `N.t = M.t` |
| **With-type constraints** | :white_check_mark: | :white_check_mark: | `S with type t = int` |
| **Nested with-constraints** | :white_check_mark: | :white_check_mark: | `S with type M.t = int` |
| **Open** | :white_check_mark: | :white_check_mark: | `open M` |
| **Include** | :white_check_mark: | :white_check_mark: | `include M` |
| **Qualified access** | :white_check_mark: | :white_check_mark: | `M.x`, `M.N.x` |
| **Local modules** | :white_check_mark: | :white_check_mark: | `let module M = ... in ...` |
| **With-module constraints** | :x: | :white_check_mark: | `S with module M = N` |
| **First-class modules** | :x: | :white_check_mark: | `(module M : S)` |
| **Recursive modules** | :x: | :white_check_mark: | `module rec M : S = ...` |
| **Generative functors** | :x: | :white_check_mark: | Lina uses applicative only |
| **Private types** | :large_orange_diamond: | :white_check_mark: | Tracked but not enforced |

### Type System Internals

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Occurs check** | :white_check_mark: | :white_check_mark: | Prevents infinite types |
| **Union-find unification** | :white_check_mark: | :white_check_mark: | Efficient type variable linking |
| **Type alias expansion** | :white_check_mark: | :white_check_mark: | Transparent during unification |
| **Cyclic alias detection** | :white_check_mark: | :white_check_mark: | Prevents `type t = t` |
| **Level propagation** | :white_check_mark: | :white_check_mark: | Correct nested let handling |

### Error Detection

| Feature | Lina | OCaml | Notes |
|---------|:----:|:-----:|-------|
| **Type mismatch errors** | :white_check_mark: | :white_check_mark: | Clear error messages |
| **Occurs check errors** | :white_check_mark: | :white_check_mark: | Cycle path reporting |
| **Unbound variable errors** | :white_check_mark: | :white_check_mark: | |
| **Unbound type errors** | :white_check_mark: | :white_check_mark: | |
| **Constructor arity errors** | :white_check_mark: | :white_check_mark: | |
| **Missing record field errors** | :white_check_mark: | :white_check_mark: | When type is annotated |
| **Duplicate field errors** | :white_check_mark: | :white_check_mark: | |
| **Signature mismatch errors** | :white_check_mark: | :white_check_mark: | |
| **Abstract type escape errors** | :white_check_mark: | :white_check_mark: | |
| **Rigid unification errors** | :white_check_mark: | :white_check_mark: | Locally abstract type escape |
| **Existential escape errors** | :white_check_mark: | :white_check_mark: | GADT existential escape |

---

## Key Differences from OCaml

### Lina Extensions

1. **Row Polymorphism for Records**
   ```lina
   (* Works on any record with field 'x' *)
   let get_x r = r.x   (* type: { x: 'a; .. } -> 'a *)

   get_x { x = 1; y = 2 }       (* OK *)
   get_x { x = "hi"; z = 3.14 } (* OK *)
   ```

2. **Structural Record Typing**
   - Records are structurally typed, not nominally
   - Two records with same fields have the same type
   - Requires explicit type annotation for "missing field" errors

### OCaml Features Not in Lina

1. **Standard Library**: Lina has no standard library. Define `list`, `option`, etc. yourself.

2. **Record Update Syntax**: No `{ r with x = 1 }` syntax.

3. **First-Class Modules**: Cannot pack/unpack modules as values.

4. **Recursive Modules**: No `module rec` support.

5. **Object System**: No object-oriented features.

6. **Classes**: No class system.

7. **Exceptions**: No exception handling (use `option`/`result`).

---

## Test Coverage

| Test Suite | Tests | Status |
|------------|------:|:------:|
| Unit tests | ~200 | :white_check_mark: |
| Integration tests | 42 | :white_check_mark: |
| OCaml compatibility | 57 | :white_check_mark: |

### OCaml Compatibility Test Categories

| Category | Tests | Coverage |
|----------|------:|----------|
| Value Restriction | 10 | Syntactic values, weak types |
| Relaxed Value Restriction | 6 | Covariant generalization |
| Variance | 7 | Inference and annotations |
| GADTs | 8 | Refinement, exhaustiveness, existentials |
| Modules | 9 | Functors, strengthening, constraints |
| Locally Abstract Types | 5 | Syntax and semantics |
| Level Propagation | 4 | Nested let handling |
| Error Cases | 8 | Proper rejection of invalid code |

Run the compatibility tests:
```bash
cd test/ocaml_compatibility && ./run_comparison.sh
```

---

## Project Structure

```
lina/
├── lib/
│   ├── common/          # Shared utilities
│   ├── parsing/         # Lexer, Parser, Surface AST
│   ├── typing/          # Type inference, Module system
│   ├── lambda/          # Lambda IR, Pattern compilation
│   ├── lua/             # Lua codegen
│   ├── format/          # Code formatter
│   ├── driver/          # Compilation pipeline
│   └── lsp/             # Language Server Protocol
├── bin/                 # CLI entry points
├── test/                # Unit tests
│   ├── integration/     # Integration tests
│   └── ocaml_compatibility/  # OCaml comparison tests
```

---

## Documentation

- [Type System Internals](lib/typing/README.md) - How inference works
- [OCaml Compatibility Tests](test/ocaml_compatibility/README.md) - Test suite details
- [CLAUDE.md](CLAUDE.md) - Development guidelines

---

## References

### Type Inference
- Damas & Milner (1982). *Principal type-schemes for functional programs*
- Rémy (1992). *Extension of ML type system with a sorted equational theory*

### Value Restriction
- Wright (1995). *Simple imperative polymorphism*
- Garrigue (2004). *Relaxing the value restriction*

### GADTs
- Xi, Chen & Chen (2003). *Guarded recursive datatype constructors*
- Pottier & Régis-Gianas (2006). *Stratified type inference for generalized algebraic data types*

### Row Polymorphism
- Wand (1989). *Type inference for record concatenation and multiple inheritance*
- Rémy (1990). *Typing record concatenation for free*

### Module System
- Leroy (2000). *A modular module system*
- Rossberg, Russo & Dreyer (2014). *F-ing modules*

---

## License

[MIT](LICENSE)
