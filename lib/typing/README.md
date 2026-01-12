# Lina Type System

This document explains how Lina's type system works. It's written for developers who want to understand the internals, whether to contribute, debug, or just satisfy curiosity.

## Table of Contents

1. [The Big Picture](#the-big-picture)
2. [Types and Type Variables](#types-and-type-variables)
3. [How Type Inference Works](#how-type-inference-works)
4. [The Value Restriction](#the-value-restriction)
5. [Relaxed Value Restriction](#relaxed-value-restriction)
6. [Row Polymorphism (Records)](#row-polymorphism-records)
7. [The Module System](#the-module-system)
8. [Common Gotchas](#common-gotchas)
9. [File Guide](#file-guide)

---

## The Big Picture

Lina uses **Hindley-Milner type inference** with several extensions:

- **Let-polymorphism**: Functions defined with `let` can be used at multiple types
- **Level-based generalization**: Efficient algorithm for deciding what can be polymorphic
- **Value restriction**: Prevents unsoundness with mutable references
- **Relaxed value restriction**: Makes more programs polymorphic without sacrificing safety
- **Row polymorphism**: Structural typing for records
- **ML-style modules**: Signatures, functors, and abstract types

The type checker never requires type annotations (though you can add them). It infers the most general type for every expression.

---

## Types and Type Variables

### What Types Look Like

```
int                     -- primitive
string                  -- primitive
'a                      -- type variable (polymorphic)
'a -> 'b                -- function
'a * 'b                 -- tuple
'a list                 -- type constructor application
{ x : int; y : string } -- record
```

### Type Variables Have Levels

Here's the key insight that makes inference efficient. Every type variable has a **level** that tracks when it was created:

```ocaml
type type_variable = {
  id : int;              (* unique identifier *)
  mutable level : int;   (* depth of let-binding where created *)
  mutable link : ...;    (* for unification *)
  mutable weak : bool;   (* can't be generalized *)
}
```

When you write:

```lina
let f x = x    (* level 1 *)
```

The type variable for `x` is created at level 1. When we finish inferring `f`, we **generalize** all variables whose level is greater than the surrounding context.

This is much faster than the naive approach of computing free variables in the entire environment.

### Type Schemes

A **type scheme** is a type with some variables universally quantified:

```
id : forall 'a. 'a -> 'a
```

This means "for any type `'a`, `id` has type `'a -> 'a`". Each time you use `id`, you get a fresh copy of `'a` that can be unified with different types:

```lina
let f = id 42        (* 'a = int *)
let g = id "hello"   (* 'a = string, different variable! *)
```

---

## How Type Inference Works

### The Algorithm

Lina uses a variant of **Algorithm W** (Damas-Milner 1982) with mutable type variables and level-based generalization. Here's the intuition:

1. **Variables**: Look up the type scheme, instantiate with fresh variables
2. **Functions**: Create a fresh variable for the parameter, infer the body
3. **Applications**: Infer function and argument, unify with `τ₁ → τ₂`
4. **Let bindings**: Infer the bound expression, generalize, add to environment

### Unification

When we need two types to be equal, we **unify** them:

```ocaml
let rec unify t1 t2 =
  match (representative t1, representative t2) with
  | TypeVariable v1, TypeVariable v2 when v1.id = v2.id ->
      ()  (* same variable, nothing to do *)
  | TypeVariable v, t | t, TypeVariable v ->
      occurs_check v t;  (* prevent infinite types *)
      v.link <- Some t;  (* make v point to t *)
      v.level <- min v.level (level_of t)  (* adjust level! *)
  | TypeArrow (a1, r1), TypeArrow (a2, r2) ->
      unify a1 a2; unify r1 r2
  | TypeConstructor (c1, args1), TypeConstructor (c2, args2) when c1 = c2 ->
      List.iter2 unify args1 args2
  | _ ->
      error "type mismatch"
```

The level adjustment is crucial: when we unify a variable with a type, we must lower its level to prevent incorrect generalization.

### The Occurs Check

We prevent infinite types like `'a = 'a -> 'a`:

```lina
let f x = x x   (* Error: would create infinite type *)
```

Before linking a variable to a type, we check that the variable doesn't appear in that type.

---

## The Value Restriction

### The Problem

Without restrictions, this program would crash at runtime:

```lina
let r = ref []       (* r : 'a list ref -- polymorphic reference! *)
r := [1]             (* use 'a = int *)
let s = !r           (* use 'a = string... but r contains [1]! *)
String.length s      (* CRASH: tried to get length of integer 1 *)
```

The problem is that `ref []` creates a mutable cell, but we made it polymorphic.

### The Solution

We only generalize **syntactic values** -- expressions that don't perform computation:

```lina
(* Values - can be polymorphic *)
let id = fun x -> x           (* lambda *)
let pair = (1, "hi")          (* tuple of values *)
let none = None               (* constructor *)

(* Not values - stay monomorphic *)
let r = ref []                (* application *)
let x = if b then f else g    (* conditional *)
let y = match z with ...      (* pattern match *)
```

The `is_value` function in `value_check.ml` makes this decision.

### Weak Type Variables

When we can't generalize, the type variable becomes **weak** (printed as `'_a`):

```lina
let r = ref []    (* r : '_a list ref *)
r := [1]          (* '_a = int, r : int list ref *)
r := ["hi"]       (* Error: int vs string *)
```

Weak variables can be unified but not used polymorphically.

---

## Relaxed Value Restriction

### The Insight

The standard value restriction is too conservative. Consider:

```lina
let empty = List.rev []   (* Not a value, but result is 'a list *)
```

Even though `List.rev []` is an application (not a value), the result type `'a list` only has `'a` in a **covariant position**. This matters because:

- **Covariant**: You can only *read* the type (output position)
- **Contravariant**: You can only *write* the type (input position)
- **Invariant**: Both read and write (like `ref`)

Since covariant positions can't cause the unsoundness problem (no writing), we can safely generalize them!

### How Variance Works

```
'a list        -- 'a is covariant (can only get 'a values out)
'a -> int      -- 'a is contravariant (can only put 'a values in)
'a -> 'a       -- 'a is invariant (appears in both positions)
'a ref         -- 'a is invariant (ref allows read and write)
```

The rules compose:
- Contravariant in contravariant = covariant (double negative)
- Covariant in contravariant = contravariant
- Anything combined with invariant = invariant

### The Algorithm

```ocaml
let can_generalize_relaxed var ty =
  match check_variance var ty with
  | Covariant | Bivariant -> true   (* safe to generalize *)
  | Contravariant | Invariant -> false
```

For non-values, we only generalize variables that appear only covariantly.

### Practical Effect

```lina
(* Works with relaxed VR! *)
let empty = id []               (* 'a list -- covariant *)
let xs = 1 :: empty             (* int list *)
let ys = "hi" :: empty          (* string list *)

(* Still monomorphic *)
let f = id (fun x -> x)         (* '_a -> '_a -- invariant *)
```

---

## Row Polymorphism (Records)

### The Idea

Records have structural typing via **row polymorphism**:

```lina
let get_x r = r.x

(* get_x : { x : 'a; .. } -> 'a *)
(* The ".." means "and possibly other fields" *)
```

This function works on *any* record that has an `x` field, regardless of what other fields exist.

### How It Works

A row is a list of fields plus a "tail" that's either:
- Empty (`{}` -- closed record, no other fields allowed)
- A row variable (`{ x : int; 'r }` -- open record, more fields possible)

When you access a field, we unify with an open row:

```lina
r.x
(* r must unify with { x : 'a; 'r } for some 'a and 'r *)
```

### Record Subtyping

Open records give us width subtyping:

```lina
let f r = r.x + r.y    (* needs { x : int; y : int; .. } *)

f { x = 1; y = 2 }           (* OK *)
f { x = 1; y = 2; z = 3 }    (* OK - extra field is fine *)
f { x = 1 }                   (* Error - missing y *)
```

---

## The Module System

### Structures and Signatures

Structures bundle values, types, and other modules:

```lina
module Stack = struct
  type 'a t = 'a list
  let empty = []
  let push x s = x :: s
  let pop s = match s with [] -> None | x :: xs -> Some (x, xs)
end
```

Signatures specify the interface:

```lina
module type STACK = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
end
```

### Abstract Types

When you seal a module with a signature, types can become abstract:

```lina
module S : STACK = Stack
(* S.t is abstract -- users can't see it's a list *)
```

This is the foundation of data abstraction in ML.

### Functors

Functors are functions from modules to modules:

```lina
module MakeSet (E : COMPARABLE) = struct
  type t = E.t list
  let empty = []
  let add x s = if List.mem x s then s else x :: s
end

module IntSet = MakeSet(Int)
module StringSet = MakeSet(String)
```

### Applicative Functors

Lina uses **applicative functor semantics**: applying the same functor to the same argument gives equal types:

```lina
module A = F(M)
module B = F(M)
(* A.t and B.t are the same type *)
```

This is more permissive than Standard ML's generative functors.

### Module Strengthening

When you alias a module, types are preserved:

```lina
module M = struct type t = int end
module N = M
(* N.t = M.t = int *)
```

This happens automatically through **strengthening** -- we add type equalities when binding module aliases.

---

## Common Gotchas

### 1. Constructor Type Parameters

When you write:

```lina
type 'a option = None | Some of 'a
```

The `'a` in `Some of 'a` must be the *same* variable as the parameter. This requires special handling during type declaration processing. We fixed a bug where these were accidentally different variables!

### 2. Recursive Types Need Special Care

For:

```lina
type 'a list = Nil | Cons of 'a * 'a list
```

We need the type `list` to exist in the environment *before* we parse `Cons of 'a * 'a list`. Otherwise, we'd get "unbound type: list". We solve this by adding a preliminary declaration first.

### 3. Weak Variables Aren't Errors

If you see `'_a` in a type, it just means that variable couldn't be generalized. It's not an error -- it will be resolved when you use the value:

```lina
let r = ref []    (* r : '_a list ref *)
r := [1]          (* Now r : int list ref *)
```

### 4. Variance Matters for Generalization

```lina
let f = id (fun x -> x)    (* '_a -> '_a, not polymorphic *)
let g = id []              (* 'a list, IS polymorphic *)
```

The first can't be generalized because `'a` appears both as input and output (invariant). The second can because `'a` only appears as output (covariant).

### 5. Module Type Annotations Are Restrictive

```lina
module M : sig type t end = struct type t = int end
(* M.t is now abstract -- you can't use it as int! *)
```

The signature hides the implementation. Use `with type` to expose it:

```lina
module M : sig type t end with type t = int = struct type t = int end
```

---

## File Guide

| File | What It Does |
|------|--------------|
| `types.ml` | Type representation, generalization, printing |
| `unification.ml` | Unification algorithm with occurs check |
| `environment.ml` | Typing environment (what's in scope) |
| `expression_infer.ml` | Infer types for expressions |
| `pattern_infer.ml` | Infer types for patterns |
| `structure_infer.ml` | Infer types for top-level and modules |
| `value_check.ml` | Value restriction and variance checking |
| `module_types.ml` | Module type representation |
| `module_type_check.ml` | Check module type syntax |
| `signature_match.ml` | Match modules against signatures |
| `pattern_check.ml` | Exhaustiveness and redundancy |
| `type_utils.ml` | Substitution, constructor instantiation |
| `inference.ml` | Main entry point, ties everything together |

### Entry Points

```ocaml
(* Type-check a whole program *)
Inference.infer_structure : Environment.t -> Syntax_tree.structure
                          -> Typed_tree.typed_structure * Environment.t

(* Unify two types *)
Unification.unify : Environment.t -> Location.t
                  -> Types.type_expression -> Types.type_expression -> unit

(* Check if something is a syntactic value *)
Value_check.is_value : Typed_tree.typed_expression -> bool
```

---

## Further Reading

If you want to dive deeper:

1. **"Principal type-schemes for functional programs"** (Damas & Milner, 1982) - The original Algorithm W paper

2. **"Simple imperative polymorphism"** (Wright, 1995) - The value restriction

3. **"Relaxing the value restriction"** (Garrigue, 2004) - Variance-based relaxation

4. **"How OCaml type checker works"** (Oleg Kiselyov) - Practical implementation guide

5. **OCaml source code** (`typing/` directory) - Production implementation

---

*Last updated: January 2026*
