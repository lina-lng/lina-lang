# Lina Standard Library Implementation Plan

## Overview

Implement the Lina Standard Library with two tiers:
- **Lua.\*** — Type-safe FFI bindings to Lua's standard library (zero runtime overhead)
- **Lina.\*** — Pure Lina modules providing functional programming idioms

## Design Decisions

1. **Prelude behavior**: Lua-style — common functions available globally without imports
2. **Module access**: Nested under `Lua.*` namespace (`Lua.Math.sin`, `Lua.String.sub`)
3. **Result type**: Built-in — add to `builtins.ml` alongside `option`
4. **Priority**: Lina library first, then Lua bindings

## Directory Structure

```
lib/stdlib/
├── lua/
│   ├── basic.lina      # Lua.Basic (print, type, assert, etc.)
│   ├── string.lina     # Lua.String
│   ├── table.lina      # Lua.Table
│   ├── math.lina       # Lua.Math
│   ├── io.lina         # Lua.Io
│   ├── os.lina         # Lua.Os
│   ├── coroutine.lina  # Lua.Coroutine
│   ├── utf8.lina       # Lua.Utf8
│   ├── debug.lina      # Lua.Debug
│   └── package.lina    # Lua.Package
├── option.lina         # Option module
├── result.lina         # Result utilities
├── list.lina           # Immutable linked lists
├── array.lina          # Mutable arrays
├── dict.lina           # Key-value dictionaries
├── set.lina            # Unique value sets
├── tuple.lina          # Tuple utilities
├── fn.lina             # Function combinators
├── ord.lina            # Ordering utilities
├── char.lina           # Character utilities
└── prelude.lina        # Re-exports commonly used modules
```

---

## Implementation Checklist

### Phase 1: Foundation

- [x] **Add Result type as built-in** (`lib/typing/builtins.ml`)
  - [x] Define `type ('a, 'e) result = Ok of 'a | Error of 'e`
  - [x] Add `Ok` and `Error` constructors
  - [x] Update `environment.ml` to include Result in initial environment

- [x] **Create stdlib directory structure**
  - [x] Create `lib/stdlib/` directory
  - [ ] Create `lib/stdlib/lua/` subdirectory
  - [x] Add `lib/stdlib/dune` configuration

- [x] **Compiler integration**
  - [x] Create `lib/driver/stdlib_loader.ml` for automatic stdlib loading
  - [x] Modify `pipeline.ml` to auto-include stdlib modules
  - [x] Stdlib modules (Fn, Option, Result) available without explicit imports

### Phase 2: Lina Core Library

- [x] **Fn** (`fn.lina`) — Function combinators
  - [x] `id`, `const`
  - [x] `compose`, `compose_left`, `pipe`, `flip`
  - [x] `tap`, `negate`, `apply`, `ignore`
  - [x] Operators: `|>`, `@@`, `>>`, `<<`
  - [ ] `curry`, `uncurry` (deferred - require more complex types)

- [x] **Option** (`option.lina`) — Extend built-in option
  - [x] Constructors: `none`, `some`
  - [x] Predicates: `is_some`, `is_none`, `contains`, `for_all`, `exists`
  - [x] Extracting: `get_or`, `get_or_else`, `get_exn`, `expect`
  - [x] Transforming: `map`, `flat_map`, `bind`, `filter`, `flatten`, `join`
  - [x] Combining: `or_`, `or_else`, `and_`, `map2`, `zip`, `product`, `blend`
  - [x] Folding: `fold`, `iter`
  - [x] Comparison: `equal`, `compare`
  - [x] Conversion: `to_result`, `of_result`
  - [x] Binding operators: `let*`, `and*`, `let+`, `and+`
  - [ ] Deferred: `of_nullable`, `to_list`, `to_array` (require FFI/List)

- [x] **Result** (`result.lina`) — Utilities for Result
  - [x] Constructors: `ok`, `error`
  - [x] Querying: `is_ok`, `is_error`
  - [x] Extracting: `get_or`, `get_or_else`, `to_option`
  - [x] Transforming: `map`, `map_error`, `flat_map`, `flatten`
  - [x] Combining: `or_`, `and_`, `map2`
  - [x] Conversion: `of_option`
  - [x] Binding operators: `let*`, `and*`, `let+`, `and+`
  - [ ] Missing: `try_with`, `get_exn`, `sequence`

- [x] **List** (`list.lina`) — Immutable linked lists
  - [x] Construction: `empty`, `singleton`, `cons`, `range`, `replicate`, `init`
  - [x] Basic ops: `length`, `is_empty`, `head`, `tail`, `last`, `nth`
  - [x] Transformations: `map`, `mapi`, `filter`, `filter_map`, `reverse`, `append`, `concat`, `flat_map`
  - [x] Folding: `fold_left`, `fold_right`
  - [x] Searching: `find`, `find_index`, `exists`, `for_all`, `mem`
  - [x] Sorting: `sort`, `sort_by`, `merge`, `split_half`
  - [x] Iteration: `iter`, `iteri`
  - [x] Zipping: `zip`, `unzip`
  - [x] Comparison: `equal`, `compare`
  - [x] Additional: `take`, `drop`, `split_at`, `partition`, `intersperse`
  - [ ] Deferred: `to_array`, `of_array` (require Array module)

- [ ] **Array** (`array.lina`) — Mutable arrays
  - [ ] Core: `length`, `get`, `get_exn`, `set`, `make`, `init`, `empty`
  - [ ] Transformations: `map`, `mapi`, `filter`, `filter_map`
  - [ ] Folding: `fold_left`, `fold_right`
  - [ ] Mutation: `push`, `pop`, `reverse_in_place`, `sort_in_place`
  - [ ] Conversion: `to_list`, `of_list`

### Phase 3: Lina Extended Library

- [ ] **Tuple** (`tuple.lina`)
  - [ ] `first`, `second`
  - [ ] `swap`, `map_first`, `map_second`, `map_both`

- [ ] **Ord** (`ord.lina`)
  - [ ] Type: `type ordering = Less | Equal | Greater`
  - [ ] `compare`, `min`, `max`, `clamp`

- [ ] **Char** (`char.lina`)
  - [ ] `code`, `chr`
  - [ ] `is_digit`, `is_alpha`, `is_alnum`, `is_space`, `is_upper`, `is_lower`
  - [ ] `to_upper`, `to_lower`

- [ ] **Dict** (`dict.lina`)
  - [ ] Core: `empty`, `singleton`, `get`, `set`, `remove`, `has`, `size`
  - [ ] Accessors: `keys`, `values`, `entries`
  - [ ] Transformations: `map`, `filter`, `fold`
  - [ ] `merge`, `of_list`, `to_list`

- [ ] **Set** (`set.lina`)
  - [ ] Core: `empty`, `singleton`, `add`, `remove`, `has`, `size`
  - [ ] Set operations: `union`, `inter`, `diff`
  - [ ] `of_list`, `to_list`

### Phase 4: Lua Core Bindings

- [ ] **Lua.Basic** (`lua/basic.lina`)
  - [ ] `typeof`, `tostring`, `tonumber`
  - [ ] `print` variants
  - [ ] `assert_`, `error`
  - [ ] `pcall`, `xpcall`
  - [ ] `rawget`, `rawset`, `rawequal`, `rawlen`
  - [ ] `pairs`, `ipairs`, `next`

- [ ] **Lua.Math** (`lua/math.lina`)
  - [ ] Constants: `pi`, `huge`, `maxinteger`, `mininteger`
  - [ ] Basic: `abs`, `floor`, `ceil`, `modf`, `fmod`, `max`, `min`
  - [ ] Exponential: `exp`, `log`, `sqrt`, `pow`
  - [ ] Trigonometric: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`
  - [ ] `rad`, `deg`, `random`, `randomseed`

- [ ] **Lua.String** (`lua/string.lina`)
  - [ ] Basic: `len`, `sub`, `upper`, `lower`, `reverse`, `rep`
  - [ ] Byte ops: `byte`, `char`
  - [ ] Pattern matching: `find`, `match_`, `gmatch`, `gsub`
  - [ ] `format`, `pack`, `unpack`, `packsize`

- [ ] **Lua.Table** (`lua/table.lina`)
  - [ ] `insert`, `remove`, `move`
  - [ ] `sort`, `concat`
  - [ ] `pack`, `unpack`

### Phase 5: Lua Extended Bindings

- [ ] **Lua.Io** (`lua/io.lina`)
  - [ ] File operations: `open_`, `close`, `flush`
  - [ ] Reading: `read`, `lines`
  - [ ] Writing: `write`
  - [ ] `stdin`, `stdout`, `stderr`
  - [ ] `read_file`, `write_file`

- [ ] **Lua.Os** (`lua/os.lina`)
  - [ ] `time`, `date`, `difftime`, `clock`
  - [ ] `getenv`
  - [ ] `remove`, `rename`, `tmpname`
  - [ ] `execute`, `exit`

- [ ] **Lua.Coroutine** (`lua/coroutine.lina`)
  - [ ] `create`, `wrap`
  - [ ] `resume`, `yield`
  - [ ] `status`, `running`, `isyieldable`, `close`

- [ ] **Lua.Utf8** (`lua/utf8.lina`)
  - [ ] `charpattern`
  - [ ] `char`, `codepoint`
  - [ ] `len`, `offset`, `codes`

- [ ] **Lua.Debug** (`lua/debug.lina`)
  - [ ] `getinfo`, `traceback`
  - [ ] `getlocal`, `setlocal`
  - [ ] `getupvalue`, `setupvalue`

- [ ] **Lua.Package** (`lua/package.lina`)
  - [ ] `path`, `cpath`, `loaded`, `preload`
  - [ ] `searchpath`, `loadlib`, `require`

### Phase 6: Prelude & Integration

- [x] **Prelude** (`prelude.lina`) — Partial
  - [x] Re-export Fn module functions
  - [ ] Re-export common functions from Option, Result
  - [ ] Auto-import in all user programs

- [x] **Testing**
  - [x] Unit tests for Result in `test/test_result.ml`
  - [x] LSP completion tests include stdlib modules
  - [x] Integration tests cover stdlib functionality
  - [x] OCaml compatibility tests (187 tests passing)

- [ ] **Documentation**
  - [ ] `lib/stdlib/CLAUDE.md`
  - [x] Odoc comments on all stdlib functions

---

## Key Files Modified

| File | Changes | Status |
|------|---------|--------|
| `lib/typing/builtins.ml` | Add Result type, Ok/Error constructors, `&&`/`||` type schemes | ✅ Done |
| `lib/typing/environment.ml` | Include Result and boolean operators in initial environment | ✅ Done |
| `lib/driver/pipeline.ml` | Auto-include stdlib modules | ✅ Done |
| `lib/driver/stdlib_loader.ml` | New file: loads and prepends stdlib modules | ✅ Done |
| `lib/lambda/lambda.ml` | Add `PrimitiveBoolAnd`, `PrimitiveBoolOr` | ✅ Done |
| `lib/lua/codegen.ml` | Map boolean primitives to Lua `and`/`or` | ✅ Done |
| `lib/stdlib/dune` | Dune file for stdlib compilation | ✅ Done |
| `lib/stdlib/fn.lina` | Function combinators module | ✅ Done |
| `lib/stdlib/option.lina` | Option utilities module | ✅ Done |
| `lib/stdlib/result.lina` | Result utilities module | ✅ Done |
| `lib/stdlib/list.lina` | Immutable linked list module | ✅ Done |
| `lib/stdlib/prelude.lina` | Prelude (re-exports Fn) | ✅ Done |
| `lib/typing/cycle_check.ml` | Added cyclic type alias detection | ✅ Done |

---

## Module Dependencies

```
Fn (no deps) ✅ DONE
  ↓
Option (uses Fn) ✅ DONE
  ↓
Result (uses Option) ✅ DONE
  ↓
List (uses Option) ✅ DONE
  ↓
Array (uses Option, List) — NEXT
  ↓
Dict (uses Option, List, Array)
  ↓
Set (uses Dict)
```

Lua.* modules have no inter-dependencies.

---

## Completed Features Summary

### Phase 1 Complete
- ✅ Result type as built-in
- ✅ Stdlib directory structure
- ✅ Compiler integration (auto-loading)

### Phase 2 Complete (Core Library)
- ✅ Fn module - all combinators
- ✅ Option module - full API
- ✅ Result module - full API with binding operators
- ✅ List module - comprehensive immutable linked list operations

### Additional Features
- ✅ `&&` and `||` operators (short-circuit, OCaml-style implementation)
- ✅ Binding operators (`let*`, `and*`, `let+`, `and+`) for Option and Result
- ✅ Built-in `list` type with `Nil`/`Cons` constructors and `[]`/`::`/`[a;b;c]` syntax
- ✅ List append operator (`@`) for list concatenation
- ✅ Cyclic type alias detection for type checker
- ✅ 210 OCaml compatibility tests passing
- ✅ 67 integration tests passing
