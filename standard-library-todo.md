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

- [x] **Create stdlib directory structure** (partial)
  - [x] Create `lib/stdlib/` directory
  - [ ] Create `lib/stdlib/lua/` subdirectory
  - [ ] Add `lib/stdlib/dune` configuration

- [ ] **Compiler integration**
  - [ ] Modify driver to auto-include stdlib path
  - [ ] Add `--no-stdlib` CLI flag
  - [ ] Ensure stdlib modules available without explicit imports

### Phase 2: Lina Core Library

- [ ] **Fn** (`fn.lina`) — Function combinators
  - [ ] `id`, `const`
  - [ ] `compose`, `pipe`, `flip`
  - [ ] `tap`
  - [ ] `curry`, `uncurry`

- [ ] **Option** (`option.lina`) — Extend built-in option
  - [ ] Constructors: `none`, `some`, `of_nullable`
  - [ ] Querying: `is_some`, `is_none`, `contains`
  - [ ] Extracting: `get_or`, `get_or_else`, `get_exn`, `expect`
  - [ ] Transforming: `map`, `flat_map`, `filter`, `flatten`
  - [ ] Combining: `or_`, `or_else`, `map2`, `zip`
  - [ ] Conversion: `to_list`, `to_array`, `to_result`

- [x] **Result** (`result.lina`) — Utilities for Result
  - [x] Constructors: `ok`, `error`
  - [x] Querying: `is_ok`, `is_error`
  - [x] Extracting: `get_or`, `get_or_else`, `to_option`
  - [x] Transforming: `map`, `map_error`, `flat_map`, `flatten`
  - [x] Combining: `or_`, `and_`, `map2`
  - [x] Conversion: `of_option`
  - [x] Binding operators: `let*`, `and*`, `let+`, `and+`
  - [ ] Missing: `try_with`, `get_exn`, `sequence`

- [ ] **List** (`list.lina`) — Immutable linked lists
  - [ ] Construction: `empty`, `singleton`, `cons`, `range`, `repeat`, `init`
  - [ ] Basic ops: `length`, `is_empty`, `head`, `tail`, `last`, `nth`
  - [ ] Transformations: `map`, `mapi`, `filter`, `filter_map`, `reverse`, `append`, `concat`, `flat_map`
  - [ ] Folding: `fold_left`, `fold_right`
  - [ ] Searching: `find`, `find_index`, `exists`, `for_all`, `mem`
  - [ ] Sorting: `sort`, `sort_by`
  - [ ] Iteration: `iter`, `iteri`
  - [ ] Zipping: `zip`, `unzip`
  - [ ] Conversion: `to_array`, `of_array`

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

- [ ] **Prelude** (`prelude.lina`)
  - [ ] Auto-imported globals
  - [ ] Re-export common functions

- [ ] **Testing**
  - [ ] Unit tests in `test/test_stdlib_*.ml`
  - [ ] Integration tests in `test/integration/stdlib/`

- [ ] **Documentation**
  - [ ] `lib/stdlib/CLAUDE.md`
  - [ ] Odoc comments on all functions

---

## Key Files to Modify

| File | Changes |
|------|---------|
| `lib/typing/builtins.ml` | Add Result type and Ok/Error constructors |
| `lib/typing/environment.ml` | Include Result in initial environment |
| `lib/driver/compile.ml` | Add stdlib path to module search |
| `bin/main.ml` | Add `--no-stdlib` CLI flag |
| `dune` (project root) | Include lib/stdlib in build |
| `lib/stdlib/dune` | New dune file for stdlib |

---

## Module Dependencies

```
Fn (no deps)
  ↓
Option (uses Fn)
  ↓
Result (uses Option)
  ↓
List (uses Option, Result)
  ↓
Array (uses Option, List)
  ↓
Dict (uses Option, List, Array)
  ↓
Set (uses Dict)
```

Lua.* modules have no inter-dependencies.
