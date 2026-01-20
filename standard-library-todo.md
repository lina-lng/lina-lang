# Lina Standard Library Implementation Plan

## Overview

A **unified standard library** that provides:
- Pure Lina modules for functional programming idioms (Option, Result, List, etc.)
- Zero-cost FFI bindings to Lua's standard library (String, Math, IO, etc.)
- Seamless interop with any Lua package

**Key Principle:** Lina types ARE Lua types at runtime with zero overhead:
- `string` → Lua string
- `int/float` → Lua number
- `array` → Lua table (1-indexed)
- `dict` → Lua table
- `record` → Lua table

## Design Decisions

1. **Unified namespace**: No separate `Lua.*` modules - one `String` module, one `Math` module, etc.
2. **Zero-cost FFI**: Direct bindings to Lua functions via `@val @scope("module")` attributes
3. **Type safety**: Lina's type system ensures safe usage of Lua functions
4. **Seamless interop**: Any Lua package can be used with minimal FFI declarations
5. **UTF-8 by default**: String operations work on characters, with explicit `*_bytes` variants for byte-level access
6. **No duplication**: Use existing Lina functions where possible (e.g., `Ord.string_compare` instead of `String.compare`)

## Directory Structure

```
lib/stdlib/
├── fn.lina             # Function combinators
├── option.lina         # Optional values
├── result.lina         # Error handling
├── list.lina           # Immutable linked lists
├── array.lina          # Mutable arrays
├── dict.lina           # Key-value dictionaries
├── set.lina            # Unique value sets
├── tuple.lina          # Tuple utilities
├── ord.lina            # Ordering utilities
├── string.lina         # String manipulation (UTF-8 by default)
├── math.lina           # Mathematical functions
├── io.lina             # File I/O operations
├── os.lina             # Operating system facilities
├── coroutine.lina      # Coroutines (optional/advanced)
├── debug.lina          # Debug utilities (optional/advanced)
└── prelude.lina        # Common re-exports
```

---

## Implementation Checklist

### Phase 1: Foundation (COMPLETE)

- [x] **Result type as built-in** (`lib/typing/builtins.ml`)
- [x] **Stdlib directory structure**
- [x] **Compiler integration** - auto-loading via `stdlib_loader.ml`
- [x] **FFI system** - `external` declarations with attributes

### Phase 2: Core Data Types (COMPLETE)

- [x] **Fn** (`fn.lina`) — Function combinators
  - [x] `id`, `const`, `flip`
  - [x] `compose`, `compose_left`, `pipe`
  - [x] `tap`, `negate`, `apply`, `ignore`
  - [x] Operators: `|>`, `@@`, `>>`, `<<`
  - [x] `curry`, `uncurry`

- [x] **Option** (`option.lina`) — Optional values
  - [x] Constructors: `none`, `some`
  - [x] Predicates: `is_some`, `is_none`, `contains`, `for_all`, `exists`
  - [x] Extracting: `get_or`, `get_or_else`, `get_exn`, `expect`
  - [x] Transforming: `map`, `flat_map`, `bind`, `filter`, `flatten`, `join`
  - [x] Combining: `or_`, `or_else`, `and_`, `map2`, `zip`, `product`, `blend`
  - [x] Folding: `fold`, `iter`
  - [x] Comparison: `equal`, `compare`
  - [x] Conversion: `to_result`, `of_result`, `to_list`, `to_array`
  - [x] Binding operators: `let*`, `and*`, `let+`, `and+`
  - [ ] `of_nullable` (requires FFI)

- [x] **Result** (`result.lina`) — Error handling
  - [x] Constructors: `ok`, `error`
  - [x] Querying: `is_ok`, `is_error`
  - [x] Extracting: `get_or`, `get_or_else`, `get_exn`, `to_option`
  - [x] Transforming: `map`, `map_error`, `flat_map`, `flatten`
  - [x] Combining: `or_`, `and_`, `map2`
  - [x] Conversion: `of_option`
  - [x] Sequencing: `sequence`
  - [x] Binding operators: `let*`, `and*`, `let+`, `and+`
  - [ ] `try_with` (requires FFI pcall)

- [x] **List** (`list.lina`) — Immutable linked lists
  - [x] Construction: `empty`, `singleton`, `cons`, `range`, `replicate`, `init`
  - [x] Basic: `length`, `is_empty`, `head`, `tail`, `last`, `nth`
  - [x] Transform: `map`, `mapi`, `filter`, `filter_map`, `reverse`, `append`, `concat`, `flat_map`
  - [x] Fold: `fold_left`, `fold_right`
  - [x] Search: `find`, `find_index`, `exists`, `for_all`, `mem`
  - [x] Sort: `sort`, `sort_by`, `merge`, `split_half`
  - [x] Iterate: `iter`, `iteri`
  - [x] Zip: `zip`, `unzip`
  - [x] Compare: `equal`, `compare`
  - [x] Utils: `take`, `drop`, `split_at`, `partition`, `intersperse`
  - [x] Convert: `to_array`, `of_array`

- [x] **Array** (`array.lina`) — Mutable arrays
  - [x] Core: `make`, `init`, `empty`, `length`, `is_empty`, `get`, `get_exn`, `set`, `set_exn`
  - [x] Transform: `map`, `mapi`, `copy`
  - [x] Fold: `fold_left`, `fold_right`
  - [x] Iterate: `iter`, `iteri`
  - [x] Search: `exists`, `for_all`, `find`, `find_index`, `mem`
  - [x] Filter: `filter`, `filter_map`
  - [x] Convert: `of_list`, `to_list`
  - [x] Compare: `compare`, `equal`
  - [x] Stack: `push`, `pop` (functional, return new arrays)
  - [x] In-place: `reverse_in_place`, `sort_in_place`
  - [ ] In-place insert/remove: `insert_in_place`, `remove_in_place` (from Table module)

### Phase 3: Extended Data Types (COMPLETE)

- [x] **Tuple** (`tuple.lina`)
  - [x] `make`, `fst`, `snd`
  - [x] `swap`, `map_fst`, `map_snd`, `map`
  - [x] `fold`, `iter`
  - [x] `equal`, `compare`
  - [x] `to_list`

- [x] **Ord** (`ord.lina`)
  - [x] Type: `type ordering = Less | Equal | Greater`
  - [x] Constructors: `less`, `equal_ordering`, `greater`
  - [x] Conversion: `of_int`, `to_int`
  - [x] Predicates: `is_less`, `is_equal`, `is_greater`
  - [x] Combinators: `flip`, `then_`
  - [x] Helpers: `int_compare`, `bool_compare`, `string_compare`
  - [x] Self: `compare`, `equal`
  - [x] Min/Max: `min`, `max`, `clamp`

- [x] **Dict** (`dict.lina`) — Immutable dictionaries
  - [x] Core: `empty`, `singleton`, `get`, `get_or`, `set`, `remove`, `has`, `size`, `is_empty`
  - [x] Access: `keys`, `values`, `entries`
  - [x] Transform: `map`, `mapi`, `filter`, `filter_map`, `fold`
  - [x] Iterate: `iter`
  - [x] Merge: `merge`
  - [x] Convert: `of_list`, `to_list`
  - [x] Compare: `equal`
  - [x] Search: `find`, `exists`, `for_all`

- [x] **Set** (`set.lina`) — Immutable sets
  - [x] Core: `empty`, `singleton`, `add`, `remove`, `mem`, `has`, `size`, `is_empty`
  - [x] Ops: `union`, `inter`, `diff`, `sym_diff`, `subset`, `disjoint`
  - [x] Transform: `map`, `filter`, `filter_map`, `partition`
  - [x] Fold: `fold`, `iter`
  - [x] Predicate: `exists`, `for_all`
  - [x] Search: `find`
  - [x] Convert: `elements`, `to_list`, `of_list`
  - [x] Compare: `equal`, `compare`

### Phase 4: String Module (Byte-Based, Like Lua) — COMPLETE

- [x] **String** (`string.lina`) — String manipulation (byte-based)
  - [x] **Basic Operations**
    - [x] `length : string -> int` — Byte count
    - [x] `is_empty : string -> bool` — Empty check
    - [x] `sub : string -> int -> int -> string` — Substring by byte index (1-based, supports negative)
    - [x] `get : string -> int -> int option` — Get byte at position
    - [x] `get_exn : string -> int -> int` — Get byte or raise
  - [x] **Case Conversion** (ASCII only)
    - [x] `upper : string -> string` — Convert to uppercase
    - [x] `lower : string -> string` — Convert to lowercase
    - [x] `capitalize : string -> string` — Capitalize first character
    - [x] `uncapitalize : string -> string` — Uncapitalize first character
  - [x] **Building**
    - [x] `rep : string -> int -> string` — Repeat string n times
    - [x] `make : int -> int -> string` — Create string of n copies of byte
    - [x] `join : string -> string list -> string` — Join with separator
    - [x] `concat : string -> string -> string` — Concatenate two strings
    - [x] `reverse : string -> string` — Reverse string (by bytes)
  - [x] **Searching** (Lua patterns)
    - [x] `find : string -> string -> bool` — Pattern found check
    - [x] `contains : string -> string -> bool` — Literal substring check
    - [x] `match_ : string -> string -> string option` — Match pattern
    - [x] `gsub : string -> string -> string -> string` — Global substitution
  - [x] **Predicates**
    - [x] `starts_with : string -> string -> bool` — Prefix check
    - [x] `ends_with : string -> string -> bool` — Suffix check
  - [x] **Trimming**
    - [x] `trim : string -> string` — Remove leading/trailing whitespace
    - [x] `trim_start : string -> string` — Remove leading whitespace
    - [x] `trim_end : string -> string` — Remove trailing whitespace
  - [x] **Splitting**
    - [x] `split : string -> string -> string list` — Split by literal separator
    - [x] `lines : string -> string list` — Split by newline
  - [x] **Byte Conversion**
    - [x] `to_bytes : string -> int list` — String to byte list
    - [x] `of_bytes : int list -> string` — Byte list to string
    - [x] `of_byte : int -> string` — Single byte to string
  - [x] **Comparison**
    - [x] `compare : string -> string -> int` — Lexicographic comparison
    - [x] `equal : string -> string -> bool` — Equality test
  - [x] **Iteration**
    - [x] `iter : (int -> unit) -> string -> unit` — Apply to each byte
    - [x] `iteri : (int -> int -> unit) -> string -> unit` — Apply with index
    - [x] `fold_left : ('a -> int -> 'a) -> 'a -> string -> 'a` — Fold over bytes
    - [x] `for_all : (int -> bool) -> string -> bool` — All bytes satisfy predicate
    - [x] `exists : (int -> bool) -> string -> bool` — Any byte satisfies predicate
  - Note: Use `Ord.string_compare` for ordering type result

### Phase 5: Math Module (COMPLETE)

- [x] **Math** (`math.lina`) — Mathematical functions
  - [x] **Constants**
    - [x] `pi : float` — π (3.14159...)
    - [x] `huge : float` — Positive infinity
    - Note: `max_integer`/`min_integer` skipped (Lua 5.3+ only, not in LuaJIT)
  - [x] **Rounding**
    - [x] `floor : float -> float` — Round down
    - [x] `ceil : float -> float` — Round up
    - [x] `round : float -> float` — Round to nearest (half away from zero)
    - [x] `trunc : float -> float` — Truncate toward zero
  - [x] **Arithmetic**
    - [x] `abs : float -> float` — Absolute value (float)
    - [x] `abs_int : int -> int` — Absolute value (int)
    - [x] `fmod : float -> float -> float` — Floating modulo
    - [x] `modf : float -> (float * float)` — Integer and fractional parts
    - [x] `min : float -> float -> float` — Minimum of two floats
    - [x] `max : float -> float -> float` — Maximum of two floats
    - [x] `min_int : int -> int -> int` — Minimum of two ints
    - [x] `max_int : int -> int -> int` — Maximum of two ints
  - [x] **Exponential & Logarithmic**
    - [x] `exp : float -> float` — e^x
    - [x] `log : float -> float` — Natural logarithm
    - [x] `log10 : float -> float` — Base-10 logarithm
    - [x] `sqrt : float -> float` — Square root
    - [x] `pow : float -> float -> float` — Power
  - [x] **Trigonometric**
    - [x] `sin : float -> float` — Sine
    - [x] `cos : float -> float` — Cosine
    - [x] `tan : float -> float` — Tangent
    - [x] `asin : float -> float` — Arc sine
    - [x] `acos : float -> float` — Arc cosine
    - [x] `atan : float -> float` — Arc tangent
    - [x] `atan2 : float -> float -> float` — Two-argument arc tangent
  - [x] **Angle Conversion**
    - [x] `rad : float -> float` — Degrees to radians
    - [x] `deg : float -> float` — Radians to degrees
  - [x] **Random Numbers**
    - [x] `random : unit -> float` — Random float [0, 1)
    - [x] `random_int : int -> int` — Random int [1, n]
    - [x] `random_range : int -> int -> int` — Random int [m, n]
    - [x] `randomseed : int -> unit` — Set random seed

### Phase 6: IO Module (File Operations)

- [ ] **Io** (`io.lina`) — File I/O operations
  - [ ] **Types**
    - [ ] `type file` — File handle (abstract)
  - [ ] **Standard Streams**
    - [ ] `stdin : file` — Standard input
    - [ ] `stdout : file` — Standard output
    - [ ] `stderr : file` — Standard error
  - [ ] **File Operations**
    - [ ] `open_ : string -> string -> file option` — Open file (path, mode)
    - [ ] `open_exn : string -> string -> file` — Open or raise
    - [ ] `close : file -> unit` — Close file
    - [ ] `flush : file -> unit` — Flush buffer
  - [ ] **Reading**
    - [ ] `read_line : file -> string option` — Read line
    - [ ] `read_all : file -> string` — Read entire file
    - [ ] `read_bytes : file -> int -> string option` — Read n bytes
  - [ ] **Writing**
    - [ ] `write : file -> string -> unit` — Write string
    - [ ] `write_line : file -> string -> unit` — Write string with newline
  - [ ] **Positioning**
    - [ ] `seek : file -> string -> int -> int option` — Seek position
    - [ ] `tell : file -> int` — Get current position
  - [ ] **Convenience Functions** (pure Lina wrappers)
    - [ ] `read_file : string -> (string, string) result` — Read entire file by path
    - [ ] `write_file : string -> string -> (unit, string) result` — Write string to file
    - [ ] `append_file : string -> string -> (unit, string) result` — Append to file
    - [ ] `with_file : string -> string -> (file -> 'a) -> ('a, string) result` — Open, use, close
  - Note: `print` and `error` are already builtins

### Phase 7: OS Module

- [ ] **Os** (`os.lina`) — Operating system facilities
  - [ ] **Date and Time**
    - [ ] `time : unit -> int` — Current Unix timestamp
    - [ ] `clock : unit -> float` — CPU time used
    - [ ] `difftime : int -> int -> int` — Time difference in seconds
    - [ ] `date : string -> string` — Format current time
    - [ ] `date_of : string -> int -> string` — Format given timestamp
  - [ ] **Environment**
    - [ ] `getenv : string -> string option` — Get environment variable
  - [ ] **File System**
    - [ ] `remove : string -> (unit, string) result` — Delete file
    - [ ] `rename : string -> string -> (unit, string) result` — Rename file
    - [ ] `tmpname : unit -> string` — Generate temporary filename
  - [ ] **Process Control**
    - [ ] `execute : string -> int` — Execute shell command, return exit code
    - [ ] `exit : int -> unit` — Exit program with code

### Phase 8: Coroutine Module (Optional/Advanced)

- [ ] **Coroutine** (`coroutine.lina`) — Cooperative multitasking
  - [ ] **Types**
    - [ ] `type 'a thread` — Coroutine handle
    - [ ] `type status = Running | Suspended | Normal | Dead`
  - [ ] **Creation**
    - [ ] `create : (unit -> 'a) -> 'a thread` — Create coroutine
    - [ ] `wrap : (unit -> 'a) -> (unit -> 'a)` — Create as callable function
  - [ ] **Control**
    - [ ] `resume : 'a thread -> ('a, string) result` — Resume execution
    - [ ] `yield : 'a -> 'a` — Yield value
  - [ ] **Inspection**
    - [ ] `status : 'a thread -> status` — Get coroutine status
    - [ ] `running : unit -> 'a thread option` — Get current coroutine
    - [ ] `is_yieldable : unit -> bool` — Can yield from current context

### Phase 9: Debug Module (Optional/Advanced)

- [ ] **Debug** (`debug.lina`) — Debug utilities
  - [ ] **Introspection**
    - [ ] `traceback : unit -> string` — Get stack traceback
    - [ ] `traceback_from : int -> string` — Traceback from level
  - [ ] **Advanced** (use with caution)
    - [ ] `getinfo : int -> { ... } option` — Get function info at stack level
    - [ ] `getlocal : int -> int -> (string * 'a) option` — Get local variable
    - [ ] `getupvalue : ('a -> 'b) -> int -> (string * 'c) option` — Get upvalue

### Phase 10: FFI Utilities

- [ ] **Update Option module**
  - [ ] `of_nullable : 'a -> 'a option` — Convert Lua nil to None

- [ ] **Update Result module**
  - [ ] `try_with : (unit -> 'a) -> ('a, string) result` — Catch errors via pcall

### Phase 11: Prelude

- [ ] **Prelude** (`prelude.lina`) — Common re-exports
  - [ ] Re-export Fn operators: `|>`, `@@`, `>>`, `<<`
  - [ ] Re-export common Option functions
  - [ ] Re-export common Result functions
  - [ ] Auto-import in all user programs

---

## Testing Checklist

- [x] Unit tests for String module (byte operations)
- [x] Unit tests for Math module
- [ ] Unit tests for IO module (file operations)
- [ ] Unit tests for OS module
- [ ] Integration tests with Lua packages (verify zero-cost FFI)
- [ ] Performance tests vs raw Lua (ensure no overhead)

---

## Key Files

| File | Status | Description |
|------|--------|-------------|
| `lib/stdlib/fn.lina` | ✅ Complete | Function combinators |
| `lib/stdlib/option.lina` | ✅ Complete | Optional values (needs `of_nullable`) |
| `lib/stdlib/result.lina` | ✅ Complete | Error handling (needs `try_with`) |
| `lib/stdlib/list.lina` | ✅ Complete | Immutable lists |
| `lib/stdlib/array.lina` | ✅ Complete | Mutable arrays (needs `insert/remove_in_place`) |
| `lib/stdlib/dict.lina` | ✅ Complete | Dictionaries |
| `lib/stdlib/set.lina` | ✅ Complete | Sets |
| `lib/stdlib/tuple.lina` | ✅ Complete | Tuples |
| `lib/stdlib/ord.lina` | ✅ Complete | Ordering |
| `lib/stdlib/string.lina` | ✅ Complete | String manipulation (byte-based) |
| `lib/stdlib/math.lina` | ✅ Complete | Math functions |
| `lib/stdlib/io.lina` | ⬜ TODO | File I/O |
| `lib/stdlib/os.lina` | ⬜ TODO | OS facilities |
| `lib/stdlib/coroutine.lina` | ⬜ Optional | Coroutines |
| `lib/stdlib/debug.lina` | ⬜ Optional | Debug utilities |
| `lib/stdlib/prelude.lina` | 🔄 Partial | Re-exports |

---

## Zero-Cost FFI Pattern

All Lua bindings use this pattern for zero overhead:

```lina
@val @scope("math")
external sin : float -> float = "sin"
```

Generates direct Lua call:
```lua
math.sin(x)
```

No wrapper functions, no type conversions, no runtime overhead.

---

## Removed/Merged Modules

| Original | Decision | Reason |
|----------|----------|--------|
| Table module | **Merged** | `join` → String, `insert/remove_in_place` → Array |
| UTF-8 module | **Merged** | String is UTF-8 by default, byte ops are explicit |
| Package module | **Skipped** | FFI with `@module("name")` handles `require()` |

## Duplicates Avoided

| Function | Use Instead |
|----------|-------------|
| `String.compare` | `Ord.string_compare` |
| `Io.print` | Built-in `print` |
| `Io.eprint` | Built-in `error` or write to `Io.stderr` |
