(** Embedded stdlib source files.

    This module provides compile-time embedded Lina source code for the
    standard library modules. Sources are embedded using ppx_blob to
    avoid runtime file dependencies.

    {2 Module Sources}

    Each module's source code is available as a string constant. The sources
    contain odoc-style documentation and are valid Lina code.

    {2 Dependencies}

    Modules must be loaded in dependency order:
    - [Fn], [Ord]: No dependencies (foundational)
    - [List]: No dependencies
    - [Result]: Depends on [List] (for [sequence])
    - [Option]: No dependencies
    - [Array], [Tuple], [Dict], [Set]: Depend on [List] and/or [Option]
    - [String]: Depends on [Array], [Option]
    - [Math]: No dependencies
    - [Io], [Os]: Depend on [Result]
    - [Coroutine], [Debug]: No dependencies (use runtime helpers)

    @see {!Driver.Stdlib_loader} for loading these modules into the type environment *)

(** {1 Core Modules} *)

(** Function combinators (id, const, flip, |>, >>, <<).

    Provides fundamental higher-order function utilities for function
    composition, application, and transformation. *)
val fn_source : string

(** Type-safe ordering (Less, Equal, Greater).

    Provides a three-valued ordering type as an alternative to integer
    comparison conventions (-1, 0, 1). *)
val ord_source : string

(** Immutable linked list operations.

    Provides comprehensive operations for the built-in ['a list] type
    including construction, transformation, folding, searching, and sorting. *)
val list_source : string

(** Error handling with [('a, 'e) result].

    Provides utilities for the built-in result type including constructors,
    transformations, and monadic operations. *)
val result_source : string

(** Optional value operations.

    Provides utilities for the built-in ['a option] type including
    constructors, transformations, and monadic operations. *)
val option_source : string

(** Mutable fixed-size arrays.

    Provides operations for the built-in ['a array] type, which are
    0-indexed, fixed-size, mutable sequences backed by Lua tables. *)
val array_source : string

(** Pair (2-tuple) utilities.

    Provides functions for accessing, transforming, and comparing
    pairs without requiring pattern matching. *)
val tuple_source : string

(** Immutable key-value dictionaries.

    Provides operations for the built-in [('k, 'v) dict] type.
    Keys can be any comparable type. *)
val dict_source : string

(** Immutable sets of unique values.

    Provides operations for the built-in ['a set] type including
    set operations (union, intersection, difference). *)
val set_source : string

(** {1 Utility Modules} *)

(** String manipulation (byte-based).

    Provides Lua-compatible string operations. All operations work on
    bytes, not Unicode characters. Indices are 1-based (Lua convention). *)
val string_source : string

(** Mathematical functions and constants.

    Provides bindings to Lua's math library plus additional utilities.
    All trigonometric functions work in radians. *)
val math_source : string

(** File I/O operations.

    Provides bindings to Lua's io library for file operations including
    reading, writing, and file positioning. *)
val io_source : string

(** Operating system facilities.

    Provides bindings to Lua's os library for date/time, environment,
    filesystem, and process control. Targets LuaJIT / Lua 5.1 semantics. *)
val os_source : string

(** Cooperative multitasking coroutines.

    Provides bindings to Lua's coroutine library for cooperative
    multitasking where execution can be suspended and resumed. *)
val coroutine_source : string

(** Debug introspection utilities.

    Provides bindings to Lua's debug library for stack traces,
    function introspection, and variable inspection. *)
val debug_source : string

(** {1 Runtime Support} *)

(** Lua runtime helpers for coroutine and debug support.

    This code is prepended to the stdlib prelude before any module
    definitions. It provides:
    - [_lina_coroutine_resume]: Wraps coroutine.resume for Result type
    - [_lina_call_generator]: Calls wrapped coroutine generators
    - [_lina_debug_getlocal]: Stack-frame-aware local variable access
    - [_lina_debug_getupvalue]: Upvalue access wrapper *)
val lua_runtime_helpers : string

(** {1 Module Metadata} *)

(** Stdlib module definition with name and source. *)
type stdlib_module = {
  name : string;
  source : string;
}

(** All stdlib modules in dependency order.

    Modules are ordered so that each module only depends on modules
    appearing earlier in the list. This ordering must be preserved
    when loading modules.

    The order is:
    1. Fn, Ord (foundational, no dependencies)
    2. List (collection base)
    3. Result, Option (monadic types)
    4. Array, Tuple, Dict, Set (collections, depend on List/Option)
    5. String (depends on Array)
    6. Math, Io, Os (utilities)
    7. Coroutine, Debug (runtime introspection) *)
val all_modules : stdlib_module list
