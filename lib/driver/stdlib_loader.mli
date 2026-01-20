(** Stdlib module loader.

    Loads the Lina standard library modules and makes them available
    in the typing environment. The stdlib provides 15 modules:

    {2 Core Modules}

    - {b Fn} - Function combinators (id, const, flip, |>, >>, <<)
    - {b Ord} - Type-safe ordering (Less, Equal, Greater instead of int)
    - {b List} - Immutable linked lists (map, filter, fold, sort)
    - {b Result} - Error handling with [('a, 'e) result = Ok of 'a | Error of 'e]
    - {b Option} - Optional values with ['a option = None | Some of 'a]
    - {b Array} - Mutable fixed-size arrays
    - {b Tuple} - Pair utilities (fst, snd, swap, map)
    - {b Dict} - Immutable key-value dictionaries
    - {b Set} - Immutable sets of unique values

    {2 Utility Modules}

    - {b String} - String manipulation (byte-based, like Lua)
    - {b Math} - Mathematical functions (trig, rounding, random)
    - {b Io} - File I/O (open, read, write, with_file)
    - {b Os} - OS facilities (time, date, getenv, execute)
    - {b Coroutine} - Cooperative multitasking (create, resume, yield)
    - {b Debug} - Stack introspection (traceback, getinfo)

    {2 Usage}

    {[
      let env = Stdlib_loader.initial_with_stdlib ()
      let ctx = Typing_context.create env
      let prelude = Stdlib_loader.stdlib_prelude ()
      (* User code can use: open List, Option.map, Result.flat_map, etc. *)
      (* Prepend prelude to generated Lua code for runtime definitions *)
    ]}

    {2 Implementation}

    Stdlib sources are embedded at compile time using ppx_blob, ensuring
    zero runtime file dependencies. The sources live in separate [.lina]
    files under [lib/stdlib/modules/] for better maintainability.

    @see <Lina_stdlib.Stdlib_sources> for the embedded source files *)

(** Get the initial environment with stdlib modules loaded.

    Returns an environment containing:
    - Built-in types (int, bool, string, option, result, list, array, dict, set)
    - Built-in operators (+, -, *, /, ^, @, &&, ||, etc.)
    - All 15 stdlib modules listed above

    This is the recommended way to get the initial environment for
    compiling user code.

    @see {!stdlib_prelude} for the corresponding Lua runtime definitions *)
val initial_with_stdlib : unit -> Typing.Environment.t

(** Get the Lua prelude code for stdlib modules.

    Returns Lua code that defines the stdlib modules as local tables.
    This should be prepended to any compiled Lua code that uses
    stdlib modules.

    The prelude includes:
    - Runtime helpers for coroutine resume/yield wrapping
    - Runtime helpers for debug stack introspection
    - All 15 stdlib modules wrapped as local tables via IIFE pattern

    @see {!initial_with_stdlib} for the corresponding type environment *)
val stdlib_prelude : unit -> string
