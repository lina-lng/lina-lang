(** Stdlib module loader.

    Loads the Lina standard library modules and makes them available
    in the typing environment. The stdlib includes:

    - [Fn] - Function combinators (id, const, flip, |>, >>, etc.)
    - [Result] - Result utilities (map, flat_map, bind, etc.)
    - [Option] - Option utilities (map, flat_map, bind, etc.)

    Usage:
    {[
      let env = Stdlib_loader.initial_with_stdlib ()
      let ctx = Typing_context.create env
      let prelude = Stdlib_loader.stdlib_prelude ()
      (* Now user code can use: open Fn, Option.map, Result.flat_map, etc. *)
      (* Prepend prelude to generated Lua code for runtime definitions *)
    ]}
*)

(** Get the initial environment with stdlib modules loaded.

    Returns an environment containing:
    - All built-in types (int, bool, string, option, result, etc.)
    - All built-in operators (+, -, *, /, etc.)
    - Stdlib modules (Fn, Result, Option)

    This is the recommended way to get the initial environment for
    compiling user code. *)
val initial_with_stdlib : unit -> Typing.Environment.t

(** Get the Lua prelude code for stdlib modules.

    Returns Lua code that defines the stdlib modules as local tables.
    This should be prepended to any compiled Lua code that uses
    stdlib modules. *)
val stdlib_prelude : unit -> string
