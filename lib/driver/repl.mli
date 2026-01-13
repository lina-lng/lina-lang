(** Interactive REPL for Lina.

    Provides an interactive environment for evaluating Lina expressions,
    exploring types, and experimenting with the language.

    {1 Usage}

    The REPL can be used directly, but for line editing support,
    users should wrap it with rlwrap:
    {[
      rlwrap lina repl
    ]}

    {1 Commands}

    - [:type <expr>] - Show the type of an expression without evaluating
    - [:load <file>] - Load and evaluate a Lina source file
    - [:env] - Show all bindings in the current environment
    - [:clear] - Reset the environment to initial state
    - [:help] - Show available commands
    - [:quit] or [:q] - Exit the REPL *)

(** {1 Types} *)

(** REPL state containing the current typing environment. *)
type state

(** {1 Running the REPL} *)

(** [create ()] creates a new REPL state with the initial environment. *)
val create : unit -> state

(** [run ?prompt state] starts the REPL loop with the given state.
    The default prompt is "lina> ". *)
val run : ?prompt:string -> state -> unit

(** {1 Commands} *)

(** [eval_line state line] evaluates a single line of input.
    Returns [true] if the REPL should continue, [false] to quit. *)
val eval_line : state -> string -> bool

(** [type_of state expr] returns the type of an expression as a string,
    or an error message. *)
val type_of : state -> string -> (string, string) result

(** [load_file state path] loads and evaluates a Lina source file,
    returning any output or an error message. *)
val load_file : state -> string -> (string, string) result

(** [show_env state] returns a string representation of all bindings. *)
val show_env : state -> string

(** [clear state] resets the state to the initial environment. *)
val clear : state -> unit

(** [show_help ()] returns the help text for REPL commands. *)
val show_help : unit -> string
