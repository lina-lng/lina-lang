(** Singleton constructor registry for code generation.

    Nullary variant constructors (like [None] in [option]) are represented
    as singleton tables in Lua to avoid allocating a new table on every use.

    This module tracks which singleton constructors have been used during
    code generation and generates the preamble statements to define them.

    {2 Usage}

    {[
      let ctx = empty in
      let ctx = register ctx "option" 0 in  (* None has tag 0 *)
      let preamble = generate_preamble ctx
      (* Produces: local _Ctor_option_0 = \{_tag = 0\} *)
    ]}

    {2 Design}

    Uses a set to track unique (type_name, tag_index) pairs, ensuring each
    singleton is declared exactly once regardless of how many times it's used.
    The context is threaded through code generation functions. *)

(** Opaque context type tracking registered singletons. *)
type t

(** Empty context with no registered singletons. *)
val empty : t

(** [register ctx type_name tag_index] registers a singleton constructor.

    @param ctx The current context
    @param type_name The name of the variant type (e.g., "option")
    @param tag_index The 0-based tag index of the constructor
    @return Updated context with the singleton registered *)
val register : t -> string -> int -> t

(** [var_name type_name tag_index] returns the Lua variable name for a singleton.

    @param type_name The name of the variant type
    @param tag_index The 0-based tag index
    @return Variable name like [_Ctor_option_0] *)
val var_name : string -> int -> Lua_ast.identifier

(** [generate_preamble ctx] generates Lua statements declaring all singletons.

    @param ctx The context with registered singletons
    @return List of [local _Ctor_X_N = \{_tag = N\}] statements *)
val generate_preamble : t -> Lua_ast.block
