(** Shared constructor type processing for variant declarations.

    This module provides a shared implementation for processing constructor
    declarations, including GADT constructors with refined return types.
    Used by both structure inference and signature type checking. *)

(** Process a list of variant constructors, handling both regular and GADT constructors.

    @param ctx Typing context with the type already added to the environment
    @param param_names Names of the type's parameters (e.g., ["a"; "b"] for ['a, 'b] t)
    @param type_params Type variables corresponding to the parameters
    @param result_type Default result type for non-GADT constructors (e.g., int option)
    @param type_name Name of the type being declared (for constructor_type_name field)
    @param constructors List of constructor declarations from the parser
    @return Constructor info list and updated context *)
val check_constructors :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Types.type_expression ->
  string ->
  Parsing.Syntax_tree.constructor_declaration list ->
  Types.constructor_info list * Typing_context.t
