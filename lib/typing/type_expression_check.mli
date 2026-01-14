(** Type expression checking.

    Converts syntactic type expressions to semantic types with proper
    type variable sharing for type parameters.

    {2 Type Variable Sharing}

    Type declarations like [type 'a option = None | Some of 'a] require
    that the ['a] in [Some of 'a] refers to the same type variable as
    the parameter ['a]. Use {!check_type_expression_with_params} for this.

    {2 GADT Support}

    GADT constructors may introduce fresh type variables in their return
    types. Use {!check_gadt_constructor} to handle both argument and
    return types with proper variable sharing. *)

(** [check_type_expression ctx ty_expr] converts a syntactic type expression
    to a semantic type.

    Note: This does NOT preserve type variable sharing. For type declarations
    with parameters, use {!check_type_expression_with_params}.

    @param ctx The typing context
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
val check_type_expression :
  Typing_context.t ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression * Typing_context.t

(** [check_type_expression_with_params ctx param_names param_vars ty_expr]
    converts a syntactic type expression using a provided parameter mapping.

    This preserves sharing: multiple occurrences of the same parameter name
    map to the same semantic type variable.

    @param ctx The typing context
    @param param_names List of type parameter names (e.g., ["'a"; "'b"])
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] *)
val check_type_expression_with_params :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression * Typing_context.t

(** [check_gadt_return_type ctx param_names param_vars ty_expr] checks a GADT
    constructor return type, accumulating fresh type variables.

    This ensures that multiple occurrences of the same type variable name
    (like 'a in ('a, 'a) eq) map to the same semantic variable.

    @param ctx The typing context
    @param param_names List of type parameter names from the type declaration
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic return type expression
    @return A tuple [(type_expr, updated_ctx, gadt_params)] where gadt_params
            are the fresh type variables introduced in the return type *)
val check_gadt_return_type :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression * Typing_context.t * Types.type_variable list

(** [check_gadt_constructor ctx param_names param_vars arg_ty_expr_opt ret_ty_expr]
    checks both argument and return types for a GADT constructor.

    For a constructor like [Pair : ('a expr * 'b expr) -> ('a * 'b) expr],
    this ensures 'a and 'b refer to the same type variables in both positions.

    The function parses the return type first (accumulating fresh type variables),
    then parses the argument type using the same variable mapping.

    @param ctx The typing context
    @param param_names List of type parameter names from the type declaration
    @param param_vars Corresponding list of semantic type variables
    @param arg_ty_expr_opt Optional syntactic argument type expression
    @param ret_ty_expr The syntactic return type expression
    @return A tuple [(arg_type, ret_type, gadt_params, ctx)] where gadt_params
            are the fresh type variables introduced *)
val check_gadt_constructor :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Parsing.Syntax_tree.type_expression option ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression option * Types.type_expression * Types.type_variable list * Typing_context.t
