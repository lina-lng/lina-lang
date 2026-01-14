(** Module type checking.

    This module handles the conversion of syntactic module types to semantic
    module types. It processes:
    - Module type paths ([S], [M.S])
    - Explicit signatures ([sig ... end])
    - Functor types ([functor (X : S) -> MT])
    - With-constraints ([S with type t = int])

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable].

    {2 Signatures}

    Signatures are checked item by item, converting syntactic declarations
    to semantic representations. Type expressions in signatures are also
    converted to the internal type representation.

    {2 With-Constraints}

    Both type constraints ([with type t = int]) and module constraints
    ([with module M = N]) are supported. Constraints can target nested
    paths like [with type M.N.t = int]. *)

(** {1 Module Type Checking} *)

(** [check_module_type ctx mty] checks a syntactic module type.

    Converts the parsed module type to its semantic representation,
    resolving module type paths and checking signature items.

    @param ctx The typing context
    @param mty The syntactic module type to check
    @return A pair [(module_type, updated_ctx)]
    @raise Compiler_error.Type_error on unbound module types or invalid constructs *)
val check_module_type :
  Typing_context.t ->
  Parsing.Syntax_tree.module_type ->
  Module_types.module_type * Typing_context.t

(** {1 Helper Functions} *)

(** [lookup_module_path env path_modules loc] looks up a module by path.

    @param env The typing environment
    @param path_modules List of module names forming the path
    @param loc Source location for error messages
    @return A pair [(base_binding, final_binding)] where [base_binding] is
            the top-level module and [final_binding] is the target module
    @raise Compiler_error.Type_error if module is not found *)
val lookup_module_path :
  Environment.t ->
  string list ->
  Common.Location.t ->
  Module_types.module_binding * Module_types.module_binding

(** [check_type_expression ctx ty_expr] converts a syntactic type expression
    to a semantic type.

    Used for type annotations in signatures.

    Note: This function does NOT preserve type variable sharing. Each occurrence
    of a type variable name creates a fresh variable. For type declarations with
    parameters, use [check_type_expression_with_params] instead.

    @param ctx The typing context
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)]
    @raise Compiler_error.Type_error on unbound types *)
val check_type_expression :
  Typing_context.t ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression * Typing_context.t

(** [check_type_expression_with_params ctx param_names param_vars ty_expr]
    converts a syntactic type expression to a semantic type, preserving
    type variable sharing for declared parameters.

    This is used when processing type declarations like [type 'a option = Some of 'a]
    where the ['a] in [Some of 'a] must refer to the same type variable as the
    parameter ['a].

    @param ctx The typing context
    @param param_names List of type parameter names (e.g., ["'a"; "'b"])
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic type expression
    @return A pair [(type_expr, updated_ctx)] with proper variable sharing
    @raise Compiler_error.Type_error on unbound types *)
val check_type_expression_with_params :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression * Typing_context.t

(** [check_gadt_return_type ctx param_names param_vars ty_expr]
    checks a GADT constructor return type, accumulating fresh type variables.

    This ensures that multiple occurrences of the same type variable name
    (like 'a in [('a, 'a) eq]) map to the same semantic variable, which is
    essential for GADT constructors that constrain type parameters.

    @param ctx The typing context
    @param param_names List of type parameter names from the type declaration
    @param param_vars Corresponding list of semantic type variables
    @param ty_expr The syntactic return type expression
    @return A tuple [(type_expr, updated_ctx, gadt_params)] where gadt_params
            are the fresh type variables introduced in the return type
    @raise Compiler_error.Type_error on unbound types *)
val check_gadt_return_type :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression * Typing_context.t * Types.type_variable list

(** [check_gadt_constructor ctx param_names param_vars arg_ty_expr_opt ret_ty_expr]
    checks a GADT constructor's argument and return types together, ensuring
    type variables are shared between them.

    For a constructor like [Pair : ('a expr * 'b expr) -> ('a * 'b) expr],
    this ensures that 'a and 'b refer to the same type variables in both
    the argument and return type positions.

    The function parses the return type first (accumulating fresh type variables),
    then parses the argument type using the same variable mapping.

    @param ctx The typing context
    @param param_names List of type parameter names from the type declaration
    @param param_vars Corresponding list of semantic type variables
    @param arg_ty_expr_opt Optional syntactic argument type expression
    @param ret_ty_expr The syntactic return type expression
    @return A tuple [(arg_type, ret_type, gadt_params, ctx)] where gadt_params
            are the fresh type variables introduced in the constructor
    @raise Compiler_error.Type_error on unbound types *)
val check_gadt_constructor :
  Typing_context.t ->
  string list ->
  Types.type_variable list ->
  Parsing.Syntax_tree.type_expression option ->
  Parsing.Syntax_tree.type_expression ->
  Types.type_expression option * Types.type_expression * Types.type_variable list * Typing_context.t

(** [module_path_to_internal_path base_id path_modules] converts a module path
    to an internal path representation.

    @param base_id The identifier for the base module
    @param path_modules List of module names in the path
    @return The internal path representation *)
val module_path_to_internal_path :
  Common.Identifier.t ->
  string list ->
  Types.path
