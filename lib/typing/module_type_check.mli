(** Module type checking.

    This module handles the conversion of syntactic module types to semantic
    module types. It processes:
    - Module type paths ([S], [M.S])
    - Explicit signatures ([sig ... end])
    - Functor types ([functor (X : S) -> MT])
    - With-constraints ([S with type t = int])

    Type expression checking has been moved to {!Type_expression_check}.

    {2 Signatures}

    Signatures are checked item by item, converting syntactic declarations
    to semantic representations.

    {2 With-Constraints}

    Both type constraints ([with type t = int]) and module constraints
    ([with module M = N]) are supported. Constraints can target nested
    paths like [with type M.N.t = int]. *)

(** {1 Forward References} *)

(** Forward reference for module expression inference.
    Set by Structure_infer to break circular dependency.
    Used for [module type of M] to get the signature of a module expression. *)
val infer_module_expression_ref :
  (Typing_context.t ->
   Parsing.Syntax_tree.module_expression ->
   Typed_tree.typed_module_expression * Typing_context.t) ref

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

(** [module_path_to_internal_path base_id path_modules] converts a module path
    to an internal path representation.

    @param base_id The identifier for the base module
    @param path_modules List of module names in the path
    @return The internal path representation *)
val module_path_to_internal_path :
  Common.Identifier.t ->
  string list ->
  Types.path
