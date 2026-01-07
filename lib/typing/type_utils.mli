(** Type manipulation utilities.

    This module provides common operations on types that are used across
    the type inference and module system. It consolidates duplicated code
    for type parameter substitution and path substitution.

    {2 Row Utilities}

    Helper functions for manipulating row types in records.

    {2 Type Parameter Substitution}

    Used for instantiating polymorphic types and expanding type aliases.
    Given type parameters and arguments, substitute occurrences in a type body.

    {2 Path Substitution}

    Used when applying functors to replace parameter paths with argument paths.
    For example, when applying [F(M)], occurrences of the parameter path
    are replaced with [M]'s path. *)

(** {1 Row Utilities} *)

(** [map_row_types f row] applies [f] to each type within a row (shallow).

    Unlike [Type_traversal.map_row], this does not recurse into sub-types.
    It applies [f] directly to each field type and the row tail.

    @param f The transformation to apply to each type
    @param row The row to transform
    @return A new row with transformed types *)
val map_row_types :
  (Types.type_expression -> Types.type_expression) ->
  Types.row ->
  Types.row

(** {1 Type Parameter Substitution} *)

(** [substitute_type_params params args body] substitutes type arguments for
    type parameters in a type body.

    Given type parameters [\['a; 'b\]] and arguments [[int; string]],
    replaces all occurrences of ['a] with [int] and ['b] with [string]
    in the body type.

    @param params The type parameter variables to substitute
    @param args The type arguments to substitute in
    @param body The type expression containing parameters
    @return The type with parameters replaced by arguments *)
val substitute_type_params :
  Types.type_variable list ->
  Types.type_expression list ->
  Types.type_expression ->
  Types.type_expression

(** {1 Constructor Instantiation} *)

(** [instantiate_constructor ctor] creates fresh type variables for a
    constructor's type parameters and returns the instantiated argument
    and result types.

    For a constructor like [Some : 'a -> 'a option], this creates a fresh
    type variable ['t0] and returns [Some 't0] as argument type and
    ['t0 option] as result type.

    @param ctor The constructor info to instantiate
    @return A pair [(arg_type_opt, result_type)] with fresh type variables *)
val instantiate_constructor :
  Types.constructor_info ->
  Types.type_expression option * Types.type_expression

(** {1 Path Substitution} *)

(** [substitute_path_prefix ~old_path ~new_path path] replaces [old_path]
    prefix with [new_path] in a path.

    If [path] equals [old_path], returns [new_path].
    If [path] is [old_path.X], returns [new_path.X].

    @param old_path The path prefix to replace
    @param new_path The replacement path
    @param path The path to transform
    @return The path with prefix substituted *)
val substitute_path_prefix :
  old_path:Types.path ->
  new_path:Types.path ->
  Types.path ->
  Types.path

(** [substitute_path_in_type ~old_path ~new_path ty] replaces path prefixes
    in type constructors within a type expression.

    @param old_path The path to replace
    @param new_path The replacement path
    @param ty The type expression to transform
    @return The type with paths substituted *)
val substitute_path_in_type :
  old_path:Types.path ->
  new_path:Types.path ->
  Types.type_expression ->
  Types.type_expression

(** [substitute_path_in_scheme ~old_path ~new_path scheme] replaces paths
    in the body of a type scheme.

    @param old_path The path to replace
    @param new_path The replacement path
    @param scheme The type scheme to transform
    @return The scheme with paths substituted *)
val substitute_path_in_scheme :
  old_path:Types.path ->
  new_path:Types.path ->
  Types.type_scheme ->
  Types.type_scheme

(** [substitute_path_in_type_decl ~old_path ~new_path decl] replaces paths
    in a type declaration's manifest type.

    @param old_path The path to replace
    @param new_path The replacement path
    @param decl The type declaration to transform
    @return The declaration with paths substituted *)
val substitute_path_in_type_decl :
  old_path:Types.path ->
  new_path:Types.path ->
  Types.type_declaration ->
  Types.type_declaration

(** [substitute_path_in_signature ~old_path ~new_path sig_] replaces paths
    in all items of a signature.

    @param old_path The path to replace
    @param new_path The replacement path
    @param sig_ The signature to transform
    @return The signature with paths substituted *)
val substitute_path_in_signature :
  old_path:Types.path ->
  new_path:Types.path ->
  Module_types.signature ->
  Module_types.signature

(** [substitute_path_in_module_type ~old_path ~new_path mty] replaces paths
    in a module type (signature, functor, or identifier).

    @param old_path The path to replace
    @param new_path The replacement path
    @param mty The module type to transform
    @return The module type with paths substituted *)
val substitute_path_in_module_type :
  old_path:Types.path ->
  new_path:Types.path ->
  Module_types.module_type ->
  Module_types.module_type
