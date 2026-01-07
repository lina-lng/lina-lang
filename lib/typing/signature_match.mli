(** Signature matching for modules.

    This module implements the OCaml-style signature matching algorithm.
    Matching checks whether an implementation provides at least what a
    specification requires (structural subtyping).

    {2 Matching Rules}

    - Implementation can have more than specification requires
    - Values must have compatible types (unifiable)
    - Types must be present (abstract types match any definition)
    - Submodules are matched recursively

    {2 Functor Variance}

    Functor matching follows variance rules:
    - {b Parameters}: Contravariant (spec accepts impl that needs less)
    - {b Results}: Covariant (impl can provide more than spec promises)

    Example: [functor (X : BIG) -> SMALL] matches [functor (X : SMALL) -> BIG]
    because the impl accepts more general input and produces more specific output. *)

(** {1 Configuration} *)

(** [set_module_type_lookup lookup] sets the function for expanding [ModTypeIdent].

    Must be called before matching to resolve named module types.

    @param lookup Function from path to module type definition *)
val set_module_type_lookup : (Types.path -> Module_types.module_type option) -> unit

(** {1 Module Strengthening} *)

(** [strengthen_signature path sig_] makes abstract types concrete.

    When binding a module ([module M = ...]), strengthening ensures that
    [M.t] is equal to the implementation's type, not abstract.

    Example: If [struct type t = int end] is bound to [M], strengthening
    makes [M.t] equal to [int], not an abstract type.

    @param path The path to the module being bound
    @param sig_ The signature to strengthen
    @return Signature with abstract types bound to their paths *)
val strengthen_signature : Types.path -> Module_types.signature -> Module_types.signature

(** [strengthen_module_type path mty] strengthens a module type.

    Applies strengthening recursively through functor results and nested
    signatures.

    @param path The path to the module
    @param mty The module type to strengthen *)
val strengthen_module_type : Types.path -> Module_types.module_type -> Module_types.module_type

(** {1 Path Substitution} *)

(** [substitute_path_in_module_type old_path new_path mty] replaces paths.

    Used during functor application to substitute parameter paths with
    argument paths. For [F(A)], substitutes [X] (functor parameter) with [A].

    @param old_path Path to replace (typically functor parameter)
    @param new_path Replacement path (typically functor argument)
    @param mty Module type to transform *)
val substitute_path_in_module_type : Types.path -> Types.path -> Module_types.module_type -> Module_types.module_type

(** {1 Match Results} *)

(** Reasons why signature matching can fail. *)
type match_error =
  | MissingValue of string
      (** Specification requires a value not present in implementation *)
  | MissingType of string
      (** Specification requires a type not present in implementation *)
  | MissingModule of string
      (** Specification requires a submodule not present in implementation *)
  | TypeMismatch of string * Types.type_expression * Types.type_expression
      (** Value has incompatible type: name, expected, actual *)
  | ModuleTypeMismatch of string * string
      (** Submodule has incompatible module type: name, reason *)

(** Result of signature matching. *)
type match_result = (unit, match_error) result

(** {1 Matching Functions} *)

(** [match_signature loc impl spec] checks implementation against specification.

    For each item in [spec], checks that [impl] has a compatible item:
    - [SigValue]: Implementation value type must unify with spec type
    - [SigType]: Implementation must have a type with the same name
    - [SigModule]: Implementation module must match spec module recursively
    - [SigModuleType]: Not fully implemented (skipped)

    @param loc Location for error messages
    @param impl Implementation signature (what we have)
    @param spec Specification signature (what we need)
    @return [Ok ()] if impl satisfies spec, [Error reason] otherwise *)
val match_signature : Common.Location.t -> Module_types.signature -> Module_types.signature -> match_result

(** [match_module_type loc impl spec] checks module type compatibility.

    Handles three cases:
    - Both signatures: delegates to {!match_signature}
    - Both functors: checks contravariant params, covariant result
    - Named module type: expands via lookup and recurses

    @param loc Location for error messages
    @param impl Implementation module type
    @param spec Specification module type
    @return [Ok ()] if compatible, [Error reason] otherwise *)
val match_module_type : Common.Location.t -> Module_types.module_type -> Module_types.module_type -> match_result

(** [format_match_error err] converts a match error to a human-readable string. *)
val format_match_error : match_error -> string
