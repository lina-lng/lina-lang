(** Semantic representation of module types for the type checker.

    This module defines the internal representation of modules and module types
    used during type inference and signature matching. This is the semantic
    representation, as opposed to the syntactic representation in
    {!Parsing.Syntax_tree}.

    {2 Module Type Hierarchy}

    - {!module_type}: The type of a module (signature, functor type, or named)
    - {!signature}: A list of {!signature_item}s
    - {!signature_item}: Individual declarations (values, types, submodules)

    {2 Module Expressions}

    {!module_expr} represents typed module expressions created during inference.
    These track the structure of modules for code generation.

    {2 Paths}

    Module paths ({!path}) reference types and modules through potentially
    nested module structures. See {!Types.path} for the shared definition. *)

open Common

(** {1 Paths} *)

(** Path to a type or module. Re-exported from {!Types} for convenience. *)
type path = Types.path

(** [path_to_string path] converts a path to a human-readable string.

    Examples:
    - [PathBuiltin "int"] becomes ["int"]
    - [PathLocal "t"] becomes ["t"]
    - [PathDot (PathIdent id, "t")] becomes ["M.t"] (where [id] has name "M") *)
val path_to_string : path -> string

(** [path_equal p1 p2] tests structural equality of two paths.

    Two paths are equal if they have the same structure and all components
    are equal. For {!Types.PathIdent}, equality uses {!Common.Identifier.equal}. *)
val path_equal : path -> path -> bool

(** {1 Signature Components} *)

(** Description of a value in a signature.

    Created when processing [val x : t] declarations in signatures. *)
type value_description = {
  value_type : Types.type_scheme;  (** The type scheme of the value *)
  value_location : Location.t;     (** Source location of the declaration *)
}

(** Parameter of a functor.

    Represents the [X : S] part of [functor (X : S) -> ...]. *)
type functor_parameter = {
  parameter_name : string;               (** Name of the parameter module *)
  parameter_id : Common.Identifier.t;    (** Unique identifier for code generation *)
  parameter_type : module_type;          (** Required signature of the argument *)
}

(** {1 Module Types} *)

(** The type of a module.

    Module types classify modules, analogous to how types classify values.

    - {!ModTypeSig}: A concrete signature listing all exports
    - {!ModTypeFunctor}: A function from modules to modules
    - {!ModTypeIdent}: A reference to a named module type *)
and module_type =
  | ModTypeSig of signature
      (** [sig ... end] - A concrete signature *)
  | ModTypeFunctor of functor_parameter * module_type
      (** [functor (X : S) -> MT] - A functor type *)
  | ModTypeIdent of path
      (** Named module type reference: [S], [M.S], [F(A).S] *)

(** A signature is a list of signature items.

    Order matters for scoping: later items can reference earlier ones. *)
and signature = signature_item list

(** Individual items that can appear in a signature.

    Each variant corresponds to a kind of declaration:
    - {!SigValue}: Value declarations ([val x : t])
    - {!SigType}: Type declarations ([type t = ...] or [type t])
    - {!SigModule}: Submodule declarations ([module M : S])
    - {!SigModuleType}: Module type declarations ([module type S = ...]) *)
and signature_item =
  | SigValue of string * value_description
      (** [val x : t] - Value with name and type scheme *)
  | SigType of string * Types.type_declaration
      (** [type t = ...] - Type with name and declaration *)
  | SigModule of string * module_type
      (** [module M : S] - Submodule with name and module type *)
  | SigModuleType of string * module_type option
      (** [module type S = MT] - Module type declaration.
          [None] for abstract module types, [Some mt] for definitions. *)

(** {1 Module Expressions} *)

(** Typed module expressions.

    These represent the structure of modules after type inference,
    used for code generation and further analysis. *)
type module_expr =
  | ModExprStruct of signature
      (** [struct ... end] with inferred signature *)
  | ModExprFunctor of functor_parameter * module_expr
      (** [functor (X : S) -> ME] *)
  | ModExprApply of module_expr * module_expr
      (** [F(A)] - Functor application *)
  | ModExprIdent of path
      (** [M] or [M.N] - Module path reference *)
  | ModExprConstraint of module_expr * module_type
      (** [(ME : MT)] - Constrained module *)

(** {1 Module Bindings} *)

(** A module binding in the environment.

    Created when a module is defined ([module M = ...]) or when
    a functor parameter is introduced. *)
type module_binding = {
  binding_name : string;              (** The module's name *)
  binding_id : Common.Identifier.t;   (** Unique identifier for code generation *)
  binding_type : module_type;         (** The module's type *)
  binding_alias : path option;        (** Original path if this is an alias.
                                          Used for module strengthening. *)
}

(** {1 Pretty Printing} *)

(** [pp_module_type fmt mty] pretty-prints a module type.

    Uses OCaml-like syntax: [sig ... end], [functor (X : S) -> MT]. *)
val pp_module_type : Format.formatter -> module_type -> unit

(** [pp_signature_item fmt item] pretty-prints a signature item.

    Outputs declarations like [val x : int], [type t], [module M : S]. *)
val pp_signature_item : Format.formatter -> signature_item -> unit

(** [module_type_to_string mty] converts a module type to a string.

    Convenience wrapper around {!pp_module_type}. *)
val module_type_to_string : module_type -> string

(** {1 Signature Lookup} *)

(** [find_in_sig matcher sig_] searches for an item in a signature.

    Applies [matcher] to each item in order, returning the first [Some] result.

    @param matcher Function that returns [Some x] for matching items
    @param sig_ The signature to search
    @return [Some x] if [matcher] returned [Some x], [None] otherwise *)
val find_in_sig : (signature_item -> 'a option) -> signature -> 'a option

(** [find_value_in_sig name sig_] finds a value declaration by name.

    @param name The value name to search for
    @param sig_ The signature to search
    @return [Some desc] if [val name : ...] exists, [None] otherwise *)
val find_value_in_sig : string -> signature -> value_description option

(** [find_type_in_sig name sig_] finds a type declaration by name.

    @param name The type name to search for
    @param sig_ The signature to search
    @return [Some decl] if [type name ...] exists, [None] otherwise *)
val find_type_in_sig : string -> signature -> Types.type_declaration option

(** [find_module_in_sig name sig_] finds a submodule declaration by name.

    @param name The module name to search for
    @param sig_ The signature to search
    @return [Some mty] if [module name : ...] exists, [None] otherwise *)
val find_module_in_sig : string -> signature -> module_type option

(** [find_module_type_in_sig name sig_] finds a module type declaration by name.

    @param name The module type name to search for
    @param sig_ The signature to search
    @return [Some (Some mty)] if [module type name = mty] exists,
            [Some None] if [module type name] (abstract) exists,
            [None] if no such declaration *)
val find_module_type_in_sig : string -> signature -> module_type option option
