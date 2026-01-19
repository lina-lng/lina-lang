(** Typing environment for name resolution and type lookups.

    The environment maps names to their types, identifiers, and declarations.
    It supports:
    - Value bindings: [let x = ...] binds [x] to a type scheme
    - Type declarations: [type t = ...] adds type [t] to the environment
    - Constructor bindings: variant constructors are resolved here
    - Module bindings: [module M = ...] adds module [M]
    - Module type definitions: [module type S = ...]

    Environments are immutable and use shadowing for nested scopes.

    {2 Initial Environment}

    {!initial} contains bindings for built-in functions:
    - [print : 'a -> unit]
    - Arithmetic operators via primitives *)

(** {1 Types} *)

(** Value binding with definition location for go-to-definition support. *)
type value_binding = {
  binding_id : Common.Identifier.t;
  binding_scheme : Types.type_scheme;
  binding_location : Common.Location.t;
}

(** {1 Environment Type} *)

(** Abstract environment type.
    Internally uses maps from names to bindings. *)
type t

(** {1 Creation} *)

(** Empty environment with no bindings. *)
val empty : t

(** Initial environment with built-in bindings.
    Includes [print], [option] type, [result] type, and other primitive operations. *)
val initial : t

(** {1 Built-in Option Type}

    The option type and its constructors are built-in for optional arguments.
    When a function has an optional parameter [?x:int], the internal type is
    [int option]. When called with [~x:5], the argument is wrapped as [Some 5].
    When the argument is omitted, it defaults to [None]. *)

(** Constructor info for [None : 'a option].
    Re-exported from {!Builtins} for convenience. *)
val none_constructor : Types.constructor_info

(** Constructor info for [Some : 'a -> 'a option].
    Re-exported from {!Builtins} for convenience. *)
val some_constructor : Types.constructor_info

(** {1 Built-in Result Type}

    The result type is built-in for error handling.
    [Ok x] represents successful computation with value [x].
    [Error e] represents failed computation with error [e]. *)

(** Constructor info for [Ok : 'a -> ('a, 'e) result].
    Re-exported from {!Builtins} for convenience. *)
val ok_constructor : Types.constructor_info

(** Constructor info for [Error : 'e -> ('a, 'e) result].
    Re-exported from {!Builtins} for convenience. *)
val error_constructor : Types.constructor_info

(** {1 Built-in List Type}

    The list type is built-in for immutable linked lists.
    [Nil] represents the empty list.
    [Cons (x, xs)] creates a new list by prepending element [x] to list [xs]. *)

(** Constructor info for [Nil : 'a list].
    Re-exported from {!Builtins} for convenience. *)
val nil_constructor : Types.constructor_info

(** Constructor info for [Cons : 'a * 'a list -> 'a list].
    Re-exported from {!Builtins} for convenience. *)
val cons_constructor : Types.constructor_info

(** {1 Value Bindings} *)

(** [add_value name id scheme location env] adds a value binding.

    @param name The source-level name
    @param id The unique runtime identifier
    @param scheme The polymorphic type scheme
    @param location The source location of the definition
    @param env The environment to extend
    @return A new environment with the binding added *)
val add_value : string -> Common.Identifier.t -> Types.type_scheme -> Common.Location.t -> t -> t

(** [find_value name env] looks up a value binding.

    @param name The name to look up
    @param env The environment to search
    @return [Some binding] if found, [None] otherwise *)
val find_value : string -> t -> value_binding option

(** [get_value_bindings env] returns all value bindings as a list.

    Returns [(name, identifier, scheme)] tuples for all bound values.
    Used for comparing environments in or-patterns.

    @param env The environment to extract bindings from
    @return List of (name, identifier, type scheme) triples *)
val get_value_bindings : t -> (string * Common.Identifier.t * Types.type_scheme) list

(** {1 Type Declarations} *)

(** [add_type name decl env] adds a type declaration.

    @param name The type name
    @param decl The type declaration (abstract, variant, or record)
    @param env The environment to extend
    @return A new environment with the type added *)
val add_type : string -> Types.type_declaration -> t -> t

(** [find_type name env] looks up a type by simple name.

    @param name The type name to look up
    @param env The environment to search
    @return [Some decl] if found, [None] otherwise *)
val find_type : string -> t -> Types.type_declaration option

(** [find_type_by_path path env] looks up a type by path.

    Handles qualified paths like [M.t] by looking up module [M]
    and then finding type [t] in its signature.

    @param path The type path
    @param env The environment to search
    @return [Some decl] if found, [None] otherwise *)
val find_type_by_path : Types.path -> t -> Types.type_declaration option

(** {1 Constructor Bindings} *)

(** [add_constructor name info env] adds a constructor binding.

    @param name The constructor name (e.g., "Some")
    @param info Constructor information including tag and types
    @param env The environment to extend
    @return A new environment with the constructor added *)
val add_constructor : string -> Types.constructor_info -> t -> t

(** [find_constructor name env] looks up a constructor.

    @param name The constructor name
    @param env The environment to search
    @return [Some info] if found, [None] otherwise *)
val find_constructor : string -> t -> Types.constructor_info option

(** [find_type_constructors type_name env] gets all constructors for a type.

    Returns all constructors for a variant type, in declaration order.
    Used for exhaustiveness checking.

    @param type_name The name of the variant type
    @param env The environment to search
    @return [Some ctors] for variant types, [None] for abstract/non-existent *)
val find_type_constructors : string -> t -> Types.constructor_info list option

(** [find_constructor_by_path path_strings ctor_name env] looks up a constructor
    through a module path.

    For example, [find_constructor_by_path ["M"; "N"] "Some" env] looks for
    constructor [Some] in module [M.N].

    @param path_strings Module path components (empty for unqualified names)
    @param ctor_name The constructor name
    @param env The environment to search
    @return [Some info] if found, [None] otherwise *)
val find_constructor_by_path : string list -> string -> t -> Types.constructor_info option

(** {1 Module Bindings} *)

(** [add_module name binding env] adds a module binding.

    @param name The module name
    @param binding The module binding with type and identity
    @param env The environment to extend
    @return A new environment with the module added *)
val add_module : string -> Module_types.module_binding -> t -> t

(** [find_module name env] looks up a module by simple name.

    @param name The module name
    @param env The environment to search
    @return [Some binding] if found, [None] otherwise *)
val find_module : string -> t -> Module_types.module_binding option

(** [add_module_type name mty_opt env] adds a module type definition.

    @param name The module type name (e.g., "S" for [module type S = ...])
    @param mty_opt [Some mty] for definitions, [None] for abstract
    @param env The environment to extend
    @return A new environment with the module type added *)
val add_module_type : string -> Module_types.module_type option -> t -> t

(** [find_module_type name env] looks up a module type.

    @param name The module type name
    @param env The environment to search
    @return [Some (Some mty)] for definitions, [Some None] for abstract,
            [None] if not found *)
val find_module_type : string -> t -> Module_types.module_type option option

(** [find_module_type_by_path path env] looks up a module type by path.

    Handles qualified paths like [M.S] by looking up module [M]
    and then finding module type [S] in its signature.

    @param path The module type path
    @param env The environment to search
    @return [Some mty] if found and concrete, [None] if not found or abstract *)
val find_module_type_by_path : Types.path -> t -> Module_types.module_type option

(** [find_module_by_path path env] looks up a module by path.

    Handles qualified paths like [M.N] by traversing module signatures.

    @param path The module path
    @param env The environment to search
    @return [Some binding] if found, [None] otherwise *)
val find_module_by_path : Types.path -> t -> Module_types.module_binding option

(** {1 Open and Include} *)

(** [open_module sig_ env] opens a module, bringing its contents into scope.

    All items from the signature are added to the environment.
    Fresh identifiers are created for values.

    @param sig_ The signature to open
    @param env The environment to extend
    @return A pair [(env', opened)] where [env'] is the new environment
            and [opened] is a list of (name, identifier) pairs for opened values *)
val open_module :
  Module_types.signature -> t -> t * (string * Common.Identifier.t) list

(** {1 Iteration Functions} *)

(** [fold_values f env acc] folds over all value bindings in the environment.

    @param f Function called with name, identifier, type scheme, and accumulator
    @param env The environment to iterate
    @param acc Initial accumulator value
    @return Final accumulator after processing all values *)
val fold_values :
  (string -> Common.Identifier.t -> Types.type_scheme -> 'a -> 'a) -> t -> 'a -> 'a

(** [fold_types f env acc] folds over all type declarations in the environment.

    @param f Function called with type name, declaration, and accumulator
    @param env The environment to iterate
    @param acc Initial accumulator value
    @return Final accumulator after processing all types *)
val fold_types : (string -> Types.type_declaration -> 'a -> 'a) -> t -> 'a -> 'a

(** [fold_constructors f env acc] folds over all constructor bindings.

    @param f Function called with constructor name, info, and accumulator
    @param env The environment to iterate
    @param acc Initial accumulator value
    @return Final accumulator after processing all constructors *)
val fold_constructors : (string -> Types.constructor_info -> 'a -> 'a) -> t -> 'a -> 'a

(** [fold_modules f env acc] folds over all module bindings.

    @param f Function called with module name, binding, and accumulator
    @param env The environment to iterate
    @param acc Initial accumulator value
    @return Final accumulator after processing all modules *)
val fold_modules :
  (string -> Module_types.module_binding -> 'a -> 'a) -> t -> 'a -> 'a
