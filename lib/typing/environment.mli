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

(** {1 Environment Type} *)

(** Abstract environment type.
    Internally uses maps from names to bindings. *)
type t

(** {1 Creation} *)

(** Empty environment with no bindings. *)
val empty : t

(** Initial environment with built-in bindings.
    Includes [print] and other primitive operations. *)
val initial : t

(** {1 Value Bindings} *)

(** [add_value name id scheme env] adds a value binding.

    @param name The source-level name
    @param id The unique runtime identifier
    @param scheme The polymorphic type scheme
    @param env The environment to extend
    @return A new environment with the binding added *)
val add_value : string -> Common.Identifier.t -> Types.type_scheme -> t -> t

(** [find_value name env] looks up a value binding.

    @param name The name to look up
    @param env The environment to search
    @return [Some (id, scheme)] if found, [None] otherwise *)
val find_value : string -> t -> (Common.Identifier.t * Types.type_scheme) option

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
