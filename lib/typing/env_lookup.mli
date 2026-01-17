(** Environment lookup combinators with built-in error handling.

    These combinators eliminate the repetitive "find or error" pattern
    throughout the type checker while providing consistent error messages.

    {2 Design Principle}

    Each combinator:
    - Takes a location for error reporting
    - Returns the unwrapped value directly (not option)
    - Raises a descriptive type error on failure

    This follows the "exceptions for control flow in type checking" pattern
    used throughout OCaml-style type checkers.

    {2 Example Usage}

    {[
      (* Before - manual pattern matching *)
      let binding =
        match Environment.find_value name env with
        | None -> Inference_utils.error_unbound_variable loc name
        | Some binding -> binding
      in
      let ty, ctx = Typing_context.instantiate ctx binding.binding_scheme in
      ...

      (* After - using Env_lookup *)
      let binding = Env_lookup.find_value_exn loc name env in
      let ty, ctx = Typing_context.instantiate ctx binding.binding_scheme in
      ...
    ]} *)

open Common

(** {1 Value Lookups} *)

(** [find_value_exn loc name env] finds a value binding or raises.

    @param loc Source location for error messages
    @param name The value name to look up
    @param env The environment to search
    @return The value binding
    @raise Type_error if not found *)
val find_value_exn : Location.t -> string -> Environment.t -> Environment.value_binding

(** [find_value_opt name env] finds a value binding if it exists.

    Alias for {!Environment.find_value}. *)
val find_value_opt : string -> Environment.t -> Environment.value_binding option

(** {1 Type Lookups} *)

(** [find_type_exn loc name env] finds a type declaration or raises. *)
val find_type_exn : Location.t -> string -> Environment.t -> Types.type_declaration

(** [find_type_opt name env] finds a type declaration if it exists. *)
val find_type_opt : string -> Environment.t -> Types.type_declaration option

(** {1 Constructor Lookups} *)

(** [find_constructor_exn loc name env] finds a constructor or raises. *)
val find_constructor_exn : Location.t -> string -> Environment.t -> Types.constructor_info

(** [find_constructor_opt name env] finds a constructor if it exists. *)
val find_constructor_opt : string -> Environment.t -> Types.constructor_info option

(** {1 Module Lookups} *)

(** [find_module_exn loc name env] finds a module binding or raises. *)
val find_module_exn : Location.t -> string -> Environment.t -> Module_types.module_binding

(** [find_module_opt name env] finds a module binding if it exists. *)
val find_module_opt : string -> Environment.t -> Module_types.module_binding option

(** {1 Module Type Lookups} *)

(** [find_module_type_exn loc name env] finds a module type definition or raises.

    Raises if the module type is not found OR if it is abstract (no definition). *)
val find_module_type_exn : Location.t -> string -> Environment.t -> Module_types.module_type

(** [find_module_type_opt name env] returns the module type if found and concrete. *)
val find_module_type_opt : string -> Environment.t -> Module_types.module_type option

