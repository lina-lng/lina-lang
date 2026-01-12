(** Robust module type expansion with cycle detection.

    This module provides robust expansion of module types, handling:
    - [ModTypeIdent] resolution through nested modules
    - Cycle detection in module type aliases
    - Path-dependent type resolution
    - Better error messages for expansion failures

    {2 Background}

    Module types can reference other module types by name:
    {[
      module type S = sig type t end
      module type T = S  (* T is an alias for S *)
    ]}

    When checking module constraints or functor applications, we need to
    expand these references to their definitions. This module handles that
    expansion with proper cycle detection and error handling.

    {2 Usage}

    {[
      let state = create_state ~env_lookup:(make_lookup env) in
      let expanded = expand state module_type in
    ]} *)

(** {1 Error Types} *)

(** Errors that can occur during module type expansion. *)
type expansion_error =
  | ModuleTypeNotFound of Types.path
      (** The module type path could not be resolved *)
  | CyclicModuleType of Types.path list
      (** A cycle was detected in module type definitions *)
  | PathResolutionFailed of Types.path * string
      (** Path resolution failed with a reason *)

(** Exception raised when expansion fails. *)
exception Expansion_error of expansion_error

(** {1 Expansion State} *)

(** Expansion state that tracks visited paths for cycle detection. *)
type state

(** [create_state ~env_lookup] creates a new expansion state.

    @param env_lookup A function that looks up module types by path.
           Should return [Some mty] if the path resolves to a module type,
           or [None] if not found. *)
val create_state :
  env_lookup:(Types.path -> Module_types.module_type option) ->
  state

(** {1 Expansion Functions} *)

(** [expand state mty] fully expands a module type, resolving all
    [ModTypeIdent] references.

    Recursively follows all module type identifiers until reaching
    concrete [ModTypeSig] or [ModTypeFunctor] forms.

    @param state The expansion state (for cycle detection)
    @param mty The module type to expand
    @return The fully expanded module type
    @raise Expansion_error if expansion fails *)
val expand : state -> Module_types.module_type -> Module_types.module_type

(** [try_expand state mty] attempts to expand a module type.

    Like [expand] but returns [None] on failure instead of raising.

    @param state The expansion state
    @param mty The module type to expand
    @return [Some expanded_mty] on success, [None] on failure *)
val try_expand : state -> Module_types.module_type -> Module_types.module_type option

(** [expand_shallow state mty] expands only the outermost [ModTypeIdent].

    Does not recursively expand nested module types. Useful when you
    only need to see the top-level structure.

    @param state The expansion state
    @param mty The module type to expand
    @return The shallowly expanded module type *)
val expand_shallow : state -> Module_types.module_type -> Module_types.module_type

(** {1 Error Formatting} *)

(** [format_error err] returns a user-friendly error message. *)
val format_error : expansion_error -> string
