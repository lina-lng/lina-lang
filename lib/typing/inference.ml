(** Type inference facade.

    This module provides the main entry points for type inference,
    delegating to specialized modules:
    - {!Pattern_infer} for pattern inference
    - {!Expression_infer} for expression inference
    - {!Structure_infer} for structure and module inference
    - {!Module_type_check} for module type checking *)

(** [infer_expression env expr] infers the type of an expression.
    Delegates to {!Expression_infer.infer_expression}. *)
let infer_expression = Expression_infer.infer_expression

(** [infer_structure env structure] infers types for a complete structure.
    Delegates to {!Structure_infer.infer_structure}. *)
let infer_structure = Structure_infer.infer_structure
