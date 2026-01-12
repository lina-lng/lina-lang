(** Type inference facade.

    This module provides the main entry points for type inference,
    delegating to specialized modules:
    - {!Pattern_infer} for pattern inference
    - {!Expression_infer} for expression inference
    - {!Structure_infer} for structure and module inference
    - {!Module_type_check} for module type checking *)

(** [infer_expression ctx expr] infers the type of an expression.
    Delegates to {!Expression_infer.infer_expression}. *)
let infer_expression = Expression_infer.infer_expression

(** [infer_structure ctx structure] infers types for a complete structure.
    Delegates to {!Structure_infer.infer_structure}. *)
let infer_structure = Structure_infer.infer_structure

(** [infer_structure_tolerant ctx structure] infers types for a structure,
    continuing after errors to accumulate as much context as possible.
    Delegates to {!Structure_infer.infer_structure_tolerant}. *)
let infer_structure_tolerant = Structure_infer.infer_structure_tolerant
