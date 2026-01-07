open Common

exception Unification_error of {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Location.t;
  message : string;
}

(** Set the type lookup function for alias expansion.
    Must be called before unification to enable type alias support. *)
val set_type_lookup : (Types.path -> Types.type_declaration option) -> unit

(** Expand type aliases to their definitions. *)
val expand_type : Types.type_expression -> Types.type_expression

val unify : Location.t -> Types.type_expression -> Types.type_expression -> unit
