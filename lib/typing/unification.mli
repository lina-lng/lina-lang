open Common

exception Unification_error of {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Location.t;
  message : string;
}

val unify : Location.t -> Types.type_expression -> Types.type_expression -> unit
