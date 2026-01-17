open Common
open Typing.Typed_tree

let diverging_functions = [ "raise"; "failwith"; "invalid_arg"; "exit" ]

let is_diverging_call (expr : typed_expression) =
  match expr.expression_desc with
  | TypedExpressionApply (func, _) -> (
      match func.expression_desc with
      | TypedExpressionVariable (id, _) ->
          let name = Identifier.name id in
          List.mem name diverging_functions
      | TypedExpressionModuleAccess (_, name) ->
          List.mem name diverging_functions
      | _ -> false)
  | TypedExpressionAssert _ ->
      (* assert false is diverging, but we can't easily check the argument *)
      false
  | _ -> false
