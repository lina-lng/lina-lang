(** This module combines two detection strategies:
    1. Simple sequence analysis: detects code after diverging calls like raise/failwith
    2. CFG-based analysis: detects unreachable code in complex control flow *)

open Common
open Typing.Typed_tree

type finding = {
  location : Location.t;
  reason : string;
}

let rec all_branches_diverge (expr : typed_expression) =
  match expr.expression_desc with
  | TypedExpressionIf (_, then_expr, Some else_expr) ->
      expr_diverges then_expr && expr_diverges else_expr
  | TypedExpressionMatch (_, arms) ->
      List.for_all
        (fun (arm : typed_match_arm) -> expr_diverges arm.typed_arm_expression)
        arms
  | TypedExpressionSequence (_, right) -> expr_diverges right
  | TypedExpressionLet (_, _, body) -> expr_diverges body
  | _ -> Divergence.is_diverging_call expr

and expr_diverges expr =
  Divergence.is_diverging_call expr || all_branches_diverge expr

let rec detect_expr (expr : typed_expression) : finding list =
  match expr.expression_desc with
  | TypedExpressionVariable _ | TypedExpressionConstant _
  | TypedExpressionModuleAccess _ | TypedExpressionError _ ->
      []

  | TypedExpressionTuple exprs ->
      List.concat_map detect_expr exprs

  | TypedExpressionConstructor (_, arg_opt) ->
      Option.fold ~none:[] ~some:detect_expr arg_opt

  | TypedExpressionApply (func, args) ->
      detect_expr func @ List.concat_map (fun (_, arg) -> detect_expr arg) args

  | TypedExpressionPartialApply partial ->
      detect_expr partial.partial_func
      @ List.concat_map
          (fun (_, slot) ->
            match slot with SlotFilled e -> detect_expr e | SlotNeeded _ -> [])
          partial.partial_slots

  | TypedExpressionFunction (_, body) ->
      detect_expr body

  | TypedExpressionLet (_, bindings, body) ->
      List.concat_map (fun b -> detect_expr b.binding_expression) bindings
      @ detect_expr body

  | TypedExpressionIf (cond, then_expr, None) ->
      detect_expr cond @ detect_expr then_expr
  | TypedExpressionIf (cond, then_expr, Some else_expr) ->
      detect_expr cond @ detect_expr then_expr @ detect_expr else_expr

  | TypedExpressionSequence (left, right) ->
      let left_findings = detect_expr left in
      if expr_diverges left then
        left_findings
        @ [
            {
              location = right.expression_location;
              reason = "code after diverging expression is unreachable";
            };
          ]
      else left_findings @ detect_expr right

  | TypedExpressionRecord fields ->
      List.concat_map (fun f -> detect_expr f.typed_field_value) fields

  | TypedExpressionRecordAccess (record, _) ->
      detect_expr record

  | TypedExpressionRecordUpdate (record, fields) ->
      detect_expr record
      @ List.concat_map (fun f -> detect_expr f.typed_field_value) fields

  | TypedExpressionMatch (scrutinee, arms) ->
      detect_expr scrutinee
      @ List.concat_map
          (fun (arm : typed_match_arm) ->
            let guard_findings = Option.fold ~none:[] ~some:detect_expr arm.typed_arm_guard in
            guard_findings @ detect_expr arm.typed_arm_expression)
          arms

  | TypedExpressionRef init -> detect_expr init
  | TypedExpressionDeref inner -> detect_expr inner
  | TypedExpressionAssign (lhs, rhs) -> detect_expr lhs @ detect_expr rhs
  | TypedExpressionAssert inner -> detect_expr inner

  | TypedExpressionWhile (cond, body) ->
      detect_expr cond @ detect_expr body

  | TypedExpressionFor (_, start, finish, _, body) ->
      detect_expr start @ detect_expr finish @ detect_expr body

  | TypedExpressionPolyVariant (_, arg_opt) ->
      Option.fold ~none:[] ~some:detect_expr arg_opt

  | TypedExpressionPack (mod_expr, _) ->
      detect_mod_expr mod_expr

  | TypedExpressionLetModule (_, mod_expr, body) ->
      detect_mod_expr mod_expr @ detect_expr body

and detect_mod_expr (mod_expr : typed_module_expression) : finding list =
  match mod_expr.module_desc with
  | TypedModuleStructure structure -> detect_structure structure
  | TypedModulePath _ -> []
  | TypedModuleFunctor (_, body) -> detect_mod_expr body
  | TypedModuleApply (func, arg) -> detect_mod_expr func @ detect_mod_expr arg
  | TypedModuleConstraint (inner, _) -> detect_mod_expr inner
  | TypedModuleUnpack (expr, _) -> detect_expr expr

and detect_structure structure : finding list =
  List.concat_map detect_structure_item structure

and detect_structure_item (item : typed_structure_item) : finding list =
  match item.structure_item_desc with
  | TypedStructureValue (_, bindings) ->
      List.concat_map (fun b -> detect_expr b.binding_expression) bindings
  | TypedStructureType _ -> []
  | TypedStructureModule (_, mod_expr) -> detect_mod_expr mod_expr
  | TypedStructureRecModule bindings ->
      List.concat_map (fun b -> detect_mod_expr b.rec_module_expr) bindings
  | TypedStructureModuleType _ -> []
  | TypedStructureOpen _ -> []
  | TypedStructureInclude (mod_expr, _) -> detect_mod_expr mod_expr
  | TypedStructureExternal _ -> []
  | TypedStructureTypeExtension _ -> []
  | TypedStructureExpression expr -> detect_expr expr
  | TypedStructureError _ -> []

let detect_in_expression = detect_expr
let detect_in_structure = detect_structure

let to_diagnostic config finding =
  let code = Error_code.w_dead_code in
  let severity = Common.Warning_config.severity_for config code in

  let diag =
    Compiler_error.make_diagnostic ~severity ~code ~message:finding.reason
      ~labels:
        [
          {
            label_span = finding.location;
            label_message = Some "this code will never be executed";
            is_primary = true;
          };
        ]
      ()
  in

  Compiler_error.with_suggestion ~span:finding.location
    ~message:"remove unreachable code" ~replacement:""
    ~applicability:Compiler_error.MaybeIncorrect diag
