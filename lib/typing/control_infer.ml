(** Control flow type inference.

    This module handles type inference for control flow expressions:
    - If-then-else expressions
    - Pattern match expressions (including GADT support)

    Extracted from expression_infer.ml to reduce complexity.

    {2 GADT Pattern Matching}

    Match expressions require special handling when the scrutinee or expected
    type contains rigid type variables (GADTs). In these cases:
    - Type equations are extracted from pattern matching
    - Equations refine the expected result type in each branch
    - Existential variables are checked to prevent scope escape *)

open Common
open Parsing.Syntax_tree
open Types
open Typed_tree

(** {1 Callback Types} *)

(** Expression inference function type for callback. *)
type expression_infer_fn =
  Typing_context.t -> expression -> typed_expression * Typing_context.t

(** {1 Helper Functions} *)

(** Unify types using context's environment for alias expansion. *)
let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  Inference_utils.unify_with_env env loc ty1 ty2

(** {1 If Expression Inference} *)

(** [infer_if ~infer_expr ctx loc cond_expr then_expr else_expr_opt] infers
    the type of an if-then-else expression.

    The condition must be bool. If there's an else branch, both branches
    must have the same type. If there's no else branch, the then branch
    must have type unit.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param cond_expr The condition expression
    @param then_expr The then branch expression
    @param else_expr_opt Optional else branch expression
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_if ~(infer_expr : expression_infer_fn) ctx loc cond_expr then_expr else_expr_opt =
  let typed_cond, ctx = infer_expr ctx cond_expr in
  unify ctx loc type_bool typed_cond.expression_type;
  let typed_then, ctx = infer_expr ctx then_expr in
  let typed_else, ctx = match else_expr_opt with
    | Some else_expr ->
      let typed_else, ctx = infer_expr ctx else_expr in
      unify ctx loc typed_then.expression_type typed_else.expression_type;
      (Some typed_else, ctx)
    | None ->
      unify ctx loc type_unit typed_then.expression_type;
      (None, ctx)
  in
  ({
    expression_desc = TypedExpressionIf (typed_cond, typed_then, typed_else);
    expression_type = typed_then.expression_type;
    expression_location = loc;
  }, ctx)

(** {1 Match Expression Inference} *)

(** [infer_match ~infer_expr ctx loc scrutinee_expr match_arms] infers the type
    of a pattern match expression.

    Handles GADT patterns by extracting type equations and applying them to
    the result type. Also checks for existential type variable escape.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param scrutinee_expr The expression being matched
    @param match_arms The list of match arms
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_match ~(infer_expr : expression_infer_fn) ctx loc scrutinee_expr match_arms =
  let env = Typing_context.environment ctx in

  let typed_scrutinee, ctx = infer_expr ctx scrutinee_expr in
  let scrutinee_type = typed_scrutinee.expression_type in
  let result_type, ctx = Typing_context.new_type_variable ctx in
  let scrutinee_has_rigid = Gadt.has_rigid_variables scrutinee_type in

  let typed_match_arms, ctx = List.fold_left (fun (arms, ctx) match_arm ->
    let typed_pattern, pattern_type, arm_ctx =
      Pattern_infer.infer_pattern ctx match_arm.arm_pattern in

    let gadt_equations =
      if scrutinee_has_rigid then
        let extraction = Gadt.extract_equations scrutinee_type pattern_type in
        if extraction.success then extraction.equations else []
      else
        []
    in

    unify ctx match_arm.arm_location scrutinee_type pattern_type;

    let typed_guard, arm_ctx = match match_arm.arm_guard with
      | None -> (None, arm_ctx)
      | Some guard_expression ->
          let typed_guard_expression, arm_ctx = infer_expr arm_ctx guard_expression in
          unify arm_ctx guard_expression.Location.location type_bool
            typed_guard_expression.expression_type;
          (Some typed_guard_expression, arm_ctx)
    in

    let typed_arm_expression, _arm_ctx = infer_expr arm_ctx match_arm.arm_expression in

    let existential_ids = Gadt.collect_existentials_from_pattern typed_pattern in
    begin match Gadt.check_existential_escape existential_ids typed_arm_expression.expression_type with
    | Some _escaped_id ->
        Compiler_error.type_error match_arm.arm_location
          "This expression has a type containing an existential type variable \
           that would escape its scope"
    | None -> ()
    end;

    let expected_result_type =
      if gadt_equations <> [] then Gadt.apply_equations gadt_equations result_type
      else result_type
    in
    unify ctx match_arm.arm_location expected_result_type typed_arm_expression.expression_type;

    let typed_arm = {
      Typed_tree.typed_arm_pattern = typed_pattern;
      typed_arm_guard = typed_guard;
      typed_arm_expression;
      typed_arm_location = match_arm.arm_location;
    } in
    (typed_arm :: arms, ctx)
  ) ([], ctx) match_arms in

  let typed_match_arms = List.rev typed_match_arms in
  Pattern_check.check_match env loc scrutinee_type typed_match_arms;
  ({
    expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
    expression_type = result_type;
    expression_location = loc;
  }, ctx)

(** [infer_match_with_expected ~infer_expr ctx loc expected_ty scrutinee_expr match_arms]
    infers the type of a pattern match expression with an expected result type
    containing rigid type variables.

    This is used for GADT pattern matching where the return type is a locally
    abstract type that gets refined in each branch.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param expected_ty The expected result type (containing rigid variables)
    @param scrutinee_expr The expression being matched
    @param match_arms The list of match arms
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_match_with_expected ~(infer_expr : expression_infer_fn) ctx loc expected_ty scrutinee_expr match_arms =
  let env = Typing_context.environment ctx in

  let typed_scrutinee, ctx = infer_expr ctx scrutinee_expr in
  let scrutinee_type = typed_scrutinee.expression_type in

  let typed_match_arms, ctx = List.fold_left (fun (arms, ctx) match_arm ->
    let typed_pattern, pattern_type, arm_ctx =
      Pattern_infer.infer_pattern ctx match_arm.arm_pattern in

    let gadt_equations =
      let extraction = Gadt.extract_equations scrutinee_type pattern_type in
      if extraction.success then extraction.equations else []
    in

    if gadt_equations = [] then
      unify ctx match_arm.arm_location scrutinee_type pattern_type;

    let typed_guard, arm_ctx = match match_arm.arm_guard with
      | None -> (None, arm_ctx)
      | Some guard_expression ->
          let typed_guard_expression, arm_ctx = infer_expr arm_ctx guard_expression in
          unify arm_ctx guard_expression.Location.location type_bool
            typed_guard_expression.expression_type;
          (Some typed_guard_expression, arm_ctx)
    in

    let typed_arm_expression, _arm_ctx = infer_expr arm_ctx match_arm.arm_expression in

    let existential_ids = Gadt.collect_existentials_from_pattern typed_pattern in
    begin match Gadt.check_existential_escape existential_ids typed_arm_expression.expression_type with
    | Some _escaped_id ->
        Compiler_error.type_error match_arm.arm_location
          "This expression has a type containing an existential type variable \
           that would escape its scope"
    | None -> ()
    end;

    let expected_branch_type = Gadt.apply_equations gadt_equations expected_ty in
    unify ctx match_arm.arm_location expected_branch_type typed_arm_expression.expression_type;

    let typed_arm = {
      Typed_tree.typed_arm_pattern = typed_pattern;
      typed_arm_guard = typed_guard;
      typed_arm_expression;
      typed_arm_location = match_arm.arm_location;
    } in
    (typed_arm :: arms, ctx)
  ) ([], ctx) match_arms in

  let typed_match_arms = List.rev typed_match_arms in
  Pattern_check.check_match env loc scrutinee_type typed_match_arms;
  ({
    expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
    expression_type = expected_ty;
    expression_location = loc;
  }, ctx)
