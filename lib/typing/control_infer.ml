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
type expression_infer_fn = Inference_utils.expression_infer_fn

(** {1 Helper Functions} *)

let unify = Inference_utils.unify

let unify_with_message ctx loc ty1 ty2 ~make_message =
  try unify ctx loc ty1 ty2
  with Unification.Unification_error { expected; actual; location; message = _; trace } ->
    let new_message = make_message ~expected ~actual in
    raise (Unification.Unification_error { expected; actual; location; message = new_message; trace })

(** {1 If Expression Inference} *)

let unify_if_condition ctx loc cond_type =
  unify_with_message ctx loc type_bool cond_type ~make_message:(fun ~expected:_ ~actual ->
    Printf.sprintf "The condition of this `if` expression has type `%s`, \
                    but conditions must be `bool`.\n\n\
                    If expressions require a boolean condition to decide which branch to take."
      (Types.type_expression_to_string actual))

let unify_if_branches ctx else_loc then_type else_type =
  unify_with_message ctx else_loc then_type else_type ~make_message:(fun ~expected ~actual ->
    Printf.sprintf "The `else` branch has type `%s`, but the `then` branch has type `%s`.\n\n\
                    All branches of an `if` expression must return the same type."
      (Types.type_expression_to_string actual)
      (Types.type_expression_to_string expected))

let unify_if_no_else ctx loc then_type =
  unify_with_message ctx loc type_unit then_type ~make_message:(fun ~expected:_ ~actual ->
    Printf.sprintf "This `if` has no `else` branch, so it must return `unit`, \
                    but the `then` branch has type `%s`.\n\n\
                    Add an `else` branch that returns the same type, or make `then` return `unit`."
      (Types.type_expression_to_string actual))

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
  unify_if_condition ctx cond_expr.Location.location typed_cond.expression_type;

  let typed_then, ctx = infer_expr ctx then_expr in

  let typed_else, ctx = match else_expr_opt with
    | Some else_expr ->
      let typed_else, ctx = infer_expr ctx else_expr in
      unify_if_branches ctx else_expr.Location.location
        typed_then.expression_type typed_else.expression_type;
      (Some typed_else, ctx)
    | None ->
      unify_if_no_else ctx then_expr.Location.location typed_then.expression_type;
      (None, ctx)
  in
  ({
    expression_desc = TypedExpressionIf (typed_cond, typed_then, typed_else);
    expression_type = typed_then.expression_type;
    expression_location = loc;
  }, ctx)

(** {1 Match Expression Inference} *)

(** Mode for match result type handling. *)
type match_mode =
  | InferResultType
      (** Create a fresh type variable for the result. *)
  | UseExpectedType of type_expression
      (** Use the given expected type (for GADT return type refinement). *)

(** Context for processing a single match arm. *)
type arm_context = {
  infer_expr : expression_infer_fn;
  scrutinee_type : type_expression;
  match_result_type : type_expression;
  should_extract_equations : bool;
}

(** Extract GADT equations from pattern matching. *)
let extract_gadt_equations arm_ctx pattern_type =
  if arm_ctx.should_extract_equations || Gadt.has_rigid_variables arm_ctx.scrutinee_type then
    let extraction = Gadt.extract_equations arm_ctx.scrutinee_type pattern_type in
    if extraction.success then extraction.equations else []
  else
    []

(** Link/unlink GADT equations - delegated to Gadt module. *)
let link_gadt_equations = Gadt.link_equations
let unlink_gadt_equations = Gadt.unlink_equations

(** Infer guard expression if present. *)
let infer_guard arm_ctx ctx guard_opt =
  match guard_opt with
  | None -> (None, ctx)
  | Some guard_expression ->
      let typed_guard, ctx = arm_ctx.infer_expr ctx guard_expression in
      unify ctx guard_expression.Location.location type_bool typed_guard.expression_type;
      (Some typed_guard, ctx)

(** Check that existential types don't escape their scope. *)
let check_existential_escape loc typed_pattern result_type =
  let existential_ids = Gadt.collect_existentials_from_pattern typed_pattern in
  match Gadt.check_existential_escape existential_ids result_type with
  | Some _escaped_id ->
      Compiler_error.type_error loc
        "This expression has a type containing an existential type variable \
         that would escape its scope"
  | None -> ()

(** Infer a single match arm.

    @param arm_ctx The arm inference context
    @param ctx The typing context
    @param match_arm The match arm to infer
    @return Typed arm and updated context *)
let infer_single_arm arm_ctx ctx match_arm =
  let typed_pattern, pattern_type, arm_ctx_inner =
    Pattern_infer.infer_pattern ctx match_arm.arm_pattern
  in

  let gadt_equations = extract_gadt_equations arm_ctx pattern_type in

  (* For GADT patterns, link rigid type variables to their equation types.
     For non-GADT patterns, unify normally. *)
  if gadt_equations <> [] then
    link_gadt_equations gadt_equations
  else
    unify ctx match_arm.arm_location arm_ctx.scrutinee_type pattern_type;

  let typed_guard, arm_ctx_inner = infer_guard arm_ctx arm_ctx_inner match_arm.arm_guard in
  let typed_arm_expression, _arm_ctx = arm_ctx.infer_expr arm_ctx_inner match_arm.arm_expression in

  check_existential_escape match_arm.arm_location typed_pattern typed_arm_expression.expression_type;

  let expected_branch_type =
    if gadt_equations <> [] then Gadt.apply_equations gadt_equations arm_ctx.match_result_type
    else arm_ctx.match_result_type
  in
  unify ctx match_arm.arm_location expected_branch_type typed_arm_expression.expression_type;

  unlink_gadt_equations gadt_equations;

  {
    Typed_tree.typed_arm_pattern = typed_pattern;
    typed_arm_guard = typed_guard;
    typed_arm_expression;
    typed_arm_location = match_arm.arm_location;
  }

(** Core match inference implementation parameterized by mode.

    @param mode How to handle the result type
    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param scrutinee_expr The expression being matched
    @param match_arms The list of match arms
    @return A pair [(typed_expr, updated_ctx)] *)
let infer_match_core ~mode ~(infer_expr : expression_infer_fn) ctx loc scrutinee_expr match_arms =
  let env = Typing_context.environment ctx in

  let typed_scrutinee, ctx = infer_expr ctx scrutinee_expr in
  let scrutinee_type = typed_scrutinee.expression_type in

  let result_type, ctx = match mode with
    | InferResultType -> Typing_context.new_type_variable ctx
    | UseExpectedType expected_ty -> (expected_ty, ctx)
  in

  let should_extract_equations = match mode with
    | InferResultType -> false
    | UseExpectedType _ -> true
  in

  let arm_ctx = {
    infer_expr;
    scrutinee_type;
    match_result_type = result_type;
    should_extract_equations;
  } in

  let typed_match_arms = List.map (infer_single_arm arm_ctx ctx) match_arms in

  Pattern_check.check_match env loc scrutinee_type typed_match_arms;
  ({
    expression_desc = TypedExpressionMatch (typed_scrutinee, typed_match_arms);
    expression_type = result_type;
    expression_location = loc;
  }, ctx)

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
let infer_match ~infer_expr ctx loc scrutinee_expr match_arms =
  infer_match_core ~mode:InferResultType ~infer_expr ctx loc scrutinee_expr match_arms

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
let infer_match_with_expected ~infer_expr ctx loc expected_ty scrutinee_expr match_arms =
  infer_match_core ~mode:(UseExpectedType expected_ty) ~infer_expr ctx loc scrutinee_expr match_arms
