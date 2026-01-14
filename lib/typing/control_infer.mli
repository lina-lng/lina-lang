(** Control flow type inference.

    This module handles type inference for control flow expressions:
    - If-then-else expressions
    - Pattern match expressions (including GADT support)

    {2 GADT Pattern Matching}

    Match expressions require special handling when the scrutinee or expected
    type contains rigid type variables (GADTs). In these cases:
    - Type equations are extracted from pattern matching
    - Equations refine the expected result type in each branch
    - Existential variables are checked to prevent scope escape *)

(** {1 Callback Types} *)

(** Expression inference function type for callback. *)
type expression_infer_fn =
  Typing_context.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

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
val infer_if :
  infer_expr:expression_infer_fn ->
  Typing_context.t ->
  Common.Location.t ->
  Parsing.Syntax_tree.expression ->
  Parsing.Syntax_tree.expression ->
  Parsing.Syntax_tree.expression option ->
  Typed_tree.typed_expression * Typing_context.t

(** {1 Match Expression Inference} *)

(** [infer_match ~infer_expr ctx loc scrutinee_expr match_arms] infers the type
    of a pattern match expression.

    Handles GADT patterns by extracting type equations and applying them to
    the result type. Also checks for existential type variable escape and
    performs exhaustiveness/redundancy checking.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param scrutinee_expr The expression being matched
    @param match_arms The list of match arms
    @return A pair [(typed_expr, updated_ctx)]
    @raise Compiler_error.Type_error on existential escape or pattern errors *)
val infer_match :
  infer_expr:expression_infer_fn ->
  Typing_context.t ->
  Common.Location.t ->
  Parsing.Syntax_tree.expression ->
  Parsing.Syntax_tree.match_arm list ->
  Typed_tree.typed_expression * Typing_context.t

(** [infer_match_with_expected ~infer_expr ctx loc expected_ty scrutinee_expr match_arms]
    infers the type of a pattern match expression with an expected result type
    containing rigid type variables.

    This is used for GADT pattern matching where the return type is a locally
    abstract type that gets refined in each branch. The expected type is used
    directly as the result type, and GADT equations are applied to it in each
    branch.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param expected_ty The expected result type (containing rigid variables)
    @param scrutinee_expr The expression being matched
    @param match_arms The list of match arms
    @return A pair [(typed_expr, updated_ctx)]
    @raise Compiler_error.Type_error on existential escape or pattern errors *)
val infer_match_with_expected :
  infer_expr:expression_infer_fn ->
  Typing_context.t ->
  Common.Location.t ->
  Types.type_expression ->
  Parsing.Syntax_tree.expression ->
  Parsing.Syntax_tree.match_arm list ->
  Typed_tree.typed_expression * Typing_context.t
