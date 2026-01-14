(** Binding type inference.

    This module handles type inference for let-bindings, including:
    - Non-recursive bindings with generalization
    - Recursive bindings with mutual recursion support
    - Polymorphic recursion via TypeForall annotations

    {2 Polymorphic Recursion}

    Bindings with [type a b.] annotations enable polymorphic recursion:
    - The annotated type parameters become rigid type variables
    - Recursive calls within the body instantiate the full scheme
    - This allows writing functions like [length : type a. a expr -> int]

    {2 Value Restriction}

    Non-value expressions (applications, matches, etc.) have limited
    generalization to maintain type soundness with mutable references.
    The relaxed value restriction allows generalizing covariant type
    variables even for non-values. *)

(** {1 Callback Types} *)

(** Expression inference function type for callback. *)
type expression_infer_fn =
  Typing_context.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

(** Expression inference with expected type for bidirectional checking. *)
type expression_infer_expected_fn =
  Typing_context.t ->
  Types.type_expression option ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

(** {1 Pattern Binding Collection} *)

(** [collect_pattern_bindings pattern] collects all variable bindings from
    a typed pattern.

    Returns a list of [(name, identifier, type)] tuples for each variable
    bound by the pattern. Nested patterns (tuples, records, constructors)
    have their bindings collected recursively.

    @param pattern The typed pattern to collect bindings from
    @return List of (name, identifier, type) tuples *)
val collect_pattern_bindings :
  Typed_tree.typed_pattern ->
  (string * Common.Identifier.t * Types.type_expression) list

(** {1 Binding Inference} *)

(** [infer_bindings ~infer_expr ~infer_expr_expected ctx rec_flag bindings]
    infers types for a list of bindings.

    For non-recursive bindings, each binding is processed sequentially,
    with each binding's type generalized before adding to the context.

    For recursive bindings, all binding names are first added to the context
    with fresh type variables, then all expressions are inferred, and finally
    types are generalized.

    Polymorphic recursion is supported when the binding has a [type a b. ...]
    annotation. In that case, the annotated type parameters become rigid
    variables and recursive calls instantiate the full polymorphic scheme.

    @param infer_expr The expression inference function callback
    @param infer_expr_expected The expression inference with expected type callback
    @param ctx The typing context
    @param rec_flag Whether the bindings are recursive
    @param bindings The list of bindings to infer
    @return A pair [(typed_bindings, updated_ctx)]
    @raise Compiler_error.Type_error on type errors *)
val infer_bindings :
  infer_expr:expression_infer_fn ->
  infer_expr_expected:expression_infer_expected_fn ->
  Typing_context.t ->
  Parsing.Syntax_tree.recursion_flag ->
  Parsing.Syntax_tree.binding list ->
  Typed_tree.typed_binding list * Typing_context.t
