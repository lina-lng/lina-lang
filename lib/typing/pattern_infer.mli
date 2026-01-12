(** Pattern type inference.

    This module handles type inference for patterns. Patterns introduce bindings
    into the environment and constrain types through matching.

    {2 Supported Patterns}

    - Variable patterns: [x] introduces a fresh type variable for [x]
    - Wildcard patterns: [_] introduces a fresh type variable but no binding
    - Constant patterns: [42], ["hello"], [true] use their literal types
    - Tuple patterns: [(a, b, c)] infers each sub-pattern
    - Constructor patterns: [Some x], [None] look up constructor types
    - Record patterns: [{ x; y }] with punning, [{ x = a; y = b }] with renaming
    - Alias patterns: [pattern as name] binds [name] to the pattern's type
    - Constraint patterns: [(pattern : type)] (currently ignores the constraint)

    {2 Type Inference}

    Pattern inference traverses the pattern structure, generating fresh type
    variables for unknowns and unifying with known types from constructors
    and literals. Bindings are accumulated in the returned context.

    {2 Context Threading}

    The inference functions take a [Typing_context.t] and return an updated
    context. This threads state (level, next variable ID, environment) through
    the inference without relying on global mutable state. *)

(** {1 Pattern Inference with Context} *)

(** [infer_pattern ctx pattern] infers the type of a pattern.

    Traverses the pattern, introducing bindings for variable patterns and
    unifying with known types for constructors and literals.

    @param ctx The typing context
    @param pattern The pattern to infer
    @return A triple [(typed_pattern, pattern_type, updated_ctx)] where
            [typed_pattern] is the typed pattern, [pattern_type] is its type,
            and [updated_ctx] includes any bindings introduced by the pattern *)
val infer_pattern :
  Typing_context.t ->
  Parsing.Syntax_tree.pattern ->
  Typed_tree.typed_pattern * Types.type_expression * Typing_context.t
