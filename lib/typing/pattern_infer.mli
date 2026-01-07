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
    and literals. Bindings are accumulated in the returned environment. *)

(** {1 Pattern Inference} *)

(** [infer_pattern env pattern] infers the type of a pattern.

    Traverses the pattern, introducing bindings for variable patterns and
    unifying with known types for constructors and literals.

    @param env The typing environment
    @param pattern The pattern to infer
    @return A triple [(typed_pattern, pattern_type, updated_env)] where
            [typed_pattern] is the typed pattern, [pattern_type] is its type,
            and [updated_env] includes any bindings introduced by the pattern *)
val infer_pattern :
  Environment.t ->
  Parsing.Syntax_tree.pattern ->
  Typed_tree.typed_pattern * Types.type_expression * Environment.t
