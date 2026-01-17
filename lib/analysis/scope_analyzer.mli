(** Scope analyzer for typed ASTs.

    This module walks a typed AST and builds a {!Scope.scope_tree} that
    tracks all bindings and their references throughout the program. *)

(** [analyze structure] analyzes a typed structure and returns a scope tree
    containing all bindings and their usage information.

    The analysis:
    - Creates bindings for let bindings, function parameters, pattern variables,
      modules, types, constructors, and record fields
    - Tracks all references to bindings (reads, writes, pattern matches, etc.)
    - Marks bindings that are exported (appear in module signatures)
    - Handles nested scopes correctly (functions, match arms, let expressions) *)
val analyze : Typing.Typed_tree.typed_structure -> Scope.scope_tree

(** [analyze_with_signature structure signature] analyzes a typed structure
    with an explicit signature, marking all bindings that appear in the
    signature as exported. *)
val analyze_with_signature :
  Typing.Typed_tree.typed_structure ->
  Typing.Module_types.signature option ->
  Scope.scope_tree
