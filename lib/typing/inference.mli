(** Type inference for Lina programs.

    This module implements Hindley-Milner type inference with extensions for:
    - Row polymorphism (structural records)
    - Pattern matching with exhaustiveness checking
    - Module system with functors and signatures

    Type inference uses Algorithm W with level-based generalization for
    efficient let-polymorphism. The algorithm works in two phases:
    1. Expression type inference: assign types to all expressions
    2. Structure inference: process type and module declarations

    Type variables are created using context-based state threading via
    [Typing_context.new_type_variable].

    @see <https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system>
         Hindley-Milner type system *)

(** {1 Expression Inference} *)

(** [infer_expression ctx expr] infers the type of an expression.

    Traverses the expression AST, assigns type variables to unknowns,
    and uses unification to solve constraints. The returned typed expression
    has full type annotations.

    @param ctx The typing context containing bindings
    @param expr The expression to type-check
    @return A pair [(typed_expr, updated_ctx)]
    @raise Compiler_error.Type_error on type mismatches
    @raise Unification.Unification_error on unification failures *)
val infer_expression :
  Typing_context.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

(** {1 Structure Inference} *)

(** [infer_structure ctx structure] infers types for a complete structure.

    Processes all structure items (let bindings, type definitions, modules)
    and returns a typed structure along with the updated context.

    Type definitions are added to the context before processing bindings,
    allowing forward references within a structure.

    @param ctx The initial typing context
    @param structure The structure to type-check
    @return A pair of (typed_structure, updated_context)
    @raise Compiler_error.Type_error on type errors
    @raise Unification.Unification_error on unification failures *)
val infer_structure :
  Typing_context.t ->
  Parsing.Syntax_tree.structure ->
  Typed_tree.typed_structure * Typing_context.t

(** Unification error details for tolerant inference. *)
type unification_error_details = {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Common.Location.t;
  message : string;
}

(** Error information from tolerant inference. *)
type inference_error =
  | CompilerError of Common.Compiler_error.t
  | UnificationError of unification_error_details

(** [infer_structure_tolerant ctx structure] infers types for a structure,
    continuing after errors to accumulate as much context as possible.

    This is used by LSP features like completion that need the environment
    even when the code has errors (e.g., incomplete expressions being typed).

    Unlike {!infer_structure}, this function catches type errors per structure
    item and continues processing subsequent items with the accumulated
    context.

    @param ctx The initial typing context
    @param structure The structure to infer
    @return A triple [(typed_structure_opt, accumulated_ctx, errors)] where
            [typed_structure_opt] is [Some ast] if all items succeeded,
            [None] if any item failed, and [errors] is the list of errors *)
val infer_structure_tolerant :
  Typing_context.t ->
  Parsing.Syntax_tree.structure ->
  Typed_tree.typed_structure option * Typing_context.t * inference_error list
