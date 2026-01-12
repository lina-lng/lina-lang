(** Structure and module expression type inference.

    This module handles type inference for structures (top-level declarations)
    and module expressions. It processes:
    - Value bindings (let declarations)
    - Type declarations (type definitions, variants, aliases)
    - Module definitions and bindings
    - Module type definitions
    - Open and include directives
    - Module expressions (struct, path, functor, apply, constraint)

    {2 Structure Inference}

    Structures are processed sequentially, with each item potentially
    modifying the context for subsequent items. Type declarations
    are added before value bindings are processed, allowing forward
    references within a structure.

    {2 Module Expressions}

    Module expressions include:
    - Structures: [struct ... end]
    - Module paths: [M] or [M.N.P]
    - Functors: [functor (X : S) -> ME]
    - Functor application: [F(M)]
    - Constrained modules: [(ME : S)]

    {2 Signature Extraction}

    After inferring a structure, a signature is extracted containing
    all the declarations. This signature can be used for module type
    matching and strengthening.

    Note: Type variables are created using global state (Types.new_type_variable)
    for consistency with other inference modules during the migration period. *)

(** {1 Structure Inference} *)

(** [infer_structure ctx structure] infers types for a complete structure.

    Processes all structure items sequentially, building up the context
    and returning a typed structure with the final context.

    @param ctx The initial typing context
    @param structure The structure to infer
    @return A pair [(typed_structure, updated_ctx)]
    @raise Compiler_error.Type_error on type errors *)
val infer_structure :
  Typing_context.t ->
  Parsing.Syntax_tree.structure ->
  Typed_tree.typed_structure * Typing_context.t

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
  Typed_tree.typed_structure option * Typing_context.t * Inference_utils.inference_error list

(** [infer_structure_item ctx item] infers types for a single structure item.

    @param ctx The typing context
    @param item The structure item to infer
    @return A pair [(typed_item, updated_ctx)]
    @raise Compiler_error.Type_error on type errors *)
val infer_structure_item :
  Typing_context.t ->
  Parsing.Syntax_tree.structure_item ->
  Typed_tree.typed_structure_item * Typing_context.t

(** {1 Module Expression Inference} *)

(** [infer_module_expression ctx mexpr] infers the type of a module expression.

    Handles all module expression forms and performs signature matching
    for functor applications and constraints.

    @param ctx The typing context
    @param mexpr The module expression to infer
    @return A pair [(typed_mexpr, updated_ctx)]
    @raise Compiler_error.Type_error on type errors or signature mismatches *)
val infer_module_expression :
  Typing_context.t ->
  Parsing.Syntax_tree.module_expression ->
  Typed_tree.typed_module_expression * Typing_context.t

(** {1 Signature Extraction} *)

(** [signature_of_typed_structure structure] extracts a signature from
    a typed structure.

    Creates signature items for all structure items that contribute to
    the module's interface.

    @param structure The typed structure
    @return The extracted signature *)
val signature_of_typed_structure :
  Typed_tree.typed_structure ->
  Module_types.signature
