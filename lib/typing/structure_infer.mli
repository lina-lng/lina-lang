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
    modifying the environment for subsequent items. Type declarations
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
    matching and strengthening. *)

(** {1 Structure Inference} *)

(** [infer_structure env structure] infers types for a complete structure.

    Processes all structure items sequentially, building up the environment
    and returning a typed structure with the final environment.

    @param env The initial typing environment
    @param structure The structure to infer
    @return A pair [(typed_structure, updated_env)]
    @raise Compiler_error.Type_error on type errors *)
val infer_structure :
  Environment.t ->
  Parsing.Syntax_tree.structure ->
  Typed_tree.typed_structure * Environment.t

(** [infer_structure_item env item] infers types for a single structure item.

    @param env The typing environment
    @param item The structure item to infer
    @return A pair [(typed_item, updated_env)]
    @raise Compiler_error.Type_error on type errors *)
val infer_structure_item :
  Environment.t ->
  Parsing.Syntax_tree.structure_item ->
  Typed_tree.typed_structure_item * Environment.t

(** {1 Module Expression Inference} *)

(** [infer_module_expression env mexpr] infers the type of a module expression.

    Handles all module expression forms and performs signature matching
    for functor applications and constraints.

    @param env The typing environment
    @param mexpr The module expression to infer
    @return A pair [(typed_mexpr, updated_env)]
    @raise Compiler_error.Type_error on type errors or signature mismatches *)
val infer_module_expression :
  Environment.t ->
  Parsing.Syntax_tree.module_expression ->
  Typed_tree.typed_module_expression * Environment.t

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
