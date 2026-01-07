(** Forward references for mutual recursion across inference modules.

    When inference.ml is split into multiple modules (pattern_infer, expression_infer,
    structure_infer, module_type_check), they have circular dependencies:
    - Expression inference needs pattern inference for match expressions
    - Structure inference needs expression inference for let bindings
    - Module expression inference needs structure inference for struct...end

    This module provides mutable references that are set during initialization,
    allowing the split modules to call each other without direct dependencies.

    {b Important}: All hooks must be initialized before any inference is performed.
    The {!Inference} facade module handles this initialization. *)

(** {1 Expression Inference Hook} *)

(** Hook type for expression inference. *)
type expression_hook =
  Environment.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression

(** [infer_expression env expr] infers the type of an expression.
    @raise Failure if hook is not initialized *)
val infer_expression : expression_hook

(** [set_infer_expression hook] sets the expression inference function. *)
val set_infer_expression : expression_hook -> unit

(** {1 Pattern Inference Hook} *)

(** Hook type for pattern inference.
    Returns (typed_pattern, pattern_type, updated_environment). *)
type pattern_hook =
  Environment.t ->
  Parsing.Syntax_tree.pattern ->
  Typed_tree.typed_pattern * Types.type_expression * Environment.t

(** [infer_pattern env pattern] infers the type of a pattern.
    @raise Failure if hook is not initialized *)
val infer_pattern : pattern_hook

(** [set_infer_pattern hook] sets the pattern inference function. *)
val set_infer_pattern : pattern_hook -> unit

(** {1 Structure Inference Hook} *)

(** Hook type for structure inference.
    Returns (typed_structure, updated_environment). *)
type structure_hook =
  Environment.t ->
  Parsing.Syntax_tree.structure ->
  Typed_tree.typed_structure * Environment.t

(** [infer_structure env structure] infers types for a structure.
    @raise Failure if hook is not initialized *)
val infer_structure : structure_hook

(** [set_infer_structure hook] sets the structure inference function. *)
val set_infer_structure : structure_hook -> unit

(** {1 Module Expression Inference Hook} *)

(** Hook type for module expression inference. *)
type module_expr_hook =
  Environment.t ->
  Parsing.Syntax_tree.module_expression ->
  Typed_tree.typed_module_expression * Environment.t

(** [infer_module_expression env mexpr] infers the type of a module expression.
    @raise Failure if hook is not initialized *)
val infer_module_expression : module_expr_hook

(** [set_infer_module_expression hook] sets the module expression inference function. *)
val set_infer_module_expression : module_expr_hook -> unit

(** {1 Module Type Checking Hook} *)

(** Hook type for module type checking. *)
type module_type_hook =
  Environment.t ->
  Parsing.Syntax_tree.module_type ->
  Module_types.module_type

(** [check_module_type env mty] checks a module type.
    @raise Failure if hook is not initialized *)
val check_module_type : module_type_hook

(** [set_check_module_type hook] sets the module type checking function. *)
val set_check_module_type : module_type_hook -> unit
