(** Expression type inference.

    This module handles type inference for expressions using Algorithm W with
    level-based generalization. It infers types for all expression forms and
    handles let-polymorphism through proper generalization at let-bindings.

    {2 Expression Forms}

    - Variables: Look up type scheme and instantiate
    - Constants: Assign literal type (int, float, string, bool, unit)
    - Tuples: Infer each element, combine into tuple type
    - Constructors: Look up constructor, instantiate, unify with argument
    - Application: Infer function and arguments, build arrow type
    - Functions: Infer parameter patterns, infer body in extended environment
    - Let bindings: Handle recursive and non-recursive bindings with generalization
    - If expressions: Condition must be bool, branches must match
    - Match expressions: Infer scrutinee, patterns, and bodies
    - Records: Infer fields, build row type
    - Module access: Look up value in module signature

    {2 Let-Polymorphism}

    Let bindings receive polymorphic types through generalization:
    - Enter a new level before inferring the binding
    - Infer the binding expression
    - Generalize type variables at the current level
    - Leave the level

    This ensures that type variables created during binding inference can be
    properly generalized while preserving soundness.

    {2 Context Threading}

    All inference functions thread [Typing_context.t] to track:
    - Current generalization level
    - Next type variable ID
    - Typing environment

    Type variables are created via [Typing_context.new_type_variable] which
    returns an updated context with incremented ID. *)

(** {1 Forward References}

    These forward references break circular dependencies between modules.
    They are initialized by Structure_infer. *)

(** Forward reference for module expression inference.
    Set by Structure_infer to break circular dependency. *)
val infer_module_expression_ref :
  (Typing_context.t -> Parsing.Syntax_tree.module_expression ->
   Typed_tree.typed_module_expression * Typing_context.t) ref

(** {1 Path Conversion} *)

(** Convert a module_path (string list) to a Types.path.
    Used by first-class module inference. *)
val module_path_to_types_path :
  Parsing.Syntax_tree.module_path -> Types.path

(** Convert a longident to a Types.path.
    Used by first-class module inference. *)
val longident_to_path :
  Parsing.Syntax_tree.longident -> Types.path

(** {1 Expression Inference} *)

(** [infer_expression ctx expr] infers the type of an expression.

    Recursively traverses the expression structure, inferring types for
    sub-expressions and unifying where constraints exist.

    @param ctx The typing context
    @param expr The expression to infer
    @return A pair [(typed_expr, updated_ctx)]
    @raise Compiler_error.Type_error on type errors *)
val infer_expression :
  Typing_context.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

(** {1 Labeled Argument Reordering} *)

(** [reorder_arguments params args] matches arguments to parameters for type-directed
    argument reordering. Returns a list of slots (filled with argument or needing value)
    and any unused arguments.

    For labeled parameters, matching labeled arguments are found regardless of position.
    For unlabeled parameters, unlabeled arguments are consumed in order.

    Slot types:
    - [`Filled arg]: argument matches parameter directly
    - [`FilledWrapped arg]: labeled arg filling optional param (needs [Some] wrapping)
    - [`OptionalDefault ty]: optional param with no arg (needs [None] default)
    - [`Needed ty]: required param with no arg (partial application)

    @param params Expected parameters as (label, type) pairs
    @param args Provided arguments as (label, typed_expression) pairs
    @return A pair [(slots, unused_args)] where slots are in parameter order *)
val reorder_arguments :
  (Types.arg_label * Types.type_expression) list ->
  (Types.arg_label * Typed_tree.typed_expression) list ->
  (Types.arg_label *
    [> `Filled of Typed_tree.typed_expression
     | `FilledWrapped of Typed_tree.typed_expression
     | `OptionalDefault of Types.type_expression
     | `Needed of Types.type_expression ]) list *
  (Types.arg_label * Typed_tree.typed_expression) list

(** {1 Binding Inference} *)

(** [infer_bindings ctx rec_flag bindings] infers types for a list of bindings.

    For non-recursive bindings, each binding is processed sequentially,
    with each binding's type generalized before adding to the context.

    For recursive bindings, all binding names are first added to the context
    with fresh type variables, then all expressions are inferred, and finally
    types are generalized.

    @param ctx The typing context
    @param rec_flag Whether the bindings are recursive
    @param bindings The list of bindings to infer
    @return A pair [(typed_bindings, updated_ctx)] *)
val infer_bindings :
  Typing_context.t ->
  Parsing.Syntax_tree.recursion_flag ->
  Parsing.Syntax_tree.binding list ->
  Typed_tree.typed_binding list * Typing_context.t
