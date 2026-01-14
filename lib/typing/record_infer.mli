(** Record type inference.

    This module handles type inference for record expressions:
    - Record literals: [{ x = 1; y = 2 }]
    - Record field access: [r.x] (including module access disambiguation)
    - Record update: [{ r with x = 10 }]

    {2 Module Access Disambiguation}

    When a record access expression looks like [M.N.x], the module checks if
    [M.N] forms a valid module path. If so, it's treated as module access
    rather than record field access. This allows the same syntax for both
    record access and module value access. *)

(** {1 Callback Types} *)

(** Expression inference function type for callback. *)
type expression_infer_fn =
  Typing_context.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

(** {1 Record Inference Functions} *)

(** [infer_record ~infer_expr ctx loc record_fields] infers the type of a record literal.

    Checks for duplicate fields, infers types for each field value, and builds
    a closed row type from the inferred field types.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param record_fields The list of record fields
    @return A pair [(typed_expr, updated_ctx)]
    @raise Compiler_error.Type_error if duplicate fields are found *)
val infer_record :
  infer_expr:expression_infer_fn ->
  Typing_context.t ->
  Common.Location.t ->
  Parsing.Syntax_tree.record_field list ->
  Typed_tree.typed_expression * Typing_context.t

(** [infer_record_access ~infer_expr ctx loc record_expr field_name] infers the type
    of a record field access expression.

    This handles both normal record access ([r.x]) and module access disambiguation
    when the expression looks like [M.N.x] where [M.N] is a module path.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param record_expr The record expression being accessed
    @param field_name The name of the field to access
    @return A pair [(typed_expr, updated_ctx)]
    @raise Compiler_error.Type_error if field not found in module *)
val infer_record_access :
  infer_expr:expression_infer_fn ->
  Typing_context.t ->
  Common.Location.t ->
  Parsing.Syntax_tree.expression ->
  string ->
  Typed_tree.typed_expression * Typing_context.t

(** [infer_record_update ~infer_expr ctx loc base_expr update_fields] infers the type
    of a record update expression.

    Infers the type of the base record and each update field, then unifies the
    base record type with an open row type containing the update fields.

    @param infer_expr The expression inference callback
    @param ctx The typing context
    @param loc The source location
    @param base_expr The base record expression
    @param update_fields The list of fields to update
    @return A pair [(typed_expr, updated_ctx)] *)
val infer_record_update :
  infer_expr:expression_infer_fn ->
  Typing_context.t ->
  Common.Location.t ->
  Parsing.Syntax_tree.expression ->
  Parsing.Syntax_tree.record_field list ->
  Typed_tree.typed_expression * Typing_context.t
