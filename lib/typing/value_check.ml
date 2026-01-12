(** Value restriction checking.

    Implements Wright's value restriction to ensure soundness of polymorphic
    references in ML-family type systems.

    See value_check.mli for detailed documentation. *)

(** {1 Functor-Based Value Checking}

    Uses a functor to eliminate duplication between typed and syntax expressions. *)

(** Classification of expressions for value checking.
    This type abstracts over the differences between typed and syntax ASTs. *)
type 'expr value_shape =
  | ShapeAtomicValue
      (** Variable, constant, or function - always a value *)
  | ShapeTuple of 'expr list
      (** Tuple - value if all components are values *)
  | ShapeConstructor of 'expr option
      (** Constructor - None for nullary, Some for with argument *)
  | ShapeRecord of 'expr list
      (** Record - list of field values *)
  | ShapeLet of 'expr list * 'expr
      (** Let binding - binding expressions and body *)
  | ShapeConstraint of 'expr
      (** Type constraint - inner expression (syntax AST only) *)
  | ShapeNonValue
      (** Apply, match, if, sequence, etc. - never a value *)

(** Module signature for expression types that can be checked for value-ness. *)
module type EXPRESSION = sig
  (** The expression type *)
  type t

  (** Classify an expression into its value shape *)
  val classify : t -> t value_shape
end

(** Functor that generates is_value from an EXPRESSION implementation. *)
module Make (E : EXPRESSION) = struct
  let rec is_value (expr : E.t) : bool =
    match E.classify expr with
    | ShapeAtomicValue -> true
    | ShapeTuple components -> List.for_all is_value components
    | ShapeConstructor None -> true
    | ShapeConstructor (Some arg) -> is_value arg
    | ShapeRecord field_values -> List.for_all is_value field_values
    | ShapeLet (binding_exprs, body) ->
      List.for_all is_value binding_exprs && is_value body
    | ShapeConstraint inner -> is_value inner
    | ShapeNonValue -> false
end

(** {2 Typed Expression Implementation} *)

module TypedExpression : EXPRESSION with type t = Typed_tree.typed_expression = struct
  type t = Typed_tree.typed_expression

  let classify (expr : t) : t value_shape =
    match expr.Typed_tree.expression_desc with
    (* Atomic values *)
    | Typed_tree.TypedExpressionVariable _ -> ShapeAtomicValue
    | Typed_tree.TypedExpressionConstant _ -> ShapeAtomicValue
    | Typed_tree.TypedExpressionFunction _ -> ShapeAtomicValue

    (* Compound values *)
    | Typed_tree.TypedExpressionTuple components -> ShapeTuple components
    | Typed_tree.TypedExpressionConstructor (_, arg) -> ShapeConstructor arg
    | Typed_tree.TypedExpressionRecord fields ->
      ShapeRecord (List.map (fun field -> field.Typed_tree.typed_field_value) fields)
    | Typed_tree.TypedExpressionLet (_, bindings, body) ->
      let binding_exprs = List.map
        (fun (binding : Typed_tree.typed_binding) -> binding.Typed_tree.binding_expression)
        bindings
      in
      ShapeLet (binding_exprs, body)

    (* Non-values *)
    | Typed_tree.TypedExpressionApply _ -> ShapeNonValue
    | Typed_tree.TypedExpressionMatch _ -> ShapeNonValue
    | Typed_tree.TypedExpressionIf _ -> ShapeNonValue
    | Typed_tree.TypedExpressionSequence _ -> ShapeNonValue
    | Typed_tree.TypedExpressionRecordAccess _ -> ShapeNonValue
    | Typed_tree.TypedExpressionRecordUpdate _ -> ShapeNonValue
    | Typed_tree.TypedExpressionModuleAccess _ -> ShapeNonValue
    | Typed_tree.TypedExpressionRef _ -> ShapeNonValue
    | Typed_tree.TypedExpressionDeref _ -> ShapeNonValue
    | Typed_tree.TypedExpressionAssign _ -> ShapeNonValue
end

(** {2 Syntax Expression Implementation} *)

module SyntaxExpression : EXPRESSION with type t = Parsing.Syntax_tree.expression = struct
  type t = Parsing.Syntax_tree.expression

  let classify (expr : t) : t value_shape =
    match expr.Common.Location.value with
    (* Atomic values *)
    | Parsing.Syntax_tree.ExpressionVariable _ -> ShapeAtomicValue
    | Parsing.Syntax_tree.ExpressionConstant _ -> ShapeAtomicValue
    | Parsing.Syntax_tree.ExpressionFunction _ -> ShapeAtomicValue

    (* Compound values *)
    | Parsing.Syntax_tree.ExpressionTuple components -> ShapeTuple components
    | Parsing.Syntax_tree.ExpressionConstructor (_, arg) -> ShapeConstructor arg
    | Parsing.Syntax_tree.ExpressionRecord fields ->
      ShapeRecord (List.map (fun field -> field.Parsing.Syntax_tree.field_value) fields)
    | Parsing.Syntax_tree.ExpressionLet (_, bindings, body) ->
      let binding_exprs = List.map
        (fun (binding : Parsing.Syntax_tree.binding) -> binding.Parsing.Syntax_tree.binding_expression)
        bindings
      in
      ShapeLet (binding_exprs, body)

    (* Type constraint - syntax only, transparent for value checking *)
    | Parsing.Syntax_tree.ExpressionConstraint (inner, _) ->
      ShapeConstraint inner

    (* Non-values *)
    | Parsing.Syntax_tree.ExpressionApply _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionMatch _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionIf _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionSequence _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionRecordAccess _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionRecordUpdate _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionModuleAccess _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionRef _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionDeref _ -> ShapeNonValue
    | Parsing.Syntax_tree.ExpressionAssign _ -> ShapeNonValue
end

(** {2 Instantiate the Functor} *)

module TypedCheck = Make (TypedExpression)
module SyntaxCheck = Make (SyntaxExpression)

(** [is_value expression] returns [true] if the typed expression is a
    syntactic value (non-expansive) and can be safely generalized. *)
let is_value = TypedCheck.is_value

(** [is_syntax_value expression] returns [true] if the surface syntax
    expression is a syntactic value. *)
let is_syntax_value = SyntaxCheck.is_value

(** {1 Relaxed Value Restriction - Variance Checking} *)

(** Re-export variance type from Types for backward compatibility. *)
type variance = Types.variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

(** Check the variance of a type variable in a type expression.
    Delegates to {!Variance.check_in_type}. *)
let check_variance = Variance.check_in_type

(** Check if a type variable can be generalized under relaxed value restriction.
    A variable can be generalized if it appears only in covariant positions
    or does not appear at all. *)
let can_generalize_relaxed (target_var : Types.type_variable) (ty : Types.type_expression) : bool =
  match check_variance target_var ty with
  | Covariant | Bivariant -> true
  | Contravariant | Invariant -> false
