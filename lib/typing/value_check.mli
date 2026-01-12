(** Value restriction checking.

    Determines whether an expression is a "syntactic value" (non-expansive)
    and can therefore be safely generalized in let-bindings.

    {2 Background}

    Without value restriction, polymorphic references would be unsound:
    {[
      let reference = ref []  (* Would be: 'a list ref *)
      reference := [1]        (* Instantiate 'a = int *)
      !reference @ ["hello"]  (* Instantiate 'a = string - CRASH *)
    ]}

    The value restriction ensures that only "syntactic values" (expressions
    that cannot create new mutable state) are generalized.

    {2 Syntactic Values}

    A syntactic value is an expression that:
    - Cannot allocate new mutable storage
    - Cannot perform side effects during evaluation

    Examples:
    - Variables: [x], [foo]
    - Constants: [42], ["hello"], [true]
    - Lambda abstractions: [fun x -> x + 1]
    - Constructors applied to values: [Some 42], [None]
    - Tuples of values: [(1, 2, 3)]
    - Records of values: [{ x = 1; y = 2 }]

    Non-values (expansive expressions):
    - Function applications: [f x], [ref []]
    - Match expressions: [match x with ...]
    - If expressions: [if b then e1 else e2]
    - Sequences: [e1; e2]

    {2 References}

    - Wright, A.K. "Simple Imperative Polymorphism" (1995)
    - Garrigue, J. "Relaxing the Value Restriction" (2004) *)

(** [is_value expression] returns [true] if the typed expression is a
    syntactic value (non-expansive) and can be safely generalized.

    @param expression The typed expression to check
    @return [true] if the expression is a syntactic value *)
val is_value : Typed_tree.typed_expression -> bool

(** [is_syntax_value expression] returns [true] if the surface syntax
    expression is a syntactic value. This is used for early detection
    before type inference.

    @param expression The surface syntax expression to check
    @return [true] if the expression is a syntactic value *)
val is_syntax_value : Parsing.Syntax_tree.expression -> bool

(** {2 Relaxed Value Restriction}

    Garrigue's relaxed value restriction (2004) allows generalizing type
    variables that appear only in covariant positions, even for non-values.

    The intuition is that variables in covariant-only positions can only be
    "read" (appear in outputs), never "written" (appear in inputs). Therefore,
    even if the expression creates mutable state, different instantiations
    cannot cause type confusion.

    Example:
    {[
      let x = (fun y -> y) []  (* Application - not a value *)
      (* Type: 'a list *)
      (* 'a appears only in list (covariant), so can be generalized *)
    ]}

    Counter-example:
    {[
      let r = ref []  (* Application - not a value *)
      (* Type: 'a list ref *)
      (* 'a appears in ref (invariant), so CANNOT be generalized *)
    ]} *)

(** Variance of a type variable in a type expression.

    - [Covariant]: Variable appears only in "output" positions
    - [Contravariant]: Variable appears only in "input" positions
    - [Invariant]: Variable appears in both positions (unsafe to generalize)
    - [Bivariant]: Variable does not appear (vacuously safe) *)
type variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant

(** [check_variance type_var ty] computes the variance of [type_var] in [ty].

    Variance rules:
    - In [a -> b]: [a] is contravariant, [b] is covariant
    - In [(a, b, ...)]: all elements are covariant
    - In [a option], [a list]: argument is covariant (assuming standard types)
    - In [a ref]: argument is invariant (both read and written)

    @param type_var The type variable to check
    @param ty The type expression to analyze
    @return The variance of [type_var] in [ty] *)
val check_variance : Types.type_variable -> Types.type_expression -> variance

(** [can_generalize_relaxed type_var ty] returns [true] if [type_var] can be
    generalized under the relaxed value restriction.

    A variable can be generalized if it appears only in covariant positions
    or does not appear at all (bivariant).

    @param type_var The type variable to check
    @param ty The type expression containing the variable
    @return [true] if safe to generalize under relaxed rules *)
val can_generalize_relaxed : Types.type_variable -> Types.type_expression -> bool
