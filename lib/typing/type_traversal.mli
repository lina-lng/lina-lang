(** Generic type traversal utilities.

    This module provides reusable traversal patterns for type expressions,
    eliminating duplicate [collect]/[copy] functions throughout the codebase.

    {2 Design}

    Type traversal follows the structure of [Types.type_expression]:
    - [TypeVariable]: leaf node
    - [TypeConstructor]: traverse arguments
    - [TypeTuple]: traverse elements
    - [TypeArrow]: traverse argument and result
    - [TypeRecord]: traverse row fields and tail
    - [TypeRowEmpty]: leaf node

    All traversals use [Types.representative] to follow union-find links.

    {2 Usage}

    For collecting type variables (like generalization):
    {[
      let vars = ref [] in
      iter (function
        | TypeVariable tv when tv.level > current -> vars := tv :: !vars
        | _ -> ()
      ) ty;
      !vars
    ]}

    For copying types with substitution (like instantiation):
    {[
      map (function
        | TypeVariable tv ->
          (match List.assoc_opt tv.id subst with
           | Some fresh -> fresh
           | None -> TypeVariable tv)
        | ty -> ty
      ) ty
    ]} *)

open Types

(** {1 Iteration} *)

(** [iter f ty] applies [f] to [ty] and all its sub-types (depth-first, pre-order).

    The function [f] receives the representative of each type node.
    Follows union-find links via [Types.representative].

    @param f Function to apply to each type node
    @param ty The type expression to traverse *)
val iter : (type_expression -> unit) -> type_expression -> unit

(** [iter_row f row] applies [f] to all types within a row.

    Traverses all field types and the row tail.

    @param f Function to apply to each type
    @param row The row to traverse *)
val iter_row : (type_expression -> unit) -> row -> unit

(** {1 Mapping} *)

(** [map f ty] transforms [ty] by applying [f] to each sub-type (bottom-up).

    The function [f] receives the already-transformed type node.
    Use this for type substitution and copying.

    @param f Transformation function for each type node
    @param ty The type expression to transform
    @return The transformed type expression *)
val map : (type_expression -> type_expression) -> type_expression -> type_expression

(** [map_row f row] transforms all types within a row.

    @param f Transformation function for each type
    @param row The row to transform
    @return The transformed row *)
val map_row : (type_expression -> type_expression) -> row -> row

(** {1 Folding} *)

(** [fold f acc ty] folds over [ty] and all its sub-types (depth-first, pre-order).

    @param f Folding function: [f acc sub_type]
    @param acc Initial accumulator value
    @param ty The type expression to fold over
    @return Final accumulated value *)
val fold : ('a -> type_expression -> 'a) -> 'a -> type_expression -> 'a

(** [fold_row f acc row] folds over all types within a row.

    @param f Folding function
    @param acc Initial accumulator value
    @param row The row to fold over
    @return Final accumulated value *)
val fold_row : ('a -> type_expression -> 'a) -> 'a -> row -> 'a

(** {1 Context-Aware Traversal} *)

(** [fold_with_context f ctx acc ty] folds over [ty] with context transformation.

    The function [f] receives the current context and can return a modified
    context for child nodes. This is useful for variance inference where the
    context (covariant/contravariant) changes when entering function arguments.

    @param f Function [f ctx ty acc] returns [(new_ctx, new_acc)]
    @param ctx Initial context
    @param acc Initial accumulator
    @param ty The type to traverse
    @return Final accumulated value *)
val fold_with_context :
  ('ctx -> type_expression -> 'acc -> 'ctx * 'acc) ->
  'ctx -> 'acc -> type_expression -> 'acc

(** [fold_row_with_context f ctx acc row] folds over a row with context.

    @param f Context-aware folding function
    @param ctx Current context
    @param acc Current accumulator
    @param row The row to traverse
    @return Final accumulated value *)
val fold_row_with_context :
  ('ctx -> type_expression -> 'acc -> 'ctx * 'acc) ->
  'ctx -> 'acc -> row -> 'acc

(** {1 Type Variable Operations} *)

(** [collect_variables predicate ty] collects all type variables in [ty]
    that satisfy [predicate].

    Variables are deduplicated by ID and returned in traversal order.

    @param predicate Test function for variables
    @param ty The type to search
    @return List of matching type variables (deduplicated) *)
val collect_variables :
  (type_variable -> bool) -> type_expression -> type_variable list

(** [exists_variable predicate ty] checks if any type variable in [ty]
    satisfies [predicate].

    Short-circuits on first match for efficiency.

    @param predicate Test function for variables
    @param ty The type to search
    @return [true] if any variable matches, [false] otherwise *)
val exists_variable : (type_variable -> bool) -> type_expression -> bool

(** [for_all_variables predicate ty] checks if all type variables in [ty]
    satisfy [predicate].

    @param predicate Test function for variables
    @param ty The type to check
    @return [true] if all variables match (or no variables), [false] otherwise *)
val for_all_variables : (type_variable -> bool) -> type_expression -> bool

(** {1 Common Variable Collections} *)

(** [free_type_variables ty] returns all type variables in [ty].

    Variables are deduplicated by ID.

    @param ty The type expression
    @return List of all type variables *)
val free_type_variables : type_expression -> type_variable list

(** [free_type_variables_above_level level ty] returns type variables
    with level strictly greater than [level].

    This is useful for generalization: variables above the current level
    can be generalized.

    @param level The threshold level
    @param ty The type expression
    @return List of variables with level > [level] *)
val free_type_variables_above_level : int -> type_expression -> type_variable list

(** {1 Weak Variable Detection} *)

(** [has_weak_variables ty] returns true if [ty] contains any weak type variables.

    Weak variables are non-generalizable and cannot satisfy polymorphic signatures.
    This is used to detect weak type escape at module boundaries.

    @param ty The type to check
    @return [true] if any type variable in [ty] is marked weak *)
val has_weak_variables : type_expression -> bool

(** {1 Path Qualification} *)

(** [qualify_path_local ~type_names ~prefix ty] qualifies [PathLocal] references
    in [ty] with the given [prefix].

    Only type names in [type_names] are qualified. Other [PathLocal] references
    are left unchanged. This is used for module strengthening to replace local
    type references with qualified paths.

    Example: [qualify_path_local ~type_names:["t"] ~prefix:(PathIdent m) (PathLocal "t")]
    returns [PathDot (PathIdent m, "t")].

    @param type_names List of type names defined in the current signature
    @param prefix The path prefix to add to matching [PathLocal] references
    @param ty The type to transform
    @return The type with qualified paths *)
val qualify_path_local :
  type_names:string list -> prefix:path -> type_expression -> type_expression
