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
