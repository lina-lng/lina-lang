(** Row type utilities.

    Provides common operations on record and poly-variant row types,
    eliminating duplication in row field processing.

    {2 Row Type Structure}

    A row type represents the fields of a record:
    - [row_fields]: list of (name, field_presence) pairs
    - [row_more]: either [TypeRowEmpty] (closed) or a type variable (open)

    Poly-variant rows are similar but for sum types.

    {2 Example Usage}

    {[
      (* Map over all field types *)
      let qualified_row = Row_utils.map_fields (qualify_type path) row

      (* Check if any field contains a weak variable *)
      let has_weak = Row_utils.exists_in_all has_weak_variable row
    ]} *)

open Types

(** {1 Row Field Operations} *)

(** [map_fields f row] applies [f] to each field type in the row.

    Preserves field names and structure. Uses physical equality
    optimization to avoid allocation when unchanged. *)
val map_fields : (type_expression -> type_expression) -> row -> row

(** [fold_fields f acc row] folds over all field types in the row.

    Does NOT include the row tail ([row_more]) - use {!fold_all} for that. *)
val fold_fields : ('a -> type_expression -> 'a) -> 'a -> row -> 'a

(** [fold_all f acc row] folds over all types in the row including the tail. *)
val fold_all : ('a -> type_expression -> 'a) -> 'a -> row -> 'a

(** [iter_fields f row] applies [f] to each field type. *)
val iter_fields : (type_expression -> unit) -> row -> unit

(** [iter_all f row] applies [f] to each field type and the tail. *)
val iter_all : (type_expression -> unit) -> row -> unit

(** {1 Row Predicates} *)

(** [exists_in_fields pred row] returns true if any field type satisfies [pred]. *)
val exists_in_fields : (type_expression -> bool) -> row -> bool

(** [exists_in_all pred row] returns true if any type (fields or tail) satisfies [pred]. *)
val exists_in_all : (type_expression -> bool) -> row -> bool

(** [is_closed row] returns true if the row is closed (tail is [TypeRowEmpty]). *)
val is_closed : row -> bool

(** [is_open row] returns true if the row is open (tail is a variable). *)
val is_open : row -> bool

(** {1 Row Construction} *)

(** [close row] returns a closed version of the row (replaces tail with [TypeRowEmpty]). *)
val close : row -> row

(** [extend ~name ~ty row] adds a field to the row. *)
val extend : name:string -> ty:type_expression -> row -> row

(** [get_field name row] returns the type of a field if present. *)
val get_field : string -> row -> type_expression option

(** [field_names row] returns the list of field names in order. *)
val field_names : row -> string list

(** {1 Poly-Variant Row Operations} *)

(** [pv_map_fields f pv_row] applies [f] to each present field type in a poly-variant row. *)
val pv_map_fields : (type_expression -> type_expression) -> poly_variant_row -> poly_variant_row

(** [pv_fold_fields f acc pv_row] folds over present field types (non-None). *)
val pv_fold_fields : ('a -> type_expression -> 'a) -> 'a -> poly_variant_row -> 'a

(** [pv_exists_in_fields pred pv_row] checks if any present field satisfies [pred]. *)
val pv_exists_in_fields : (type_expression -> bool) -> poly_variant_row -> bool
