(** Diverging call detection.

    Provides functions to identify calls that never return, such as
    [raise], [failwith], [invalid_arg], [exit], and [assert false]. *)

(** List of known diverging function names. *)
val diverging_functions : string list

(** [is_diverging_call expr] returns [true] if [expr] is a call to a known
    diverging function. This includes direct calls like [failwith "error"]
    and module-qualified calls like [Stdlib.exit 1]. *)
val is_diverging_call : Typing.Typed_tree.typed_expression -> bool
