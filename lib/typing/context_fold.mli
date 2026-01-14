(** Context-threading fold utilities.

    Provides common patterns for threading a context (like {!Typing_context.t})
    through list operations while accumulating results.

    {2 Common Patterns}

    Type inference frequently needs to:
    - Process a list of items
    - Thread a context through each processing step
    - Collect results in order

    This module provides efficient implementations of these patterns,
    using reverse accumulation internally for O(n) complexity.

    {2 Example Usage}

    {[
      (* Before - manual fold *)
      let typed_items, ctx =
        List.fold_left (fun (items, ctx) item ->
          let typed_item, ctx = infer_item ctx item in
          (typed_item :: items, ctx)
        ) ([], ctx) items
      in
      let typed_items = List.rev typed_items in

      (* After - using Context_fold *)
      let typed_items, ctx = Context_fold.fold_map infer_item ctx items
    ]} *)

(** {1 Fold with Context} *)

(** [fold_map f ctx list] processes each element while threading context.

    For each element, calls [f ctx element] which returns [(result, new_ctx)].
    Collects results in order while threading the context through.

    @param f Processing function that takes context and element
    @param ctx Initial context
    @param list Elements to process
    @return [(results, final_ctx)] where results are in the same order as input *)
val fold_map : ('ctx -> 'a -> 'b * 'ctx) -> 'ctx -> 'a list -> 'b list * 'ctx

(** [fold_map_rev f ctx list] like {!fold_map} but returns results in reverse order.

    More efficient when order doesn't matter or you'll reverse later anyway. *)
val fold_map_rev : ('ctx -> 'a -> 'b * 'ctx) -> 'ctx -> 'a list -> 'b list * 'ctx

(** [fold_filter_map f ctx list] processes elements, optionally producing results.

    Like {!fold_map} but [f] returns [option] - [None] results are filtered out. *)
val fold_filter_map : ('ctx -> 'a -> 'b option * 'ctx) -> 'ctx -> 'a list -> 'b list * 'ctx

(** {1 Multiple Result Lists} *)

(** [fold_map2 f ctx list] threads context and produces two parallel lists.

    Useful for cases where processing produces two related outputs
    (e.g., typed expression and its type). *)
val fold_map2 : ('ctx -> 'a -> 'b * 'c * 'ctx) -> 'ctx -> 'a list -> 'b list * 'c list * 'ctx

(** [fold_map3 f ctx list] threads context and produces three parallel lists.

    Useful for pattern inference where we need typed patterns, types, and bindings. *)
val fold_map3 : ('ctx -> 'a -> 'b * 'c * 'd * 'ctx) -> 'ctx -> 'a list -> 'b list * 'c list * 'd list * 'ctx

(** {1 Indexed Variants} *)

(** [fold_mapi f ctx list] like {!fold_map} but [f] also receives the index. *)
val fold_mapi : ('ctx -> int -> 'a -> 'b * 'ctx) -> 'ctx -> 'a list -> 'b list * 'ctx

(** {1 Simple Context Threading} *)

(** [fold_ctx f ctx list] threads context without accumulating results.

    Useful when you only care about the final context (e.g., adding bindings). *)
val fold_ctx : ('ctx -> 'a -> 'ctx) -> 'ctx -> 'a list -> 'ctx

(** [iter_ctx f ctx list] like {!fold_ctx} but [f] returns unit.

    Useful for side-effecting operations that modify context via mutation. *)
val iter_ctx : ('ctx -> 'a -> unit) -> 'ctx -> 'a list -> unit
