(** Shared AST traversal utilities for LSP features. *)

open Typing.Typed_tree

type node_at_position =
  | ExpressionNode of typed_expression
  | PatternNode of typed_pattern

let offset_in_location offset (loc : Common.Location.t) =
  loc.start_pos.offset <= offset && offset < loc.end_pos.offset

let find_first searcher items = List.find_map searcher items

let try_searches searches = List.find_map (fun search -> search ()) searches

let rec find_pattern_at offset (pat : typed_pattern) =
  if not (offset_in_location offset pat.pattern_location) then None
  else

  let nested_pattern = match pat.pattern_desc with
    (* Compound patterns - recurse into children *)
    | TypedPatternTuple patterns ->
        find_first (find_pattern_at offset) patterns
    | TypedPatternConstructor (_, Some arg) ->
        find_pattern_at offset arg
    | TypedPatternRecord (fields, _) ->
        find_first
          (fun field -> find_pattern_at offset field.typed_pattern_field_pattern)
          fields
    | TypedPatternPolyVariant (_, Some arg) ->
        find_pattern_at offset arg
    | TypedPatternAlias (inner, _) ->
        find_pattern_at offset inner
    | TypedPatternOr (left, right) ->
        try_searches [
          (fun () -> find_pattern_at offset left);
          (fun () -> find_pattern_at offset right);
        ]

    (* Terminal patterns - no children to search *)
    | TypedPatternVariable _
    | TypedPatternWildcard
    | TypedPatternConstant _
    | TypedPatternConstructor (_, None)
    | TypedPatternLocallyAbstract _
    | TypedPatternPolyVariant (_, None)
    | TypedPatternError _ ->
        None
  in

  match nested_pattern with
  | Some _ -> nested_pattern
  | None -> Some pat

let pattern_as_node pattern = Option.map (fun pat -> PatternNode pat) pattern

let find_pattern_in_list offset patterns =
  find_first (fun pat -> pattern_as_node (find_pattern_at offset pat)) patterns

let find_pattern_in_bindings offset bindings =
  find_first
    (fun (binding : typed_binding) ->
      pattern_as_node (find_pattern_at offset binding.binding_pattern))
    bindings

let rec find_node_at offset (expr : typed_expression) =
  if not (offset_in_location offset expr.expression_location) then None
  else

  let nested_node = search_children offset expr in

  match nested_node with
  | Some _ -> nested_node
  | None -> Some (ExpressionNode expr)

and search_children offset expr =
  match expr.expression_desc with
  (* Simple compound expressions *)
  | TypedExpressionTuple exprs ->
      find_first (find_node_at offset) exprs
  | TypedExpressionConstructor (_, Some arg) ->
      find_node_at offset arg
  | TypedExpressionPolyVariant (_, Some arg) ->
      find_node_at offset arg

  (* Application and function definition *)
  | TypedExpressionApply (func, labeled_args) ->
      try_searches [
        (fun () -> find_node_at offset func);
        (fun () -> find_first (fun (_, e) -> find_node_at offset e) labeled_args);
      ]
  | TypedExpressionPartialApply { partial_func; partial_slots } ->
      let filled_exprs = List.filter_map (fun (_, slot) ->
        match slot with
        | SlotFilled expr -> Some expr
        | SlotNeeded _ -> None
      ) partial_slots in
      try_searches [
        (fun () -> find_node_at offset partial_func);
        (fun () -> find_first (fun e -> find_node_at offset e) filled_exprs);
      ]
  | TypedExpressionFunction (labeled_params, body) ->
      try_searches [
        (fun () -> find_pattern_in_list offset (List.map snd labeled_params));
        (fun () -> find_node_at offset body);
      ]

  (* Binding and control flow *)
  | TypedExpressionLet (_, bindings, body) ->
      try_searches [
        (fun () -> find_pattern_in_bindings offset bindings);
        (fun () -> find_in_binding_expressions offset bindings);
        (fun () -> find_node_at offset body);
      ]
  | TypedExpressionIf (cond, then_branch, else_branch) ->
      try_searches [
        (fun () -> find_node_at offset cond);
        (fun () -> find_node_at offset then_branch);
        (fun () -> Option.bind else_branch (find_node_at offset));
      ]
  | TypedExpressionSequence (first, second) ->
      try_searches [
        (fun () -> find_node_at offset first);
        (fun () -> find_node_at offset second);
      ]
  | TypedExpressionMatch (scrutinee, arms) ->
      try_searches [
        (fun () -> find_node_at offset scrutinee);
        (fun () -> find_in_match_arms offset arms);
      ]

  (* Record expressions *)
  | TypedExpressionRecord fields ->
      find_first
        (fun (field : typed_record_field) -> find_node_at offset field.typed_field_value)
        fields
  | TypedExpressionRecordAccess (record, _) ->
      find_node_at offset record
  | TypedExpressionRecordUpdate (base, updates) ->
      try_searches [
        (fun () -> find_node_at offset base);
        (fun () -> find_first
            (fun (field : typed_record_field) -> find_node_at offset field.typed_field_value)
            updates);
      ]

  (* Reference expressions *)
  | TypedExpressionRef inner
  | TypedExpressionDeref inner
  | TypedExpressionAssert inner ->
      find_node_at offset inner
  | TypedExpressionAssign (ref_expr, value_expr) ->
      try_searches [
        (fun () -> find_node_at offset ref_expr);
        (fun () -> find_node_at offset value_expr);
      ]

  (* Loop expressions *)
  | TypedExpressionWhile (cond, body) ->
      try_searches [
        (fun () -> find_node_at offset cond);
        (fun () -> find_node_at offset body);
      ]
  | TypedExpressionFor (_, start_e, end_e, _, body) ->
      try_searches [
        (fun () -> find_node_at offset start_e);
        (fun () -> find_node_at offset end_e);
        (fun () -> find_node_at offset body);
      ]

  (* Terminal expressions - no children to search *)
  | TypedExpressionVariable _
  | TypedExpressionConstant _
  | TypedExpressionConstructor (_, None)
  | TypedExpressionModuleAccess _
  | TypedExpressionPolyVariant (_, None)
  | TypedExpressionPack _
  | TypedExpressionLetModule _
  | TypedExpressionError _ ->
      None

and find_in_binding_expressions offset bindings =
  find_first
    (fun (binding : typed_binding) -> find_node_at offset binding.binding_expression)
    bindings

and find_in_match_arms offset arms = find_first (find_in_match_arm offset) arms

and find_in_match_arm offset (arm : typed_match_arm) =
  try_searches [
    (fun () -> pattern_as_node (find_pattern_at offset arm.typed_arm_pattern));
    (fun () -> Option.bind arm.typed_arm_guard (find_node_at offset));
    (fun () -> find_node_at offset arm.typed_arm_expression);
  ]

let rec find_in_structure_item offset (item : typed_structure_item) =
  match item.structure_item_desc with
  | TypedStructureValue (_, bindings) ->
      find_first (find_in_value_binding offset) bindings

  | TypedStructureModule (_, mod_expr) ->
      find_in_module_expression offset mod_expr

  | TypedStructureRecModule rec_bindings ->
      find_first (fun (binding : typed_rec_module_binding) ->
        find_in_module_expression offset binding.rec_module_expr
      ) rec_bindings

  | TypedStructureType _
  | TypedStructureModuleType _
  | TypedStructureOpen _
  | TypedStructureInclude _
  | TypedStructureExternal _
  | TypedStructureTypeExtension _
  | TypedStructureError _ ->
      None

and find_in_value_binding offset (binding : typed_binding) =
  try_searches [
    (fun () -> pattern_as_node (find_pattern_at offset binding.binding_pattern));
    (fun () -> find_node_at offset binding.binding_expression);
  ]

and find_in_module_expression offset (mod_expr : typed_module_expression) =
  match mod_expr.module_desc with
  | TypedModuleStructure items ->
      find_first (find_in_structure_item offset) items

  | TypedModulePath _
  | TypedModuleFunctor _
  | TypedModuleApply _
  | TypedModuleConstraint _
  | TypedModuleUnpack _ ->
      None

let find_node_in_structure offset structure =
  find_first (find_in_structure_item offset) structure
