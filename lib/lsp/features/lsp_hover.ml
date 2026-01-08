(** Hover information for the LSP server. *)

(** Hover result with markdown content and optional range. *)
type hover_result = {
  contents : string;
  range : Lsp_types.range option;
}

(** Result of searching for a node at a position - can be expression or pattern. *)
type node_at_position =
  | ExpressionNode of Typing.Typed_tree.typed_expression
  | PatternNode of Typing.Typed_tree.typed_pattern

(** Find the typed pattern at a given offset. *)
let rec find_pattern_at offset (pat : Typing.Typed_tree.typed_pattern) =
  let loc = pat.pattern_location in
  if loc.Common.Location.start_pos.offset <= offset
     && offset < loc.Common.Location.end_pos.offset
  then
    (* Check children first for more specific match *)
    let child_match =
      match pat.pattern_desc with
      | Typing.Typed_tree.TypedPatternTuple pats ->
          List.find_map (find_pattern_at offset) pats
      | Typing.Typed_tree.TypedPatternConstructor (_, Some arg) ->
          find_pattern_at offset arg
      | Typing.Typed_tree.TypedPatternRecord (fields, _) ->
          List.find_map
            (fun (f : Typing.Typed_tree.typed_record_pattern_field) ->
              find_pattern_at offset f.typed_pattern_field_pattern)
            fields
      | _ -> None
    in
    match child_match with
    | Some _ as result -> result
    | None -> Some pat
  else None

(** Find the typed expression or pattern at a given offset in the typed AST. *)
let rec find_node_at offset (expr : Typing.Typed_tree.typed_expression) =
  let loc = expr.expression_location in
  if loc.Common.Location.start_pos.offset <= offset
     && offset < loc.Common.Location.end_pos.offset
  then
    (* Check children first for more specific match *)
    let child_match =
      match expr.expression_desc with
      | Typing.Typed_tree.TypedExpressionTuple exprs ->
          List.find_map (find_node_at offset) exprs
      | Typing.Typed_tree.TypedExpressionConstructor (_, Some arg) ->
          find_node_at offset arg
      | Typing.Typed_tree.TypedExpressionApply (f, args) -> (
          match find_node_at offset f with
          | Some _ as result -> result
          | None -> List.find_map (find_node_at offset) args)
      | Typing.Typed_tree.TypedExpressionFunction (params, body) -> (
          (* Check function parameters (patterns) first *)
          let param_match =
            List.find_map
              (fun pat ->
                match find_pattern_at offset pat with
                | Some p -> Some (PatternNode p)
                | None -> None)
              params
          in
          match param_match with
          | Some _ as result -> result
          | None -> find_node_at offset body)
      | Typing.Typed_tree.TypedExpressionLet (_, bindings, body) -> (
          (* Check binding patterns first, then expressions *)
          let pattern_match =
            List.find_map
              (fun (b : Typing.Typed_tree.typed_binding) ->
                match find_pattern_at offset b.binding_pattern with
                | Some p -> Some (PatternNode p)
                | None -> None)
              bindings
          in
          match pattern_match with
          | Some _ as result -> result
          | None -> (
              let binding_match =
                List.find_map
                  (fun (b : Typing.Typed_tree.typed_binding) ->
                    find_node_at offset b.binding_expression)
                  bindings
              in
              match binding_match with
              | Some _ as result -> result
              | None -> find_node_at offset body))
      | Typing.Typed_tree.TypedExpressionIf (cond, then_, else_) -> (
          match find_node_at offset cond with
          | Some _ as result -> result
          | None -> (
              match find_node_at offset then_ with
              | Some _ as result -> result
              | None -> Option.bind else_ (find_node_at offset)))
      | Typing.Typed_tree.TypedExpressionSequence (e1, e2) -> (
          match find_node_at offset e1 with
          | Some _ as result -> result
          | None -> find_node_at offset e2)
      | Typing.Typed_tree.TypedExpressionRecord fields ->
          List.find_map
            (fun (f : Typing.Typed_tree.typed_record_field) ->
              find_node_at offset f.typed_field_value)
            fields
      | Typing.Typed_tree.TypedExpressionRecordAccess (e, _) ->
          find_node_at offset e
      | Typing.Typed_tree.TypedExpressionRecordUpdate (e, fields) -> (
          match find_node_at offset e with
          | Some _ as result -> result
          | None ->
              List.find_map
                (fun (f : Typing.Typed_tree.typed_record_field) ->
                  find_node_at offset f.typed_field_value)
                fields)
      | Typing.Typed_tree.TypedExpressionMatch (scrutinee, arms) -> (
          match find_node_at offset scrutinee with
          | Some _ as result -> result
          | None ->
              List.find_map
                (fun (arm : Typing.Typed_tree.typed_match_arm) ->
                  (* Check pattern first, then guard, then expression *)
                  match find_pattern_at offset arm.typed_arm_pattern with
                  | Some p -> Some (PatternNode p)
                  | None -> (
                      match arm.typed_arm_guard with
                      | Some guard -> (
                          match find_node_at offset guard with
                          | Some _ as result -> result
                          | None -> find_node_at offset arm.typed_arm_expression)
                      | None -> find_node_at offset arm.typed_arm_expression))
                arms)
      | _ -> None
    in
    match child_match with
    | Some _ as result -> result
    | None -> Some (ExpressionNode expr)
  else None

(** Find expression or pattern in a structure. *)
let find_node_in_structure offset (structure : Typing.Typed_tree.typed_structure)
    =
  let rec find_in_item (item : Typing.Typed_tree.typed_structure_item) =
    match item.structure_item_desc with
    | Typing.Typed_tree.TypedStructureValue (_, bindings) ->
        List.find_map
          (fun (b : Typing.Typed_tree.typed_binding) ->
            (* Check pattern first, then expression *)
            match find_pattern_at offset b.binding_pattern with
            | Some p -> Some (PatternNode p)
            | None -> find_node_at offset b.binding_expression)
          bindings
    | Typing.Typed_tree.TypedStructureModule (_, mod_expr) ->
        find_in_module_expr mod_expr
    | _ -> None
  and find_in_module_expr (me : Typing.Typed_tree.typed_module_expression) =
    match me.module_desc with
    | Typing.Typed_tree.TypedModuleStructure items ->
        List.find_map find_in_item items
    | _ -> None
  in
  List.find_map find_in_item structure

(** Get hover information at position. *)
let get_hover store uri (pos : Lsp_types.position) =
  match Document_store.get_document store uri with
  | None -> None
  | Some doc -> (
      match Document_store.lsp_position_to_offset doc pos with
      | None -> None
      | Some offset -> (
          (* Get typed AST *)
          let _, _, _ = Diagnostics.type_check_document store uri in
          match Document_store.get_typing_cache store uri with
          | None -> None
          | Some cache -> (
              match cache.typed_ast with
              | None -> None
              | Some typed_ast -> (
                  match find_node_in_structure offset typed_ast with
                  | None -> None
                  | Some node ->
                      let type_expr, location =
                        match node with
                        | ExpressionNode expr ->
                            (expr.expression_type, expr.expression_location)
                        | PatternNode pat ->
                            (pat.pattern_type, pat.pattern_location)
                      in
                      let type_str =
                        Typing.Types.type_expression_to_string type_expr
                      in
                      let contents =
                        Printf.sprintf "```lina\n%s\n```" type_str
                      in
                      let range = Some (Lsp_types.range_of_location location) in
                      Some { contents; range }))))
