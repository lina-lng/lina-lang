(** Hover information for the LSP server. *)

(** Hover result with markdown content and optional range. *)
type hover_result = {
  contents : string;
  range : Lsp_types.range option;
}

(** Find the typed expression at a given offset in the typed AST. *)
let rec find_expression_at offset (expr : Typing.Typed_tree.typed_expression) =
  let loc = expr.expression_location in
  if loc.start_pos.offset <= offset && offset < loc.end_pos.offset then
    (* Check children first for more specific match *)
    let child_match =
      match expr.expression_desc with
      | Typing.Typed_tree.TypedExpressionTuple exprs ->
          List.find_map (find_expression_at offset) exprs
      | Typing.Typed_tree.TypedExpressionConstructor (_, Some arg) ->
          find_expression_at offset arg
      | Typing.Typed_tree.TypedExpressionApply (f, args) -> (
          match find_expression_at offset f with
          | Some _ as result -> result
          | None -> List.find_map (find_expression_at offset) args)
      | Typing.Typed_tree.TypedExpressionFunction (_, body) ->
          find_expression_at offset body
      | Typing.Typed_tree.TypedExpressionLet (_, bindings, body) -> (
          let binding_match =
            List.find_map
              (fun (b : Typing.Typed_tree.typed_binding) ->
                find_expression_at offset b.binding_expression)
              bindings
          in
          match binding_match with
          | Some _ as result -> result
          | None -> find_expression_at offset body)
      | Typing.Typed_tree.TypedExpressionIf (cond, then_, else_) -> (
          match find_expression_at offset cond with
          | Some _ as result -> result
          | None -> (
              match find_expression_at offset then_ with
              | Some _ as result -> result
              | None -> Option.bind else_ (find_expression_at offset)))
      | Typing.Typed_tree.TypedExpressionSequence (e1, e2) -> (
          match find_expression_at offset e1 with
          | Some _ as result -> result
          | None -> find_expression_at offset e2)
      | Typing.Typed_tree.TypedExpressionRecord fields ->
          List.find_map
            (fun (f : Typing.Typed_tree.typed_record_field) ->
              find_expression_at offset f.typed_field_value)
            fields
      | Typing.Typed_tree.TypedExpressionRecordAccess (e, _) ->
          find_expression_at offset e
      | Typing.Typed_tree.TypedExpressionRecordUpdate (e, fields) -> (
          match find_expression_at offset e with
          | Some _ as result -> result
          | None ->
              List.find_map
                (fun (f : Typing.Typed_tree.typed_record_field) ->
                  find_expression_at offset f.typed_field_value)
                fields)
      | Typing.Typed_tree.TypedExpressionMatch (scrutinee, arms) -> (
          match find_expression_at offset scrutinee with
          | Some _ as result -> result
          | None ->
              List.find_map
                (fun (arm : Typing.Typed_tree.typed_match_arm) ->
                  find_expression_at offset arm.typed_arm_expression)
                arms)
      | _ -> None
    in
    match child_match with
    | Some _ as result -> result
    | None -> Some expr
  else None

(** Find expression in a structure. *)
let find_expression_in_structure offset
    (structure : Typing.Typed_tree.typed_structure) =
  let rec find_in_item (item : Typing.Typed_tree.typed_structure_item) =
    match item.structure_item_desc with
    | Typing.Typed_tree.TypedStructureValue (_, bindings) ->
        List.find_map
          (fun (b : Typing.Typed_tree.typed_binding) ->
            find_expression_at offset b.binding_expression)
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
                  match find_expression_in_structure offset typed_ast with
                  | None -> None
                  | Some expr ->
                      let type_str =
                        Typing.Types.type_expression_to_string
                          expr.expression_type
                      in
                      let contents =
                        Printf.sprintf "```lina\n%s\n```" type_str
                      in
                      let range =
                        Some
                          (Lsp_types.range_of_location expr.expression_location)
                      in
                      Some { contents; range }))))
