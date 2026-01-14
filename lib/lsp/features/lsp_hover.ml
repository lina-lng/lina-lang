(** Hover information for the LSP server. *)

let ( let* ) = Option.bind

type hover_result = {
  contents : string;
  range : Lsp_types.range option;
}

let build_hover_result node =
  let type_expr, location = match node with
    | Lsp_traversal.ExpressionNode expr ->
        (expr.expression_type, expr.expression_location)
    | Lsp_traversal.PatternNode pat ->
        (pat.pattern_type, pat.pattern_location)
  in

  let type_string = Typing.Types.type_expression_to_string type_expr in
  let markdown_contents = Printf.sprintf "```lina\n%s\n```" type_string in
  let hover_range = Lsp_types.range_of_location location in

  { contents = markdown_contents; range = Some hover_range }

let find_hover_in_typed_ast typed_ast offset =
  Lsp_traversal.find_node_in_structure offset typed_ast
  |> Option.map build_hover_result

let find_hover_in_cache cache offset =
  let* typed_ast = cache.Document_store.typed_ast in
  find_hover_in_typed_ast typed_ast offset

let find_hover_at_offset store uri offset =
  (* Force type checking to populate the cache before we query it *)
  let _ = Diagnostics.type_check_document store uri in
  let* cache = Document_store.get_typing_cache store uri in
  find_hover_in_cache cache offset

let get_hover store uri (pos : Lsp_types.position) =
  let* doc = Document_store.get_document store uri in
  let* offset = Document_store.lsp_position_to_offset doc pos in
  find_hover_at_offset store uri offset
