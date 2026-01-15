(** Go-to-definition support for the LSP server. *)

let ( let* ) = Option.bind

type definition_result = {
  uri : string;
  range : Lsp_types.range;
}

type identifier_info =
  | ValueWithDefinition of Common.Location.t
  | ValueIdentifier of string
  | ModuleAccess of Typing.Module_types.path * string
  | PatternBinding of Common.Identifier.t
  | NoIdentifier

let identifier_from_expression (expr : Typing.Typed_tree.typed_expression) =
  let open Typing.Typed_tree in
  match expr.expression_desc with
  | TypedExpressionVariable (_id, Some def_loc) ->
      ValueWithDefinition def_loc
  | TypedExpressionVariable (id, None) ->
      ValueIdentifier (Common.Identifier.name id)
  | TypedExpressionModuleAccess (path, name) ->
      ModuleAccess (path, name)
  | _ ->
      NoIdentifier

let identifier_from_pattern (pat : Typing.Typed_tree.typed_pattern) =
  let open Typing.Typed_tree in
  match pat.pattern_desc with
  | TypedPatternVariable id -> PatternBinding id
  | _ -> NoIdentifier

let identifier_from_node = function
  | Lsp_traversal.ExpressionNode expr -> identifier_from_expression expr
  | Lsp_traversal.PatternNode pat -> identifier_from_pattern pat

let build_definition_result uri (def_loc : Common.Location.t) =
  let def_uri =
    if def_loc.start_pos.filename = "" then uri
    else def_loc.start_pos.filename
  in
  { uri = def_uri; range = Lsp_types.range_of_location def_loc }

let lookup_value_definition env name uri =
  let* binding = Typing.Environment.find_value name env in
  let loc = binding.Typing.Environment.binding_location in

  if Common.Location.is_none loc then None
  else Some (build_definition_result uri loc)

let lookup_module_member_definition env path name uri =
  let open Typing.Module_types in
  let* binding = Typing.Environment.find_module_by_path path env in
  let* value_desc =
    match binding.binding_type with
    | ModTypeSig sig_ -> find_value_in_sig name sig_
    | _ -> None
  in

  let loc = value_desc.value_location in

  if Common.Location.is_none loc then None
  else Some (build_definition_result uri loc)

let resolve_identifier env uri = function
  | ValueWithDefinition def_loc ->
      Some (build_definition_result uri def_loc)

  | ValueIdentifier name ->
      lookup_value_definition env name uri

  | ModuleAccess (path, name) ->
      lookup_module_member_definition env path name uri

  | PatternBinding _ | NoIdentifier ->
      None

let find_definition_at_node env uri node =
  resolve_identifier env uri (identifier_from_node node)

let find_definition_in_typed_ast typed_ast env uri offset =
  let* node = Lsp_traversal.find_node_in_structure offset typed_ast in
  find_definition_at_node env uri node

let find_definition_at_offset store uri offset =
  let typed_ast_opt, env, _ = Diagnostics.type_check_document store uri in
  let* typed_ast = typed_ast_opt in
  find_definition_in_typed_ast typed_ast env uri offset

let find_definition store uri pos =
  Lsp_pipeline.with_document_at_position store uri pos (fun ctx ->
    find_definition_at_offset ctx.store ctx.uri ctx.offset)
