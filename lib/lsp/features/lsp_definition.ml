(** Go-to-definition support for the LSP server.

    This module provides definition location lookup for identifiers.
    It reuses the typed AST traversal from hover to find the node at
    a position, then looks up the definition location from the environment. *)

(** Definition location result. *)
type definition_result = {
  uri : string;
  range : Lsp_types.range;
}

(** Result type for definition lookup - what kind of identifier we found. *)
type identifier_at_position =
  | ValueWithDefLocation of Common.Location.t  (** A value with known definition location *)
  | ValueIdentifier of string  (** A value variable reference (fallback to env lookup) *)
  | ModuleAccess of Typing.Module_types.path * string  (** M.x or M.N.x *)
  | PatternBinding of Common.Identifier.t  (** Pattern defines a new binding *)
  | NoIdentifier  (** Not an identifier we can navigate to *)

(** Extract identifier information from a typed expression. *)
let identifier_from_expression (expr : Typing.Typed_tree.typed_expression) =
  match expr.expression_desc with
  | Typing.Typed_tree.TypedExpressionVariable (_id, Some def_loc) ->
      (* Use definition location directly from typed AST *)
      ValueWithDefLocation def_loc
  | Typing.Typed_tree.TypedExpressionVariable (id, None) ->
      (* Fall back to environment lookup for builtins *)
      ValueIdentifier (Common.Identifier.name id)
  | Typing.Typed_tree.TypedExpressionModuleAccess (path, name) ->
      ModuleAccess (path, name)
  | _ -> NoIdentifier

(** Extract identifier information from a typed pattern.
    Patterns define bindings, so we return the identifier for the definition itself. *)
let identifier_from_pattern (pat : Typing.Typed_tree.typed_pattern) =
  match pat.pattern_desc with
  | Typing.Typed_tree.TypedPatternVariable id ->
      PatternBinding id
  | _ -> NoIdentifier

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
                | Some p -> Some (`Pattern p)
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
                | Some p -> Some (`Pattern p)
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
                  | Some p -> Some (`Pattern p)
                  | None -> (
                      match arm.typed_arm_guard with
                      | Some guard -> (
                          match find_node_at offset guard with
                          | Some _ as result -> result
                          | None -> find_node_at offset arm.typed_arm_expression)
                      | None -> find_node_at offset arm.typed_arm_expression))
                arms)
      | Typing.Typed_tree.TypedExpressionRef inner ->
          find_node_at offset inner
      | Typing.Typed_tree.TypedExpressionDeref inner ->
          find_node_at offset inner
      | Typing.Typed_tree.TypedExpressionAssign (ref_expr, value_expr) -> (
          match find_node_at offset ref_expr with
          | Some _ as result -> result
          | None -> find_node_at offset value_expr)
      | _ -> None
    in
    match child_match with
    | Some _ as result -> result
    | None -> Some (`Expression expr)
  else None

(** Find expression or pattern in a structure. *)
let find_node_in_structure offset (structure : Typing.Typed_tree.typed_structure) =
  let rec find_in_item (item : Typing.Typed_tree.typed_structure_item) =
    match item.structure_item_desc with
    | Typing.Typed_tree.TypedStructureValue (_, bindings) ->
        List.find_map
          (fun (b : Typing.Typed_tree.typed_binding) ->
            (* Check pattern first, then expression *)
            match find_pattern_at offset b.binding_pattern with
            | Some p -> Some (`Pattern p)
            | None -> find_node_at offset b.binding_expression)
          bindings
    | Typing.Typed_tree.TypedStructureModule (_, mod_expr) ->
        find_in_module_expr mod_expr
    | Typing.Typed_tree.TypedStructureExternal ext ->
        (* Check if offset is within external location *)
        let loc = ext.external_location in
        if loc.Common.Location.start_pos.offset <= offset
           && offset < loc.Common.Location.end_pos.offset
        then Some (`External ext)
        else None
    | _ -> None
  and find_in_module_expr (me : Typing.Typed_tree.typed_module_expression) =
    match me.module_desc with
    | Typing.Typed_tree.TypedModuleStructure items ->
        List.find_map find_in_item items
    | _ -> None
  in
  List.find_map find_in_item structure

(** Look up definition location for a value name in the environment. *)
let lookup_value_definition env name uri =
  match Typing.Environment.find_value name env with
  | None -> None
  | Some binding ->
      let loc = binding.Typing.Environment.binding_location in
      (* Check if location is valid (not Location.none) *)
      if Common.Location.is_none loc
      then None
      else
        let def_uri =
          if loc.Common.Location.start_pos.filename = "" then uri
          else loc.Common.Location.start_pos.filename
        in
        Some {
          uri = def_uri;
          range = Lsp_types.range_of_location loc;
        }

(** Look up definition location for a module member.
    For M.x, we need to find x's definition in M's signature. *)
let lookup_module_member_definition env path name uri =
  match Typing.Environment.find_module_by_path path env with
  | None -> None
  | Some binding ->
    match binding.Typing.Module_types.binding_type with
    | Typing.Module_types.ModTypeSig sig_ ->
      begin match Typing.Module_types.find_value_in_sig name sig_ with
      | None -> None
      | Some value_desc ->
        let loc = value_desc.Typing.Module_types.value_location in
        if Common.Location.is_none loc then None
        else
          let def_uri =
            if loc.Common.Location.start_pos.filename = "" then uri
            else loc.Common.Location.start_pos.filename
          in
          Some {
            uri = def_uri;
            range = Lsp_types.range_of_location loc;
          }
      end
    | _ -> None

(** Find definition at position.

    Returns the definition location if the cursor is on an identifier
    that has a known definition location. *)
let find_definition store uri (pos : Lsp_types.position) =
  match Document_store.get_document store uri with
  | None -> None
  | Some doc -> (
      match Document_store.lsp_position_to_offset doc pos with
      | None -> None
      | Some offset -> (
          (* Type check document to get typed AST and environment *)
          let typed_ast_opt, env, _ = Diagnostics.type_check_document store uri in
          match typed_ast_opt with
          | None -> None
          | Some typed_ast -> (
              match find_node_in_structure offset typed_ast with
              | None -> None
              | Some node ->
                  let identifier_info =
                    match node with
                    | `Expression expr -> identifier_from_expression expr
                    | `Pattern pat -> identifier_from_pattern pat
                    | `External _ -> NoIdentifier
                  in
                  match identifier_info with
                  | ValueWithDefLocation def_loc ->
                      (* Use definition location from typed AST directly *)
                      let def_uri =
                        if def_loc.Common.Location.start_pos.filename = "" then uri
                        else def_loc.Common.Location.start_pos.filename
                      in
                      Some {
                        uri = def_uri;
                        range = Lsp_types.range_of_location def_loc;
                      }
                  | ValueIdentifier name ->
                      lookup_value_definition env name uri
                  | ModuleAccess (path, name) ->
                      lookup_module_member_definition env path name uri
                  | PatternBinding _id ->
                      (* Pattern defines this binding - cursor is already at definition *)
                      None
                  | NoIdentifier -> None)))
