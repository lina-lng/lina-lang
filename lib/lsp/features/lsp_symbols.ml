(** Document symbols for the LSP server. *)

(** Symbol kinds. *)
type symbol_kind =
  | Function
  | Variable
  | Module
  | Type
  | Constructor
  | Field

(** A document symbol. *)
type document_symbol = {
  name : string;
  kind : symbol_kind;
  range : Lsp_types.range;
  detail : string option;
  children : document_symbol list;
}

(** Check if type is a function type. *)
let is_function_type (ty : Typing.Types.type_expression) =
  match Typing.Types.representative ty with
  | Typing.Types.TypeArrow _ -> true
  | _ -> false

(** Extract name from a typed pattern. *)
let pattern_name (pat : Typing.Typed_tree.typed_pattern) =
  match pat.pattern_desc with
  | Typing.Typed_tree.TypedPatternVariable id -> Some (Common.Identifier.name id)
  | _ -> None

(** Get symbols from a typed structure. *)
let rec symbols_of_structure (structure : Typing.Typed_tree.typed_structure) =
  List.concat_map symbols_of_item structure

and symbols_of_item (item : Typing.Typed_tree.typed_structure_item) =
  match item.structure_item_desc with
  | Typing.Typed_tree.TypedStructureValue (_, bindings) ->
      List.filter_map
        (fun (b : Typing.Typed_tree.typed_binding) ->
          match pattern_name b.binding_pattern with
          | None -> None
          | Some name ->
              let kind =
                if is_function_type b.binding_expression.expression_type then
                  Function
                else Variable
              in
              let detail =
                Some
                  (Typing.Types.type_expression_to_string
                     b.binding_expression.expression_type)
              in
              Some
                {
                  name;
                  kind;
                  range = Lsp_types.range_of_location b.binding_location;
                  detail;
                  children = [];
                })
        bindings
  | Typing.Typed_tree.TypedStructureType decls ->
      List.map
        (fun (decl : Typing.Types.type_declaration) ->
          {
            name = decl.declaration_name;
            kind = Type;
            range = Lsp_types.range_of_location item.structure_item_location;
            detail = None;
            children = [];
          })
        decls
  | Typing.Typed_tree.TypedStructureModule (id, mod_expr) ->
      let name = Common.Identifier.name id in
      let children =
        match mod_expr.module_desc with
        | Typing.Typed_tree.TypedModuleStructure items ->
            symbols_of_structure items
        | _ -> []
      in
      [
        {
          name;
          kind = Module;
          range = Lsp_types.range_of_location item.structure_item_location;
          detail = None;
          children;
        };
      ]
  | Typing.Typed_tree.TypedStructureRecModule rec_bindings ->
      List.map (fun (binding : Typing.Typed_tree.typed_rec_module_binding) ->
        let name = Common.Identifier.name binding.rec_module_id in
        let children =
          match binding.rec_module_expr.module_desc with
          | Typing.Typed_tree.TypedModuleStructure items ->
              symbols_of_structure items
          | _ -> []
        in
        {
          name;
          kind = Module;
          range = Lsp_types.range_of_location binding.rec_module_location;
          detail = Some "recursive";
          children;
        }
      ) rec_bindings
  | _ -> []

(** Get document symbols. *)
let get_document_symbols store uri =
  (* Ensure document is type-checked *)
  let _ = Diagnostics.type_check_document store uri in
  match Document_store.get_typing_cache store uri with
  | None -> []
  | Some cache -> (
      match cache.typed_ast with
      | None -> []
      | Some typed_ast -> symbols_of_structure typed_ast)
