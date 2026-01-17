(** Module-level formatting with semantic awareness.

    This module provides semantic formatters for structure items, module
    expressions, signatures, and module-level declarations. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** Use centralized forward refs from Format_common *)
let format_expression = format_expression
let format_pattern = format_pattern
let format_type = format_type

(** {1 Binding Formatters} *)

let format_binding node =
  with_required2
    (Binding.name node)
    (Binding.equals node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun name eq ->
      let name_doc = format_pattern name in
      let params_doc = format_node_list format_pattern (Binding.parameters node) in
      let ty_doc = format_with_prefix (colon ^^ space) format_type (Binding.type_annotation node) in
      let eq_doc = format_token eq in
      let body_doc = format_node_list format_expression (Binding.body_expressions node) in

      group
        (name_doc ^^ params_doc ^^ ty_doc ^^ eq_doc
        ^^ nest (indent_width ()) body_doc))

(** {1 Structure Item Formatters} *)

let rec format_value_definition node =
  match Value_definition.let_keyword node with
  | Some let_kw ->
      let let_doc = format_token let_kw in
      let rec_doc = format_optional_token (Value_definition.rec_keyword node) in
      let bindings = Value_definition.bindings node in
      let bindings_doc =
        match bindings with
        | [] -> empty
        | _ ->
            let formatted = List.map format_binding bindings in
            format_separated (space ^^ text "and" ^^ space) formatted
      in
      group (let_doc ^^ rec_doc ^^ bindings_doc)
  | None -> format_children node

and format_type_definition node =
  match Type_definition.type_keyword node with
  | Some type_kw ->
      let type_doc = format_token type_kw in
      let decls = Type_definition.declarations node in
      let decls_doc =
        match decls with
        | [] -> empty
        | _ ->
            let formatted = List.map format_type_declaration decls in
            format_separated (space ^^ text "and" ^^ space) formatted
      in
      (* Trivia already includes spacing, don't add extra *)
      group (type_doc ^^ decls_doc)
  | None -> format_children node

and format_type_declaration node =
  match Type_declaration.name node with
  | Some name_tok ->
      let params_doc = format_with_prefix empty format_children (Type_declaration.params node) in
      let name_doc = format_token name_tok in
      let body_doc =
        match Type_declaration.equals node, Type_declaration.body node with
        | Some eq, Some body ->
            let eq_doc = format_token eq in
            let body_formatted = format_type_body body in
            eq_doc ^^ nest (indent_width ()) body_formatted
        | _ -> empty
      in
      group (params_doc ^^ name_doc ^^ body_doc)
  | None -> format_children node

and format_type_body node =
  let kind = Red_tree.kind node in
  if Syntax_kind.is_type kind then format_type node
  else if Syntax_kind.equal kind Syntax_kind.NK_CONSTRUCTOR_DECLARATION then
    format_constructor_declarations_from node
  else format_children node

and format_constructor_declarations_from node =
  (* Find all constructor declarations *)
  match Red_tree.parent node with
  | Some parent ->
      let siblings = Red_tree.child_nodes parent in
      let ctors =
        List.filter
          (fun n ->
            Syntax_kind.equal (Red_tree.kind n)
              Syntax_kind.NK_CONSTRUCTOR_DECLARATION)
          siblings
      in
      let formatted = List.map format_constructor_declaration ctors in
      concat_list (List.map (fun d -> hardline ^^ d) formatted)
  | None -> format_constructor_declaration node

and format_constructor_declaration node =
  let bar_doc =
    match Constructor_declaration.bar node with
    | Some bar -> format_token bar
    | None -> pipe ^^ space
  in
  match Constructor_declaration.name node with
  | Some name_tok ->
      let name_doc = format_token name_tok in
      let arg_doc =
        match
          Constructor_declaration.of_keyword node,
          Constructor_declaration.argument_type node
        with
        | Some of_kw, Some arg_ty ->
            space ^^ format_token of_kw ^/^ format_type arg_ty
        | _ -> empty
      in
      group (bar_doc ^^ name_doc ^^ arg_doc)
  | None -> format_children node

and format_module_definition node =
  with_required4
    (Module_definition.module_keyword node)
    (Module_definition.name node)
    (Module_definition.equals node)
    (Module_definition.body node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun mod_kw name eq body ->
      let module_keyword_doc = format_token mod_kw in
      let name_doc = format_token name in
      let functor_params = Module_definition.params node in
      let functor_params_doc =
        match functor_params with
        | [] -> empty
        | _ -> space ^^ format_node_list format_functor_parameter functor_params
      in
      let type_annotation_doc =
        match Module_definition.type_annotation node with
        | Some ty -> space ^^ colon ^^ space ^^ format_module_type ty
        | None -> empty
      in
      let equals_doc = format_token eq in
      let body_doc = format_module_expr body in
      group
        (module_keyword_doc ^/^ name_doc ^^ functor_params_doc ^^ type_annotation_doc ^/^ equals_doc
        ^^ nest (indent_width ()) (softline ^^ body_doc)))

and format_open_declaration node =
  with_required2
    (Open_declaration.open_keyword node)
    (Open_declaration.path node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun open_kw path ->
      let open_keyword_doc = format_token open_kw in
      let path_doc = format_children path in
      open_keyword_doc ^/^ path_doc)

and format_include_declaration node =
  with_required2
    (Include_declaration.include_keyword node)
    (Include_declaration.module_expr node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun incl_kw mod_expr ->
      let include_keyword_doc = format_token incl_kw in
      let module_expr_doc = format_module_expr mod_expr in
      include_keyword_doc ^/^ module_expr_doc)

and format_external_declaration node =
  with_required2
    (External_declaration.external_keyword node)
    (External_declaration.name node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun ext_kw name_tok ->
      let ext_doc = format_token ext_kw in
      let name_doc = format_token name_tok in
      let colon_doc = format_token_or colon (External_declaration.colon node) in
      let ty_doc =
        match External_declaration.type_expr node with
        | Some ty -> format_type ty
        | None -> empty
      in
      let equals_doc = format_optional_token (External_declaration.equals node) in
      let lua_name_doc = format_optional_token (External_declaration.lua_name node) in
      group
        (ext_doc ^/^ name_doc ^/^ colon_doc ^/^ ty_doc ^/^ equals_doc ^/^
         lua_name_doc))

(** {1 Module Expression Formatters} *)

and format_structure node =
  with_required2
    (Structure.struct_keyword node)
    (Structure.end_keyword node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun struct_kw end_kw ->
      let struct_doc = format_token struct_kw in
      let end_doc = format_token end_kw in
      match Structure.items node with
      | [] -> struct_doc ^^ end_doc
      | items ->
          let items_doc = format_structure_items items in
          group (struct_doc ^^ nest (indent_width ()) items_doc ^^ end_doc))

and format_structure_items items =
  (* Don't add separators - the leading trivia of each item already contains
     the newlines from the source. Adding extra hardlines would double them. *)
  concat_list (List.map format_structure_item items)

and format_structure_item node =
  with_error_recovery node ~formatter:(fun kind _node ->
    match kind with
    | Syntax_kind.NK_VALUE_DEFINITION -> format_value_definition node
    | Syntax_kind.NK_TYPE_DEFINITION -> format_type_definition node
    | _ -> format_children node)

and format_functor_expr node =
  with_required3
    (Functor_expr.functor_keyword node)
    (Functor_expr.arrow node)
    (Functor_expr.body node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun func_kw arrow_tok body ->
      let func_doc = format_token func_kw in
      let params = Functor_expr.params node in
      let params_doc =
        match params with
        | [] -> empty
        | _ -> space ^^ format_node_list format_functor_parameter params
      in
      let arrow_doc = format_token arrow_tok in
      let body_doc = format_module_expr body in
      group
        (func_doc ^^ params_doc ^/^ arrow_doc
        ^^ nest (indent_width ()) (softline ^^ body_doc)))

and format_functor_parameter node =
  with_required5
    (Functor_parameter.lparen node)
    (Functor_parameter.name node)
    (Functor_parameter.colon node)
    (Functor_parameter.module_type node)
    (Functor_parameter.rparen node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun lp name col mty rp ->
      let lp_doc = format_token lp in
      let name_doc = format_token name in
      let col_doc = format_token col in
      let mty_doc = format_module_type mty in
      let rp_doc = format_token rp in
      group (lp_doc ^^ name_doc ^/^ col_doc ^/^ mty_doc ^^ rp_doc))

and format_module_apply node =
  with_required2
    (Module_apply.functor_expr node)
    (Module_apply.argument node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun func arg ->
      let func_doc = format_module_expr func in
      let lp_doc = format_token_or lparen (Module_apply.lparen node) in
      let arg_doc = format_module_expr arg in
      let rp_doc = format_token_or rparen (Module_apply.rparen node) in
      group (func_doc ^^ lp_doc ^^ arg_doc ^^ rp_doc))

and format_module_constraint node =
  with_required2
    (Module_constraint.module_expr node)
    (Module_constraint.module_type node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun mod_expr mod_ty ->
      let module_expr_doc = format_module_expr mod_expr in
      let colon_doc = format_token_or colon (Module_constraint.colon node) in
      let module_type_doc = format_module_type mod_ty in
      group (module_expr_doc ^/^ colon_doc ^/^ module_type_doc))

and format_module_expr node =
  with_error_recovery node ~formatter:(fun kind _node ->
    match kind with
    | Syntax_kind.NK_STRUCTURE -> format_structure node
    | Syntax_kind.NK_FUNCTOR_EXPR -> format_functor_expr node
    | Syntax_kind.NK_MODULE_APPLY -> format_module_apply node
    | Syntax_kind.NK_MODULE_CONSTRAINT -> format_module_constraint node
    | Syntax_kind.NK_MODULE_PATH -> format_children node
    | _ -> format_children node)

(** {1 Signature Formatters} *)

and format_signature node =
  with_required2
    (Signature.sig_keyword node)
    (Signature.end_keyword node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun sig_kw end_kw ->
      let sig_doc = format_token sig_kw in
      let end_doc = format_token end_kw in
      match Signature.items node with
      | [] -> sig_doc ^^ end_doc
      | items ->
          let items_doc = format_signature_items items in
          group (sig_doc ^^ nest (indent_width ()) items_doc ^^ end_doc))

and format_signature_items items =
  format_items_with_blank_sep ~formatter:format_signature_item items

and format_signature_item node =
  with_error_recovery node ~formatter:(fun kind _node ->
    match kind with
    | Syntax_kind.NK_VALUE_SPECIFICATION -> format_value_specification node
    | Syntax_kind.NK_TYPE_SPECIFICATION -> format_children node
    | Syntax_kind.NK_MODULE_SPECIFICATION -> format_children node
    | Syntax_kind.NK_MODULE_TYPE_SPECIFICATION -> format_children node
    | Syntax_kind.NK_INCLUDE_SPECIFICATION -> format_children node
    | _ -> format_children node)

and format_value_specification node =
  with_required3
    (Value_specification.val_keyword node)
    (Value_specification.name node)
    (Value_specification.type_expr node)
    ~fallback:(fun () -> format_children node)
    ~success:(fun val_kw name_tok ty ->
      let val_doc = format_token val_kw in
      let name_doc = format_token name_tok in
      let colon_doc = format_token_or colon (Value_specification.colon node) in
      let ty_doc = format_type ty in
      group (val_doc ^/^ name_doc ^/^ colon_doc ^/^ ty_doc))

and format_module_type node =
  let kind = Red_tree.kind node in
  match kind with
  | Syntax_kind.NK_SIGNATURE -> format_signature node
  | Syntax_kind.NK_MODULE_TYPE_PATH -> format_children node
  | Syntax_kind.NK_FUNCTOR_TYPE -> format_children node
  | Syntax_kind.NK_MODULE_TYPE_WITH -> format_children node
  | _ -> format_children node
