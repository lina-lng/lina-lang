(** Module-level formatting with semantic awareness.

    This module provides semantic formatters for structure items, module
    expressions, signatures, and module-level declarations. *)

open Cst
open Doc
open Format_accessors
open Format_common

(** Forward declarations for cross-module references *)
let format_expression_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_pattern_ref : (Red_tree.syntax_node -> doc) ref =
  ref (fun _ -> empty)

let format_type_ref : (Red_tree.syntax_node -> doc) ref = ref (fun _ -> empty)

let format_expression node = !format_expression_ref node
let format_pattern node = !format_pattern_ref node
let format_type node = !format_type_ref node

(** {1 Internal Helpers} *)

(** Get the first token of a node for blank line detection. *)
let first_token node = Red_tree.first_token node

(** Check if there's a blank line before a node. *)
let has_blank_before node =
  match first_token node with
  | Some tok -> has_blank_line_before tok
  | None -> false

(** {1 Binding Formatters} *)

let format_binding node =
  match Binding.name node, Binding.equals node with
  | Some name, Some eq ->
      let name_doc = format_pattern name in
      let params = Binding.parameters node in
      (* Patterns already have trailing whitespace in trivia, no need to add spaces *)
      let params_doc = concat_list (List.map format_pattern params) in
      let ty_doc =
        match Binding.type_annotation node with
        | Some ty -> colon ^^ space ^^ format_type ty
        | None -> empty
      in
      let eq_doc = format_token eq in
      (* Format ALL body expressions, not just the first one.
         The CST may not wrap the entire body expression in a single node. *)
      let body_exprs = Binding.body_expressions node in
      let body_doc = concat_list (List.map format_expression body_exprs) in
      group
        (name_doc ^^ params_doc ^^ ty_doc ^^ eq_doc
        ^^ nest (indent_width ()) body_doc)
  | _ -> format_children node

(** {1 Structure Item Formatters} *)

let rec format_value_definition node =
  match Value_definition.let_keyword node with
  | Some let_kw ->
      let let_doc = format_token let_kw in
      let rec_doc =
        match Value_definition.rec_keyword node with
        | Some rec_kw -> format_token rec_kw
        | None -> empty
      in
      let bindings = Value_definition.bindings node in
      let bindings_doc =
        match bindings with
        | [] -> empty
        | _ ->
            let formatted = List.map format_binding bindings in
            (* Join with ' and ' for mutual recursion *)
            format_separated (space ^^ text "and" ^^ space) formatted
      in
      (* Trivia already includes spacing, don't add extra *)
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
      let params_doc =
        match Type_declaration.params node with
        | Some params -> format_children params
        | None -> empty
      in
      let name_doc = format_token name_tok in
      let body_doc =
        match Type_declaration.equals node, Type_declaration.body node with
        | Some eq, Some body ->
            let eq_doc = format_token eq in
            let body_formatted = format_type_body body in
            eq_doc ^^ nest (indent_width ()) body_formatted
        | _ -> empty
      in
      (* Trivia already includes spacing, don't add extra *)
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
  match
    Module_definition.module_keyword node,
    Module_definition.name node,
    Module_definition.equals node,
    Module_definition.body node
  with
  | Some mod_kw, Some name, Some eq, Some body ->
      let mod_doc = format_token mod_kw in
      let name_doc = format_token name in
      let params = Module_definition.params node in
      let params_doc =
        match params with
        | [] -> empty
        | _ ->
            let formatted = List.map format_functor_parameter params in
            space ^^ concat_list formatted
      in
      let type_doc =
        match Module_definition.type_annotation node with
        | Some ty -> space ^^ colon ^^ space ^^ format_module_type ty
        | None -> empty
      in
      let eq_doc = format_token eq in
      let body_doc = format_module_expr body in
      group
        (mod_doc ^/^ name_doc ^^ params_doc ^^ type_doc ^/^ eq_doc
        ^^ nest (indent_width ()) (softline ^^ body_doc))
  | _ -> format_children node

and format_open_declaration node =
  match Open_declaration.open_keyword node, Open_declaration.path node with
  | Some open_kw, Some path ->
      let open_doc = format_token open_kw in
      let path_doc = format_children path in
      open_doc ^/^ path_doc
  | _ -> format_children node

and format_include_declaration node =
  match
    Include_declaration.include_keyword node, Include_declaration.module_expr node
  with
  | Some incl_kw, Some mod_expr ->
      let incl_doc = format_token incl_kw in
      let mod_doc = format_module_expr mod_expr in
      incl_doc ^/^ mod_doc
  | _ -> format_children node

and format_external_declaration node =
  match
    External_declaration.external_keyword node, External_declaration.name node
  with
  | Some ext_kw, Some name_tok ->
      let ext_doc = format_token ext_kw in
      let name_doc = format_token name_tok in
      let colon_doc =
        match External_declaration.colon node with
        | Some c -> format_token c
        | None -> colon
      in
      let ty_doc =
        match External_declaration.type_expr node with
        | Some ty -> format_type ty
        | None -> empty
      in
      let equals_doc =
        match External_declaration.equals node with
        | Some eq -> format_token eq
        | None -> empty
      in
      let lua_name_doc =
        match External_declaration.lua_name node with
        | Some s -> format_token s
        | None -> empty
      in
      group
        (ext_doc ^/^ name_doc ^/^ colon_doc ^/^ ty_doc ^/^ equals_doc ^/^
         lua_name_doc)
  | _ -> format_children node

(** {1 Module Expression Formatters} *)

and format_structure node =
  match Structure.struct_keyword node, Structure.end_keyword node with
  | Some struct_kw, Some end_kw ->
      let struct_doc = format_token struct_kw in
      let end_doc = format_token end_kw in
      let items = Structure.items node in
      (match items with
       | [] -> struct_doc ^^ end_doc
       | _ ->
           let items_doc = format_structure_items items in
           group (struct_doc ^^ nest (indent_width ()) items_doc ^^ end_doc))
  | _ -> format_children node

and format_structure_items items =
  (* Don't add separators - the leading trivia of each item already contains
     the newlines from the source. Adding extra hardlines would double them. *)
  concat_list (List.map format_structure_item items)

and format_structure_item node =
  let kind = Red_tree.kind node in
  if Red_tree.is_error node then text (Red_tree.text node)
  else if Red_tree.has_errors node then format_children node
  else
    match kind with
    | Syntax_kind.NK_VALUE_DEFINITION -> format_value_definition node
    | Syntax_kind.NK_TYPE_DEFINITION -> format_type_definition node
    (* Module-related items use format_children to preserve source structure
       since their formatting has trivia-spacing conflicts *)
    | _ -> format_children node

and format_functor_expr node =
  match
    Functor_expr.functor_keyword node,
    Functor_expr.arrow node,
    Functor_expr.body node
  with
  | Some func_kw, Some arrow_tok, Some body ->
      let func_doc = format_token func_kw in
      let params = Functor_expr.params node in
      let params_doc =
        match params with
        | [] -> empty
        | _ ->
            let formatted = List.map format_functor_parameter params in
            space ^^ concat_list formatted
      in
      let arrow_doc = format_token arrow_tok in
      let body_doc = format_module_expr body in
      group
        (func_doc ^^ params_doc ^/^ arrow_doc
        ^^ nest (indent_width ()) (softline ^^ body_doc))
  | _ -> format_children node

and format_functor_parameter node =
  match
    Functor_parameter.lparen node,
    Functor_parameter.name node,
    Functor_parameter.colon node,
    Functor_parameter.module_type node,
    Functor_parameter.rparen node
  with
  | Some lp, Some name, Some col, Some mty, Some rp ->
      let lp_doc = format_token lp in
      let name_doc = format_token name in
      let col_doc = format_token col in
      let mty_doc = format_module_type mty in
      let rp_doc = format_token rp in
      group (lp_doc ^^ name_doc ^/^ col_doc ^/^ mty_doc ^^ rp_doc)
  | _ -> format_children node

and format_module_apply node =
  match Module_apply.functor_expr node, Module_apply.argument node with
  | Some func, Some arg ->
      let func_doc = format_module_expr func in
      let lp_doc =
        match Module_apply.lparen node with
        | Some lp -> format_token lp
        | None -> lparen
      in
      let arg_doc = format_module_expr arg in
      let rp_doc =
        match Module_apply.rparen node with
        | Some rp -> format_token rp
        | None -> rparen
      in
      group (func_doc ^^ lp_doc ^^ arg_doc ^^ rp_doc)
  | _ -> format_children node

and format_module_constraint node =
  match
    Module_constraint.module_expr node, Module_constraint.module_type node
  with
  | Some mod_expr, Some mod_ty ->
      let mod_doc = format_module_expr mod_expr in
      let colon_doc =
        match Module_constraint.colon node with
        | Some c -> format_token c
        | None -> colon
      in
      let ty_doc = format_module_type mod_ty in
      group (mod_doc ^/^ colon_doc ^/^ ty_doc)
  | _ -> format_children node

and format_module_expr node =
  let kind = Red_tree.kind node in
  if Red_tree.is_error node then text (Red_tree.text node)
  else if Red_tree.has_errors node then format_children node
  else
    match kind with
    | Syntax_kind.NK_STRUCTURE -> format_structure node
    | Syntax_kind.NK_FUNCTOR_EXPR -> format_functor_expr node
    | Syntax_kind.NK_MODULE_APPLY -> format_module_apply node
    | Syntax_kind.NK_MODULE_CONSTRAINT -> format_module_constraint node
    | Syntax_kind.NK_MODULE_PATH -> format_children node
    | _ -> format_children node

(** {1 Signature Formatters} *)

and format_signature node =
  match Signature.sig_keyword node, Signature.end_keyword node with
  | Some sig_kw, Some end_kw ->
      let sig_doc = format_token sig_kw in
      let end_doc = format_token end_kw in
      let items = Signature.items node in
      (match items with
       | [] -> sig_doc ^^ end_doc
       | _ ->
           let items_doc = format_signature_items items in
           group (sig_doc ^^ nest (indent_width ()) items_doc ^^ end_doc))
  | _ -> format_children node

and format_signature_items items =
  let rec format_with_blanks = function
    | [] -> empty
    | [ item ] -> format_signature_item item
    | item :: next :: rest ->
        let formatted = format_signature_item item in
        let sep =
          if has_blank_before next then hardline ^^ hardline else hardline
        in
        formatted ^^ sep ^^ format_with_blanks (next :: rest)
  in
  format_with_blanks items

and format_signature_item node =
  let kind = Red_tree.kind node in
  if Red_tree.is_error node then text (Red_tree.text node)
  else if Red_tree.has_errors node then format_children node
  else
    match kind with
    | Syntax_kind.NK_VALUE_SPECIFICATION -> format_value_specification node
    | Syntax_kind.NK_TYPE_SPECIFICATION -> format_children node
    | Syntax_kind.NK_MODULE_SPECIFICATION -> format_children node
    | Syntax_kind.NK_MODULE_TYPE_SPECIFICATION -> format_children node
    | Syntax_kind.NK_INCLUDE_SPECIFICATION -> format_children node
    | _ -> format_children node

and format_value_specification node =
  match
    Value_specification.val_keyword node,
    Value_specification.name node,
    Value_specification.type_expr node
  with
  | Some val_kw, Some name_tok, Some ty ->
      let val_doc = format_token val_kw in
      let name_doc = format_token name_tok in
      let colon_doc =
        match Value_specification.colon node with
        | Some c -> format_token c
        | None -> colon
      in
      let ty_doc = format_type ty in
      group (val_doc ^/^ name_doc ^/^ colon_doc ^/^ ty_doc)
  | _ -> format_children node

and format_module_type node =
  let kind = Red_tree.kind node in
  match kind with
  | Syntax_kind.NK_SIGNATURE -> format_signature node
  | Syntax_kind.NK_MODULE_TYPE_PATH -> format_children node
  | Syntax_kind.NK_FUNCTOR_TYPE -> format_children node
  | Syntax_kind.NK_MODULE_TYPE_WITH -> format_children node
  | _ -> format_children node
