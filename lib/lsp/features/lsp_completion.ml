(** Completion support for the LSP server. *)

(** Completion item kinds. *)
type completion_kind =
  | Function
  | Variable
  | Module
  | Type
  | Constructor
  | Field
  | Keyword

(** A completion item. *)
type completion_item = {
  label : string;
  kind : completion_kind;
  detail : string option;
  documentation : string option;
}

(** Lina keywords for completion. *)
let keywords =
  [
    "let";
    "in";
    "if";
    "then";
    "else";
    "match";
    "with";
    "fun";
    "function";
    "type";
    "of";
    "rec";
    "and";
    "module";
    "struct";
    "end";
    "sig";
    "functor";
    "open";
    "include";
    "val";
    "external";
    "true";
    "false";
  ]

(** Check if a string starts with a prefix (case-sensitive). *)
let has_prefix prefix str =
  let prefix_len = String.length prefix in
  let str_len = String.length str in
  prefix_len <= str_len && String.sub str 0 prefix_len = prefix

(** Determine completion kind based on type: Function if arrow type, Variable otherwise. *)
let kind_of_type_scheme (scheme : Typing.Types.type_scheme) =
  (* Follow type variable links to get the actual type *)
  match Typing.Types.representative scheme.body with
  | Typing.Types.TypeArrow _ -> Function
  | _ -> Variable

(** Get completions from environment. *)
let completions_from_env prefix (env : Typing.Environment.t) =
  let value_completions =
    Typing.Environment.fold_values
      (fun name _id scheme acc ->
        if has_prefix prefix name then
          let type_str = Typing.Types.type_expression_to_string scheme.Typing.Types.body in
          {
            label = name;
            kind = kind_of_type_scheme scheme;
            detail = Some type_str;
            documentation = None;
          }
          :: acc
        else acc)
      env []
  in
  let type_completions =
    Typing.Environment.fold_types
      (fun name _decl acc ->
        if has_prefix prefix name then
          { label = name; kind = Type; detail = Some "type"; documentation = None }
          :: acc
        else acc)
      env []
  in
  let constructor_completions =
    Typing.Environment.fold_constructors
      (fun name info acc ->
        if has_prefix prefix name then
          let type_str =
            Typing.Types.type_expression_to_string info.Typing.Types.constructor_result_type
          in
          {
            label = name;
            kind = Constructor;
            detail = Some type_str;
            documentation = None;
          }
          :: acc
        else acc)
      env []
  in
  let module_completions =
    Typing.Environment.fold_modules
      (fun name _binding acc ->
        if has_prefix prefix name then
          { label = name; kind = Module; detail = Some "module"; documentation = None }
          :: acc
        else acc)
      env []
  in
  value_completions @ type_completions @ constructor_completions @ module_completions

(** Get keyword completions. *)
let keyword_completions prefix =
  keywords
  |> List.filter (fun kw -> String.length prefix = 0 || String.sub kw 0 (min (String.length prefix) (String.length kw)) = prefix)
  |> List.map (fun kw ->
         { label = kw; kind = Keyword; detail = None; documentation = None })

(** Get completions at position. *)
let get_completions store uri (pos : Lsp_types.position) =
  match Document_store.get_document store uri with
  | None -> []
  | Some doc -> (
      match Document_store.lsp_position_to_offset doc pos with
      | None -> []
      | Some offset ->
          (* Extract prefix being typed *)
          let prefix =
            let rec find_start i =
              if i < 0 then 0
              else
                let c = doc.content.[i] in
                if
                  (c >= 'a' && c <= 'z')
                  || (c >= 'A' && c <= 'Z')
                  || (c >= '0' && c <= '9')
                  || c = '_'
                then find_start (i - 1)
                else i + 1
            in
            let start = find_start (offset - 1) in
            if start < offset then String.sub doc.content start (offset - start)
            else ""
          in

          (* Trigger type checking to populate cache *)
          let _, _, _ = Diagnostics.type_check_document store uri in

          (* Get environment from typing cache *)
          let env_completions =
            match Document_store.get_typing_cache store uri with
            | None -> []
            | Some cache -> completions_from_env prefix cache.environment
          in

          (* Combine with keywords *)
          env_completions @ keyword_completions prefix)
