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

(** Get completions from environment. *)
let completions_from_env _prefix (_env : Typing.Environment.t) =
  (* This is a simplified implementation - we'd need to expose
     Environment iteration functions for full support *)
  []

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

          (* Get environment from typing cache *)
          let env_completions =
            match Document_store.get_typing_cache store uri with
            | None -> []
            | Some cache -> completions_from_env prefix cache.environment
          in

          (* Combine with keywords *)
          env_completions @ keyword_completions prefix)
