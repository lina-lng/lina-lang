(** Test helpers for LSP integration tests. *)

open Lina_lsp

(** Create a document store with a single document for testing. *)
let with_document content f =
  let store = Document_store.create () in
  let uri = "test.lina" in
  Document_store.open_document store ~uri ~content ~version:1;
  f store uri

(** Format a hover result for testing. *)
let show_hover_result (result : Lsp_hover.hover_result option) =
  match result with
  | None -> "None"
  | Some { contents; range } ->
      let range_str =
        match range with
        | None -> "no range"
        | Some r -> Lsp_types.show_range r
      in
      Printf.sprintf "Hover: %s\nRange: %s" contents range_str

(** Format a single completion item. *)
let show_completion_item (item : Lsp_completion.completion_item) =
  let kind_str =
    match item.kind with
    | Lsp_completion.Function -> "Function"
    | Lsp_completion.Variable -> "Variable"
    | Lsp_completion.Module -> "Module"
    | Lsp_completion.Type -> "Type"
    | Lsp_completion.Constructor -> "Constructor"
    | Lsp_completion.Field -> "Field"
    | Lsp_completion.Keyword -> "Keyword"
  in
  let detail_str =
    match item.detail with
    | None -> ""
    | Some d -> Printf.sprintf " : %s" d
  in
  Printf.sprintf "%s (%s)%s" item.label kind_str detail_str

(** Format completion items for testing. *)
let show_completions items =
  if items = [] then "No completions"
  else
    items
    |> List.map show_completion_item
    |> String.concat "\n"

(** Format a single document symbol. *)
let rec show_symbol indent (sym : Lsp_symbols.document_symbol) =
  let kind_str =
    match sym.kind with
    | Lsp_symbols.Function -> "Function"
    | Lsp_symbols.Variable -> "Variable"
    | Lsp_symbols.Module -> "Module"
    | Lsp_symbols.Type -> "Type"
    | Lsp_symbols.Constructor -> "Constructor"
    | Lsp_symbols.Field -> "Field"
  in
  let detail_str =
    match sym.detail with
    | None -> ""
    | Some d -> Printf.sprintf " : %s" d
  in
  let main = Printf.sprintf "%s%s (%s)%s" indent sym.name kind_str detail_str in
  let children_str =
    if sym.children = [] then ""
    else
      "\n" ^ String.concat "\n"
        (List.map (show_symbol (indent ^ "  ")) sym.children)
  in
  main ^ children_str

(** Format document symbols for testing. *)
let show_symbols symbols =
  if symbols = [] then "No symbols"
  else
    symbols
    |> List.map (show_symbol "")
    |> String.concat "\n"

(** Format a single diagnostic. *)
let show_diagnostic (diag : Lsp_types.diagnostic) =
  let severity_str =
    match diag.severity with
    | Lsp_types.Error -> "Error"
    | Lsp_types.Warning -> "Warning"
    | Lsp_types.Information -> "Info"
    | Lsp_types.Hint -> "Hint"
  in
  let code_str =
    match diag.code with
    | None -> ""
    | Some c -> Printf.sprintf "[%s] " c
  in
  let line = diag.range.start_pos.line in
  let char = diag.range.start_pos.character in
  Printf.sprintf "%d:%d %s: %s%s" line char severity_str code_str diag.message

(** Format diagnostics for testing. *)
let show_diagnostics diagnostics =
  if diagnostics = [] then "No diagnostics"
  else
    diagnostics
    |> List.map show_diagnostic
    |> String.concat "\n"

(** Create an LSP position. *)
let pos line character : Lsp_types.position = { line; character }

(** Reset type inference state before tests. *)
let reset () = Typing.Types.reset_type_variable_id ()

(** Module alias for linol LSP types. *)
module Lsp = Linol.Lsp

(** Create formatting options for testing. *)
let make_formatting_options ?(tab_size = 2) ?(insert_spaces = true) () =
  Lsp.Types.FormattingOptions.create ~tabSize:tab_size ~insertSpaces:insert_spaces ()

(** Format a single TextEdit for testing. *)
let show_text_edit (edit : Lsp.Types.TextEdit.t) =
  let range = edit.range in
  let start_line = range.start.line in
  let start_char = range.start.character in
  let end_line = range.end_.line in
  let end_char = range.end_.character in
  Printf.sprintf "Edit (%d:%d)-(%d:%d): %S"
    start_line start_char end_line end_char edit.newText

(** Format a list of TextEdits for testing. *)
let show_text_edits edits =
  match edits with
  | None -> "None"
  | Some [] -> "No edits"
  | Some edits ->
      edits
      |> List.map show_text_edit
      |> String.concat "\n"

(** Create an LSP range for testing. *)
let make_range start_line start_char end_line end_char : Lsp.Types.Range.t =
  Lsp.Types.Range.create
    ~start:(Lsp.Types.Position.create ~line:start_line ~character:start_char)
    ~end_:(Lsp.Types.Position.create ~line:end_line ~character:end_char)
