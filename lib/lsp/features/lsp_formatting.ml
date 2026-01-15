(** LSP formatting support. *)

module Lsp = Linol.Lsp

(** Format an entire document. *)
let format_document store uri (_options : Lsp.Types.FormattingOptions.t) =
  match Document_store.get_document store uri with
  | None -> None
  | Some doc ->
      let content = doc.content in
      let formatted =
        try Lina_format.Formatter.format_string_cst content
        with _ -> content  (* On error, keep original *)
      in
      if String.equal content formatted then
        Some []
      else
        let range = Lsp_types.full_document_range content in
        let lsp_range = Lsp_conversions.lsp_range_of_internal range in
        let edit = Lsp.Types.TextEdit.create ~range:lsp_range ~newText:formatted in
        Some [edit]

(** Format a range within a document.
    Note: Currently formats the entire document as range-specific formatting
    requires careful handling of partial syntax. *)
let format_range store uri (range : Lsp.Types.Range.t) (_options : Lsp.Types.FormattingOptions.t) =
  match Document_store.get_document store uri with
  | None -> None
  | Some doc ->
      let content = doc.content in
      let formatted =
        try Lina_format.Formatter.format_string_cst content
        with _ -> content  (* On error, keep original *)
      in
      if String.equal content formatted then
        Some []
      else
        let full_range = Lsp_types.full_document_range content in
        let lsp_range = Lsp_conversions.lsp_range_of_internal full_range in
        let edit = Lsp.Types.TextEdit.create ~range:lsp_range ~newText:formatted in
        let _ = range in
        Some [edit]
