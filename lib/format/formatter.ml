(** Main formatter module.

    Provides the public API for formatting Lina source code.
    Uses CST-based formatting to preserve comments and blank lines. *)

(** {1 Configuration} *)

(** Formatter configuration options. *)
type config = {
  line_width : int;
      (** Target line width. Default: 80 *)
  indent_size : int;
      (** Number of spaces per indentation level. Default: 2 *)
}

(** Default configuration. *)
let default_config = {
  line_width = 80;
  indent_size = 2;
}

(** {1 Formatting Functions} *)

(** Format source code from a string.

    Parses the input using CST, formats it preserving comments and blank lines,
    and returns the formatted result.

    @param config Formatter configuration
    @param content Source code to format
    @return [Ok formatted] on success, [Error message] on parse error *)
let format_string ?(config = default_config) content =
  Ok (Format_cst.format_string ~width:config.line_width ~indent:config.indent_size content)

(** Format a source file.

    Reads, parses via CST, formats the file preserving comments.
    If [in_place] is true and the formatted content differs, overwrites the file.

    @param config Formatter configuration
    @param in_place If true, overwrite the file with formatted content
    @param path Path to the source file
    @return [Ok formatted] on success, [Error message] on failure *)
let format_file ?(config = default_config) ?(in_place = false) path =
  try
    let content =
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      s
    in
    let formatted = Format_cst.format_string ~width:config.line_width ~indent:config.indent_size content in

    if in_place && formatted <> content then begin
      let oc = open_out path in
      output_string oc formatted;
      close_out oc
    end;

    Ok formatted
  with
  | Sys_error msg -> Error msg

(** Check if a file is already formatted.

    @param config Formatter configuration
    @param path Path to the source file
    @return [Ok true] if formatted, [Ok false] if not, [Error msg] on failure *)
let check_file ?(config = default_config) path =
  try
    let content =
      let ic = open_in path in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      s
    in
    let formatted = Format_cst.format_string ~width:config.line_width ~indent:config.indent_size content in
    Ok (formatted = content)
  with
  | Sys_error msg -> Error msg

(** {1 CST-Based Formatting Aliases}

    These are aliases for backwards compatibility. The main formatting
    functions now use CST internally. *)

(** Format source code using CST.

    This function preserves all comments and blank lines from the original
    source while normalizing whitespace.

    @param config Formatter configuration
    @param content Source code to format
    @return The formatted source code as a string *)
let format_string_cst ?(config = default_config) content =
  Format_cst.format_string ~width:config.line_width ~indent:config.indent_size content

(** Format a file using CST.

    Reads, parses via CST, and formats the file. If [in_place] is true
    and the formatted content differs, overwrites the file.

    @param config Formatter configuration
    @param in_place If true, overwrite the file
    @param path Path to the source file
    @return [Ok formatted] on success, [Error message] on failure *)
let format_file_cst ?(config = default_config) ?(in_place = false) path =
  format_file ~config ~in_place path

(** Check if a file is formatted per CST rules.

    @param config Formatter configuration
    @param path Path to the source file
    @return [Ok true] if formatted, [Ok false] if not, [Error msg] on failure *)
let check_file_cst ?(config = default_config) path =
  check_file ~config path
