(** Main formatter module.

    Provides the public API for formatting Lina source code.
    Uses CST-based formatting to preserve comments and blank lines
    while normalizing whitespace.

    {2 Basic Usage}

    {[
      (* Format a file in place *)
      match Formatter.format_file ~in_place:true "example.lina" with
      | Ok _ -> print_endline "Formatted!"
      | Error msg -> print_endline ("Error: " ^ msg)

      (* Check if a file is formatted *)
      match Formatter.check_file "example.lina" with
      | Ok true -> print_endline "Already formatted"
      | Ok false -> print_endline "Needs formatting"
      | Error msg -> print_endline ("Error: " ^ msg)
    ]}

    {2 Formatting Behavior}

    The formatter:
    - Normalizes whitespace (removes extra spaces)
    - Preserves all comments (line and block)
    - Preserves blank lines
    - Maintains original code structure *)

(** {1 Configuration} *)

(** Formatter configuration options. *)
type config = {
  line_width : int;
      (** Target line width. Default: 80 *)
  indent_size : int;
      (** Number of spaces per indentation level. Default: 2 *)
}

(** Default configuration with 80 character width and 2-space indentation. *)
val default_config : config

(** {1 Formatting Functions} *)

(** [format_string ?config content] formats source code from a string.

    Parses the input using CST, formats it preserving comments and blank lines,
    and returns the formatted result.

    @param config Formatter configuration (default: {!default_config})
    @param content Source code to format
    @return [Ok formatted] on success, [Error message] on parse error *)
val format_string :
  ?config:config ->
  string ->
  (string, string) result

(** [format_file ?config ?in_place path] formats a source file.

    Reads, parses via CST, and formats the file preserving comments.
    If [in_place] is true and the formatted content differs, overwrites the file.

    @param config Formatter configuration (default: {!default_config})
    @param in_place If true, overwrite the file (default: false)
    @param path Path to the source file
    @return [Ok formatted] on success, [Error message] on failure *)
val format_file :
  ?config:config ->
  ?in_place:bool ->
  string ->
  (string, string) result

(** [check_file ?config path] checks if a file is already formatted.

    Useful for CI pipelines to verify formatting without modifying files.

    @param config Formatter configuration (default: {!default_config})
    @param path Path to the source file
    @return [Ok true] if formatted, [Ok false] if needs formatting,
            [Error message] on failure *)
val check_file :
  ?config:config ->
  string ->
  (bool, string) result

(** {1 CST-Based API Aliases}

    These functions are aliases for backwards compatibility.
    The main formatting functions now use CST internally. *)

(** [format_string_cst ?config content] formats source code using CST.

    This is an alias for {!format_string}. The main formatting functions
    now use CST internally, so this function has identical behavior.

    @param config Formatter configuration (default: {!default_config})
    @param content Source code to format
    @return The formatted source code as a string *)
val format_string_cst :
  ?config:config ->
  string ->
  string

(** [format_file_cst ?config ?in_place path] formats a file using CST.

    This is an alias for {!format_file}.

    @param config Formatter configuration (default: {!default_config})
    @param in_place If true, overwrite the file (default: false)
    @param path Path to the source file
    @return [Ok formatted] on success, [Error message] on failure *)
val format_file_cst :
  ?config:config ->
  ?in_place:bool ->
  string ->
  (string, string) result

(** [check_file_cst ?config path] checks if a file is formatted.

    This is an alias for {!check_file}.

    @param config Formatter configuration (default: {!default_config})
    @param path Path to the source file
    @return [Ok true] if formatted, [Ok false] if needs formatting,
            [Error message] on failure *)
val check_file_cst :
  ?config:config ->
  string ->
  (bool, string) result
