(** Multi-file compilation support.

    Handles compilation of multiple .lina files with dependency tracking.
    Each .lina file becomes a Lua module that can be required by others.
*)

(** Convert filename to module name: list.lina -> List *)
val module_name_of_filename : string -> string

(** Extract external module references from a structure *)
val extract_imports : Parsing.Syntax_tree.structure -> string list

(** File information for dependency tracking *)
type file_info = {
  filename : string;
  module_name : string;
  ast : Parsing.Syntax_tree.structure;
  imports : string list;
}

(** Parse a file and extract its metadata *)
val parse_file : string -> (file_info, string) result

(** Sort files by dependencies (topological sort) *)
val topological_sort : file_info list -> (file_info list, string) result

(** Project compilation options *)
type project_options = {
  output_dir : string option;
  entry_point : string option;
  pipeline_options : Pipeline.options;
}

(** Compile multiple files to Lua modules.
    Returns list of (module_name, lua_code) pairs in dependency order. *)
val compile_project : project_options -> string list -> ((string * string) list, string) result
