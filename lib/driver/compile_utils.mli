(** Compilation utilities shared across stdlib, project, and package loaders.

    This module centralizes common functionality needed by multiple loaders
    to avoid code duplication and ensure consistent behavior. *)

(** {1 Module Compilation} *)

(** Result of compiling a single Lina module. *)
type compile_result = {
  module_name : string;
    (** The module name as it will appear in the environment *)
  binding : Typing.Module_types.module_binding;
    (** The module binding for the typing environment *)
  signature : Typing.Module_types.signature_item list;
    (** The extracted signature items *)
  lua_code : string option;
    (** Generated Lua code, or [None] if [generate_lua] was false *)
}

(** [compile_module ~env ~module_name ~filename ~source ?generate_lua ()]
    compiles Lina source code into a typed module binding.

    Performs the full compilation pipeline:
    1. Parse source into AST
    2. Type inference with the given environment
    3. Signature extraction for the module binding
    4. (Optional) Lambda translation and Lua code generation

    {2 Example}

    {[
      let result = Compile_utils.compile_module
        ~env:base_env
        ~module_name:"MyModule"
        ~filename:"<mymodule>"
        ~source:"let x = 42"
        ()
      in
      let new_env = Environment.add_module result.module_name result.binding env
    ]}

    @param env The typing environment with available modules and types
    @param module_name Name for the resulting module
    @param filename Display name for error messages (e.g., "<stdlib/List>")
    @param source The Lina source code to compile
    @param generate_lua Whether to generate Lua code (default: [true])
    @raise Compiler_error.Error on parse or type errors *)
val compile_module :
  env:Typing.Environment.t ->
  module_name:string ->
  filename:string ->
  source:string ->
  ?generate_lua:bool ->
  unit ->
  compile_result

(** {1 Signature Analysis} *)

(** Extract exported value names from a module signature.

    Only value bindings ([SigValue]) are included because:
    - Types exist only at compile time, not at Lua runtime
    - Nested modules are compiled separately with their own exports
    - Module types are purely compile-time constructs

    @param signature The module signature to analyze
    @return List of exported value names *)
val exported_names_of_signature :
  Typing.Module_types.signature_item list -> string list

(** {1 Lua Code Generation} *)

(** Wrap module Lua code in the IIFE (Immediately Invoked Function Expression) pattern.

    Produces code of the form:
    {v
      local ModuleName = (function()
        -- module internals --
        return { ["export1"] = export1, ["export2"] = export2 }
      end)();
    v}

    The IIFE pattern ensures module internals remain private and only
    explicitly listed values are accessible via [ModuleName.value].

    @param module_name Name for the local variable
    @param lua_code The module's compiled Lua code
    @param exports List of value names to export
    @return Wrapped Lua code string *)
val wrap_module_lua : string -> string -> string list -> string

(** {1 File Discovery} *)

(** Find all Lina source files in a directory recursively.

    Searches the given directory and all subdirectories for files with
    the [.lina] extension. Returns results sorted alphabetically.

    @param dir Directory to search (must exist)
    @return Sorted list of absolute paths to [.lina] files *)
val find_lina_files : string -> string list

(** {1 Topological Sorting} *)

(** Sort items by dependencies using depth-first search with cycle detection.

    Items are sorted so that each item appears after all items it depends on.
    Detects dependency cycles and returns an error message identifying the
    problematic item.

    @param items List of items to sort
    @param get_name Extract the unique name/identifier from an item
    @param get_deps Extract the list of dependency names from an item
    @return [Ok sorted_items] in dependency order (dependencies first),
            or [Error msg] if a cycle is detected *)
val topological_sort :
  'a list ->
  ('a -> string) ->
  ('a -> string list) ->
  ('a list, string) result
