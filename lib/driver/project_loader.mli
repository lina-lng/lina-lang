(** Project dependency loader.

    Loads external Lina projects as dependencies into the typing environment.
    This enables one Lina project to use modules from another project
    specified via path in lina.toml. *)

(** Result of loading project dependencies. *)
type load_result = {
  env : Typing.Environment.t;
      (** Updated typing environment with dependency modules *)
  lua_prelude : string;
      (** Lua code to include before user code for dependency modules *)
}

(** [resolve_dependency_path ~root dep] resolves the dependency path.

    @param root The current project root directory
    @param dep The dependency specification
    @return [Ok abs_path] if the path exists, [Error msg] otherwise *)
val resolve_dependency_path :
  root:string ->
  Config.lina_dependency ->
  (string, string) result

(** [load_project_dependency ~root ~dep ~env] loads a single Lina project dependency.

    Compiles the dependency project, extracts its module signatures,
    and adds them to the typing environment.

    @param root Current project root directory
    @param dep Dependency specification from lina.toml
    @param env Current typing environment
    @return [Ok result] with updated env and Lua prelude, [Error msg] on failure *)
val load_project_dependency :
  root:string ->
  dep:Config.lina_dependency ->
  env:Typing.Environment.t ->
  (load_result, string) result

(** [topological_sort_deps deps] sorts dependencies in topological order.

    Dependencies are sorted so that a project comes after all projects it
    depends on. This ensures correct loading order for transitive dependencies.

    @param deps List of (name, result) pairs where result contains
           (dep_spec, resolved_path, list_of_dependency_names)
    @return [Ok sorted_list] with dependencies in correct load order,
            [Error msg] if a cycle is detected *)
val topological_sort_deps :
  (string * (Config.lina_dependency * string * string list, string) result) list ->
  ((string * Config.lina_dependency * string) list, string) result

(** [load_all_dependencies ~root ~deps ~env] loads all Lina project dependencies.

    Automatically resolves transitive dependencies and loads them in
    topologically sorted order (dependencies before dependents).
    Optional dependencies that fail to load are skipped silently.

    @param root Current project root directory
    @param deps List of dependency specifications
    @param env Current typing environment
    @return [Ok result] with fully updated env and combined Lua prelude *)
val load_all_dependencies :
  root:string ->
  deps:Config.lina_dependency list ->
  env:Typing.Environment.t ->
  (load_result, string) result
