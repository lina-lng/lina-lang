type options = {
  dump_ast : bool;
  dump_typed : bool;
  dump_lambda : bool;
  warning_config : Common.Warning_config.t;
}

val default_options : options

val compile_string : options -> string -> string -> (string, string) result
val compile_file : options -> string -> (string, string) result

(** Compile with a pre-configured environment (for project dependencies).
    @param env Typing environment with project dependencies loaded
    @param lua_prelude Lua code for dependencies to prepend *)
val compile_string_with_env :
  options ->
  env:Typing.Environment.t ->
  lua_prelude:string ->
  string ->
  string ->
  (string, string) result

val compile_file_with_env :
  options ->
  env:Typing.Environment.t ->
  lua_prelude:string ->
  string ->
  (string, string) result
