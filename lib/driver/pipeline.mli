type options = {
  dump_ast : bool;
  dump_typed : bool;
  dump_lambda : bool;
  warning_config : Warning_config.t;
}

val default_options : options

val compile_string : options -> string -> string -> (string, string) result
val compile_file : options -> string -> (string, string) result
