(** Stdlib module loader.

    Loads the Lina standard library modules and makes them available
    in the typing environment. *)

let compile_stdlib_module env (stdlib_mod : Lina_stdlib.Stdlib_sources.stdlib_module) =
  let filename = Printf.sprintf "<stdlib/%s>" stdlib_mod.name in

  let result = Compile_utils.compile_module
    ~env
    ~module_name:stdlib_mod.name
    ~filename
    ~source:stdlib_mod.source
    ()
  in

  let exports = Compile_utils.exported_names_of_signature result.signature in
  let lua_code = Option.get result.lua_code in

  (stdlib_mod.name, result.binding, lua_code, exports)

(** Compiled stdlib result: environment and Lua prelude. *)
type stdlib_result = {
  env : Typing.Environment.t;
  lua_prelude : string;
}

let load_stdlib_with_prelude base_env =
  let env, accumulated_codes =
    List.fold_left (fun (env, codes) stdlib_mod ->
      let name, binding, lua_code, exports = compile_stdlib_module env stdlib_mod in
      let env = Typing.Environment.add_module name binding env in
      let wrapped_code = Compile_utils.wrap_module_lua name lua_code exports in
      (env, wrapped_code :: codes)
    ) (base_env, []) Lina_stdlib.Stdlib_sources.all_modules
  in

  let module_code = String.concat "\n" (List.rev accumulated_codes) in
  let lua_prelude = Lina_stdlib.Stdlib_sources.lua_runtime_helpers ^ "\n" ^ module_code in

  { env; lua_prelude }

let cached_stdlib = ref None

(** Get the stdlib result (environment + prelude), using cache. *)
let get_stdlib () =
  match !cached_stdlib with
  | Some result -> result
  | None ->
    let result = load_stdlib_with_prelude Typing.Environment.initial in
    cached_stdlib := Some result;
    result

(** Get the initial environment with stdlib modules loaded. *)
let initial_with_stdlib () =
  (get_stdlib ()).env

(** Get the Lua prelude code for stdlib modules. *)
let stdlib_prelude () =
  (get_stdlib ()).lua_prelude
