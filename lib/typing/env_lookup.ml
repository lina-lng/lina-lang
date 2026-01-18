(** Environment lookup combinators with built-in error handling.

    See {!Env_lookup} for documentation. *)

open Common

let find_exn find_fn error_fn loc name env =
  match find_fn name env with
  | Some found -> found
  | None -> error_fn loc name

let find_exn_with_env find_fn error_fn loc name env =
  match find_fn name env with
  | Some found -> found
  | None -> error_fn ~env loc name

let find_value_opt = Environment.find_value

let find_value_exn loc = find_exn Environment.find_value Inference_utils.error_unbound_variable loc

let find_value_exn_with_suggestions loc name env =
  find_exn_with_env Environment.find_value Inference_utils.error_unbound_variable_with_env loc name env

let find_type_opt = Environment.find_type

let find_type_exn loc = find_exn Environment.find_type Inference_utils.error_unbound_type loc

let find_type_exn_with_suggestions loc name env =
  find_exn_with_env Environment.find_type Inference_utils.error_unbound_type_with_env loc name env

let find_constructor_opt = Environment.find_constructor

let find_constructor_exn loc = find_exn Environment.find_constructor Inference_utils.error_unbound_constructor loc

let find_constructor_exn_with_suggestions loc name env =
  find_exn_with_env Environment.find_constructor Inference_utils.error_unbound_constructor_with_env loc name env

let find_module_opt = Environment.find_module

let find_module_exn loc = find_exn Environment.find_module Inference_utils.error_unbound_module loc

let find_module_exn_with_suggestions loc name env =
  find_exn_with_env Environment.find_module Inference_utils.error_unbound_module_with_env loc name env

let find_module_type_opt name env =
  match Environment.find_module_type name env with
  | Some (Some mty) -> Some mty
  | _ -> None

let find_module_type_impl handle_not_found loc name env =
  match Environment.find_module_type name env with
  | Some (Some mty) -> mty
  | Some None ->
    Compiler_error.type_error loc
      (Printf.sprintf "Module type %s is abstract" name)
  | None -> handle_not_found ()

let find_module_type_exn loc name env =
  find_module_type_impl
    (fun () -> Inference_utils.error_unbound_module_type loc name)
    loc name env

let find_module_type_exn_with_suggestions loc name env =
  find_module_type_impl
    (fun () -> Inference_utils.error_unbound_module_type_with_env ~env loc name)
    loc name env
