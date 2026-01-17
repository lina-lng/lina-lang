(** Environment lookup combinators with built-in error handling.

    See {!Env_lookup} for documentation. *)

open Common

let find_exn find_fn error_fn loc name env =
  match find_fn name env with
  | Some found -> found
  | None -> error_fn loc name

let find_value_opt = Environment.find_value
let find_value_exn loc = find_exn Environment.find_value Inference_utils.error_unbound_variable loc

let find_type_opt = Environment.find_type
let find_type_exn loc = find_exn Environment.find_type Inference_utils.error_unbound_type loc

let find_constructor_opt = Environment.find_constructor
let find_constructor_exn loc = find_exn Environment.find_constructor Inference_utils.error_unbound_constructor loc

let find_module_opt = Environment.find_module
let find_module_exn loc = find_exn Environment.find_module Inference_utils.error_unbound_module loc

(** {1 Module Type Lookups} *)

let find_module_type_opt name env =
  match Environment.find_module_type name env with
  | Some (Some mty) -> Some mty
  | _ -> None

let find_module_type_exn loc name env =
  match Environment.find_module_type name env with
  | Some (Some mty) -> mty
  | Some None ->
    Compiler_error.type_error loc
      (Printf.sprintf "Module type %s is abstract" name)
  | None -> Inference_utils.error_unbound_module_type loc name
