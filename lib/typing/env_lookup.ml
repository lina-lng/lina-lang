(** Environment lookup combinators with built-in error handling.

    See {!Env_lookup} for documentation. *)

open Common

let require loc ~kind find_fn key container =
  match find_fn key container with
  | Some value -> value
  | None ->
    Compiler_error.type_error loc
      (Printf.sprintf "Unbound %s: %s" kind key)

(* Value lookups *)

let find_value_opt = Environment.find_value

let find_value_exn loc name env =
  match Environment.find_value name env with
  | Some binding -> binding
  | None -> Inference_utils.error_unbound_variable loc name

(* Type lookups *)

let find_type_opt = Environment.find_type

let find_type_exn loc name env =
  match Environment.find_type name env with
  | Some decl -> decl
  | None -> Inference_utils.error_unbound_type loc name

(* Constructor lookups *)

let find_constructor_opt = Environment.find_constructor

let find_constructor_exn loc name env =
  match Environment.find_constructor name env with
  | Some info -> info
  | None -> Inference_utils.error_unbound_constructor loc name

(* Module lookups *)

let find_module_opt = Environment.find_module

let find_module_exn loc name env =
  match Environment.find_module name env with
  | Some binding -> binding
  | None -> Inference_utils.error_unbound_module loc name

(* Module type lookups *)

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
