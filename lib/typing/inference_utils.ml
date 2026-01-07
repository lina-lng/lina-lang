(** Shared utilities for type inference.

    See {!Inference_utils} for documentation. *)

open Parsing.Syntax_tree
open Types

let type_of_constant = function
  | ConstantInteger _ -> type_int
  | ConstantFloat _ -> type_float
  | ConstantString _ -> type_string
  | ConstantBoolean _ -> type_bool
  | ConstantUnit -> type_unit

let unify_with_env env loc ty1 ty2 =
  Unification.set_type_lookup (fun path -> Environment.find_type_by_path path env);
  Unification.unify loc ty1 ty2
