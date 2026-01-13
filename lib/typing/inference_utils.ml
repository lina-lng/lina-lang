(** Shared utilities for type inference.

    See {!Inference_utils} for documentation. *)

open Common
open Parsing.Syntax_tree
open Types

let type_of_constant = function
  | ConstantInteger _ -> type_int
  | ConstantFloat _ -> type_float
  | ConstantString _ -> type_string
  | ConstantBoolean _ -> type_bool
  | ConstantUnit -> type_unit

let unify_with_env env loc ty1 ty2 =
  let type_lookup path = Environment.find_type_by_path path env in
  Unification.unify ~type_lookup loc ty1 ty2

(** {1 Constructor Arity Checking} *)

let check_constructor_arity loc name ~has_arg ~expects_arg =
  match has_arg, expects_arg with
  | false, false -> ()
  | true, true -> ()
  | true, false ->
    Compiler_error.type_error loc
      (Printf.sprintf "Constructor %s does not take an argument" name)
  | false, true ->
    Compiler_error.type_error loc
      (Printf.sprintf "Constructor %s requires an argument" name)

(** {1 Module Access Validation} *)

let ensure_module_accessible loc (mty : Module_types.module_type) =
  match mty with
  | Module_types.ModTypeSig _ -> ()
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc "Cannot access value in a functor"
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc "Cannot access value in an abstract module type"

(** {1 Value Restriction and Generalization} *)

let compute_binding_scheme ~level typed_expr ty =
  if Value_check.is_value typed_expr then
    Type_scheme.generalize ~level ty
  else
    (* Relaxed value restriction: generalize covariant-only variables *)
    Type_scheme.generalize_with_filter ~level Value_check.can_generalize_relaxed ty

(** {1 Module Path Extraction} *)

(** [extract_typed_module_path mexpr] extracts a path from a typed module expression
    if it's a simple path reference.

    @param mexpr The typed module expression
    @return [Some path] if the expression is a simple module path, [None] otherwise *)
let extract_typed_module_path mexpr =
  match mexpr.Typed_tree.module_desc with
  | Typed_tree.TypedModulePath path -> Some path
  | _ -> None

(** {1 Constructor Instantiation} *)

(** [instantiate_constructor_with_ctx ctx ctor] instantiates a constructor
    using the context's current level for fresh type variables.

    This is a convenience wrapper around {!Type_utils.instantiate_constructor}
    that uses the typing context's level instead of requiring a fresh_var function.

    @param ctx The typing context (used for level)
    @param ctor The constructor to instantiate
    @return Tuple of (argument_type option, result_type) *)
let instantiate_constructor_with_ctx ctx ctor =
  let level = Typing_context.current_level ctx in
  let fresh_var () = Types.new_type_variable_at_level level in
  Type_utils.instantiate_constructor ~fresh_var ctor

(** {1 Signature Match Context Creation} *)

(** Create a module type lookup function from a typing context.
    Handles both simple paths (PathIdent) and qualified paths (PathDot). *)
let make_module_type_lookup ctx =
  let env = Typing_context.environment ctx in
  fun path -> Environment.find_module_type_by_path path env

(** Create a signature matching context from a typing context. *)
let make_match_context ctx =
  let env = Typing_context.environment ctx in
  Signature_match.create_context
    ~type_lookup:(fun path -> Environment.find_type_by_path path env)
    ~module_type_lookup:(make_module_type_lookup ctx)

(** {1 Tolerant Inference Error Types} *)

(** Unification error details for tolerant inference.
    Used by LSP features that need to continue after errors. *)
type unification_error_details = {
  expected : Types.type_expression;
  actual : Types.type_expression;
  location : Common.Location.t;
  message : string;
}

(** Error information from tolerant inference.
    Captures both compiler errors and unification failures. *)
type inference_error =
  | CompilerError of Common.Compiler_error.t
  | UnificationError of unification_error_details

(** {1 Error Helpers} *)

(** Kind of unbound entity for error messages. *)
type unbound_kind =
  | UnboundVariable
  | UnboundConstructor
  | UnboundModule
  | UnboundType
  | UnboundModuleType

let unbound_kind_to_string = function
  | UnboundVariable -> "variable"
  | UnboundConstructor -> "constructor"
  | UnboundModule -> "module"
  | UnboundType -> "type"
  | UnboundModuleType -> "module type"

let error_unbound kind loc name =
  Compiler_error.type_error loc
    (Printf.sprintf "Unbound %s: %s" (unbound_kind_to_string kind) name)

(* Convenience functions for backward compatibility *)
let error_unbound_variable loc name = error_unbound UnboundVariable loc name
let error_unbound_constructor loc name = error_unbound UnboundConstructor loc name
let error_unbound_module loc name = error_unbound UnboundModule loc name
let error_unbound_type loc name = error_unbound UnboundType loc name
let error_unbound_module_type loc name = error_unbound UnboundModuleType loc name
