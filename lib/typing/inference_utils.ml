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

let unify ctx loc ty1 ty2 =
  let env = Typing_context.environment ctx in
  unify_with_env env loc ty1 ty2

(** {1 Constructor Arity Checking} *)

let check_constructor_arity loc name ~has_arg ~expects_arg =
  match has_arg, expects_arg with
  | false, false -> ()
  | true, true -> ()
  | true, false ->
    Compiler_error.type_error ~code:Error_code.e_arity_mismatch loc
      (Printf.sprintf "The constructor `%s` doesn't take any arguments, \
                       but we found one here.\n\n\
                       Use just `%s` without an argument." name name)
  | false, true ->
    Compiler_error.type_error ~code:Error_code.e_arity_mismatch loc
      (Printf.sprintf "The constructor `%s` requires an argument, \
                       but none was provided.\n\n\
                       Use `%s value` with an appropriate value." name name)

(** {1 Module Access Validation} *)

let ensure_module_accessible loc (mty : Module_types.module_type) =
  match mty with
  | Module_types.ModTypeSig _ -> ()
  | Module_types.ModTypeFunctor _ ->
    Compiler_error.type_error loc
      "This is a functor, not a regular module. You need to apply it to an \
       argument before accessing its contents.\n\n\
       Use `Functor(Argument).value` to access values."
  | Module_types.ModTypeIdent _ ->
    Compiler_error.type_error loc
      "This module type is abstract, so we can't access its contents directly.\n\n\
       The concrete implementation is hidden."

let error_cannot_access_in_module_type loc mty action =
  let kind_desc = match mty with
    | Module_types.ModTypeFunctor _ -> "a functor"
    | Module_types.ModTypeIdent _ -> "an abstract module type"
    | Module_types.ModTypeSig _ ->
        Compiler_error.internal_error "error_cannot_access_in_module_type called on signature"
  in
  Compiler_error.type_error loc
    (Printf.sprintf "We can't %s because this is %s.\n\n\
                     The contents are not directly accessible." action kind_desc)

(** {1 Value Restriction and Generalization} *)

(** Compute binding scheme with environment for proper variance checking. *)
let compute_binding_scheme_with_env ~level ~env typed_expr ty =
  if Value_check.is_value typed_expr then
    Type_scheme.generalize ~level ty
  else begin
    (* Relaxed value restriction: generalize covariant-only variables.
       Use the environment to look up declared variances for user-defined types. *)
    let type_lookup path = Environment.find_type_by_path path env in
    let predicate tv ty =
      Value_check.can_generalize_relaxed_with_lookup ~type_lookup tv ty
    in
    Type_scheme.generalize_with_filter ~level predicate ty
  end

let compute_binding_scheme ~level typed_expr ty =
  if Value_check.is_value typed_expr then
    Type_scheme.generalize ~level ty
  else
    (* Relaxed value restriction: generalize covariant-only variables.
       Note: This version doesn't look up declared variances - use
       compute_binding_scheme_with_env for full accuracy. *)
    Type_scheme.generalize_with_filter ~level Value_check.can_generalize_relaxed ty

(** {1 Module Path Extraction} *)

(** [extract_typed_module_path mexpr] extracts a path from a typed module expression.
    Handles simple paths, functor applications, and constrained modules.

    For applicative functor semantics, we need paths for functor applications too
    so that nested applications like Id(Wrap(Base)) can perform proper path substitution.

    @param mexpr The typed module expression
    @return [Some path] if a path can be constructed, [None] otherwise *)
let rec extract_typed_module_path mexpr =
  match mexpr.Typed_tree.module_desc with
  | Typed_tree.TypedModulePath path -> Some path
  | Typed_tree.TypedModuleApply (func_expr, arg_expr) ->
    (* For functor application F(M), construct PathApply(F_path, M_path) *)
    begin match extract_typed_module_path func_expr, extract_typed_module_path arg_expr with
    | Some func_path, Some arg_path -> Some (Types.PathApply (func_path, arg_path))
    | _ -> None
    end
  | Typed_tree.TypedModuleConstraint (inner_expr, _) ->
    (* For constrained module (M : S), use the inner module's path *)
    extract_typed_module_path inner_expr
  | _ -> None

(** {1 GADT Existential Escape Checking} *)

(** [check_existential_escape_or_error loc patterns result_type] checks that no
    existential type variables from patterns escape through the result type.

    Existential type variables introduced by GADT patterns in function parameters
    or let bindings cannot appear in the function/expression's result type.

    @param loc Source location for error messages
    @param patterns List of typed patterns that may introduce existentials
    @param result_type The type to check for escaping existentials
    @raise Type_error if an existential would escape *)
let check_existential_escape_or_error loc patterns result_type =
  let existential_ids =
    List.concat_map Gadt.collect_existentials_from_pattern patterns
  in
  match Gadt.check_existential_escape existential_ids result_type with
  | Some _escaped_id ->
    Compiler_error.type_error loc
      "This expression has a type containing an existential type variable \
       that would escape its scope"
  | None -> ()

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

(** Built-in type names that are always available. *)
let builtin_type_names = ["int"; "float"; "string"; "bool"; "unit"; "ref"]

(** Collect candidate names from environment for a given kind. *)
let collect_candidates env = function
  | UnboundVariable ->
    Environment.fold_values (fun name _ _ names -> name :: names) env []
  | UnboundConstructor ->
    Environment.fold_constructors (fun name _ names -> name :: names) env []
  | UnboundModule ->
    Environment.fold_modules (fun name _ names -> name :: names) env []
  | UnboundType ->
    let user_types = Environment.fold_types (fun name _ names -> name :: names) env [] in
    builtin_type_names @ user_types
  | UnboundModuleType ->
    []

(** Human-readable description for unbound entity error. *)
let unbound_message kind name =
  match kind with
  | UnboundVariable ->
    Printf.sprintf "We couldn't find a variable named `%s`" name
  | UnboundConstructor ->
    Printf.sprintf "We couldn't find a constructor named `%s`" name
  | UnboundModule ->
    Printf.sprintf "We couldn't find a module named `%s`" name
  | UnboundType ->
    Printf.sprintf "We couldn't find a type named `%s`" name
  | UnboundModuleType ->
    Printf.sprintf "We couldn't find a module type named `%s`" name

(** Get the error code for an unbound entity kind. *)
let unbound_kind_to_code = function
  | UnboundVariable -> Error_code.e_unbound_value
  | UnboundConstructor -> Error_code.e_unbound_constructor
  | UnboundModule -> Error_code.e_unbound_module
  | UnboundType -> Error_code.e_unbound_type
  | UnboundModuleType -> Error_code.e_unbound_module_type

(** Raise an unbound error with suggestions from the environment.

    This creates a human-oriented error message with "Did you mean?" suggestions
    based on similar names in the environment. Shows multiple suggestions when
    there are several close matches. *)
let error_unbound_with_env ~env kind loc name =
  let candidates = collect_candidates env kind in
  let message = unbound_message kind name in
  let suggestion = Common.Suggestions.format_suggestions ~target:name ~candidates () in

  let full_message =
    if suggestion = "" then message
    else Printf.sprintf "%s\n\n%s" message suggestion
  in

  let code = unbound_kind_to_code kind in
  Compiler_error.type_error ~code loc full_message

(** Raise an unbound error without environment context.

    Falls back to the simple message format when environment is not available. *)
let error_unbound kind loc name =
  let code = unbound_kind_to_code kind in
  Compiler_error.type_error ~code loc
    (Printf.sprintf "Unbound %s: %s" (unbound_kind_to_string kind) name)

(* Convenience functions for backward compatibility *)
let error_unbound_variable loc name = error_unbound UnboundVariable loc name
let error_unbound_constructor loc name = error_unbound UnboundConstructor loc name
let error_unbound_module loc name = error_unbound UnboundModule loc name
let error_unbound_type loc name = error_unbound UnboundType loc name
let error_unbound_module_type loc name = error_unbound UnboundModuleType loc name

(* Enhanced versions with environment *)
let error_unbound_variable_with_env ~env loc name =
  error_unbound_with_env ~env UnboundVariable loc name

let error_unbound_constructor_with_env ~env loc name =
  error_unbound_with_env ~env UnboundConstructor loc name

let error_unbound_module_with_env ~env loc name =
  error_unbound_with_env ~env UnboundModule loc name

let error_unbound_type_with_env ~env loc name =
  error_unbound_with_env ~env UnboundType loc name

let error_unbound_module_type_with_env ~env loc name =
  error_unbound_with_env ~env UnboundModuleType loc name

(** {1 Label Conversion and Formatting} *)

let convert_syntax_label = function
  | Parsing.Syntax_tree.Nolabel -> Types.Nolabel
  | Parsing.Syntax_tree.Labelled s -> Types.Labelled s
  | Parsing.Syntax_tree.Optional s -> Types.Optional s

let format_arg_label = function
  | Types.Labelled name -> "~" ^ name
  | Types.Optional name -> "?" ^ name
  | Types.Nolabel -> "_"

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

(** {1 Constructor Lookup} *)

(** Result of looking up and instantiating a constructor. *)
type constructor_result = {
  constructor_info : Types.constructor_info;
  expected_arg_type : Types.type_expression option;
  result_type : Types.type_expression;
}

(** [lookup_constructor ctx loc name] looks up a constructor and instantiates it.

    @param ctx The typing context
    @param loc Source location for error messages
    @param name The constructor name
    @return Constructor info with instantiated types
    @raise Type_error if constructor is not found *)
let lookup_constructor ctx loc name =
  let env = Typing_context.environment ctx in
  match Environment.find_constructor name env with
  | None -> error_unbound_constructor_with_env ~env loc name
  | Some constructor_info ->
    let expected_arg_type, result_type = instantiate_constructor_with_ctx ctx constructor_info in
    { constructor_info; expected_arg_type; result_type }

(** Convert a longident to a module path (list of strings) and constructor name.
    For example, [Ldot(Ldot(Lident "M", "N"), "Some")] becomes (["M"; "N"], "Some").

    The longident structure is: Ldot(Ldot(Lident "M", "N"), "Some")
    where the rightmost name is the constructor, and all preceding are the path. *)
let split_longident (longident : Parsing.Syntax_tree.longident) : string list * string =
  let rec collect_path lid =
    match lid.Location.value with
    | Parsing.Syntax_tree.Lident name -> [name]
    | Parsing.Syntax_tree.Ldot (prefix, name) -> collect_path prefix @ [name]
  in
  let all_parts = collect_path longident in
  match List.rev all_parts with
  | [] -> Common.Compiler_error.internal_error "split_longident received empty longident"
  | ctor_name :: path_rev -> (List.rev path_rev, ctor_name)

(** [lookup_constructor_longident ctx loc longident] looks up a constructor by longident.
    Handles both simple names like [Some] and qualified names like [M.Some].

    @param ctx The typing context
    @param loc Source location for error messages
    @param longident The constructor's longident (simple or qualified)
    @return Constructor info with instantiated types
    @raise Type_error if constructor is not found *)
let lookup_constructor_longident ctx loc (longident : Parsing.Syntax_tree.longident) =
  let env = Typing_context.environment ctx in
  let (path, ctor_name) = split_longident longident in
  match Environment.find_constructor_by_path path ctor_name env with
  | None ->
    let full_name = match path with
      | [] -> ctor_name
      | _ -> String.concat "." path ^ "." ^ ctor_name
    in
    error_unbound_constructor_with_env ~env loc full_name
  | Some constructor_info ->
    let expected_arg_type, result_type = instantiate_constructor_with_ctx ctx constructor_info in
    { constructor_info; expected_arg_type; result_type }

(** [check_private_type ctx loc ctor_info] checks that a constructor's type is not private.

    Private types cannot be constructed (but can be pattern matched).

    @param ctx The typing context
    @param loc Source location for error messages
    @param ctor_info The constructor info to check
    @raise Type_error if the type is private *)
let check_private_type ctx loc ctor_info =
  let env = Typing_context.environment ctx in
  match Environment.find_type ctor_info.Types.constructor_type_name env with
  | Some type_decl when type_decl.Types.declaration_private ->
    Compiler_error.type_error loc
      (Printf.sprintf "The type `%s` is private and cannot be constructed \
                       outside its defining module.\n\n\
                       Private types can be pattern matched but not created directly."
         ctor_info.Types.constructor_type_name)
  | _ -> ()

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

(** {1 Callback Type Aliases} *)

type expression_infer_fn =
  Typing_context.t ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

type expression_infer_expected_fn =
  Typing_context.t ->
  Types.type_expression option ->
  Parsing.Syntax_tree.expression ->
  Typed_tree.typed_expression * Typing_context.t

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
