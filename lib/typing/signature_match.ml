(** Signature matching for modules.

    Implements the algorithm to check if a module implementation
    satisfies a module signature specification.
*)

open Common
open Types

(** {1 Match Context}

    Context for signature matching operations, containing lookup functions
    for type aliases and module types. *)

(** Context for signature matching operations. *)
type match_context = {
  type_lookup : Unification.type_lookup;
  module_type_lookup : path -> Module_types.module_type option;
}

(** Create a match context with the given lookup functions. *)
let create_context ~type_lookup ~module_type_lookup =
  { type_lookup; module_type_lookup }

(** Create an expansion state from a match context. *)
let create_expansion_state ctx =
  Module_expansion.create_state ~env_lookup:ctx.module_type_lookup

(** Expand a ModTypeIdent to its definition if available.

    Uses the robust Module_expansion module for cycle detection and
    better error handling. Falls back to the unexpanded type if expansion fails. *)
let expand_module_type ctx mty =
  let state = create_expansion_state ctx in
  match Module_expansion.try_expand state mty with
  | Some expanded -> expanded
  | None -> mty  (* Can't expand - leave as is *)

(** Module strengthening: Makes abstract types concrete by binding them
    to their path. This ensures that `module N = M` makes N.t = M.t.

    This involves two operations:
    1. Update abstract type declarations with manifests pointing to qualified paths
    2. Substitute PathLocal references in value types with qualified paths

    For example, strengthening `sig type t; val show : t -> string end` with path M:
    - `type t` becomes `type t = M.t`
    - `val show : t -> string` becomes `val show : M.t -> string` *)

(** Collect all type names defined in a signature. *)
let collect_type_names sig_ =
  List.filter_map (function
    | Module_types.SigType (name, _) -> Some name
    | _ -> None
  ) sig_

(** Substitute PathLocal type references with qualified paths.
    Only substitutes types that are defined in this signature. *)
let qualify_type_in_signature path type_names ty =
  Type_traversal.qualify_path_local ~type_names ~prefix:path ty

let qualify_scheme_in_signature path type_names scheme =
  { scheme with Types.body = qualify_type_in_signature path type_names scheme.Types.body }

(** Qualify types inside a constructor declaration. *)
let qualify_constructor_in_signature path type_names (ctor : Types.constructor_info) =
  let arg_type = Option.map (qualify_type_in_signature path type_names) ctor.constructor_argument_type in
  let result_type = qualify_type_in_signature path type_names ctor.constructor_result_type in
  { ctor with
    constructor_argument_type = arg_type;
    constructor_result_type = result_type;
  }

(** Qualify types inside a type declaration (especially in variant constructors). *)
let qualify_declaration_in_signature path type_names (decl : Types.type_declaration) =
  match decl.declaration_kind with
  | Types.DeclarationVariant constructors ->
    let qualified_ctors = List.map (qualify_constructor_in_signature path type_names) constructors in
    { decl with declaration_kind = Types.DeclarationVariant qualified_ctors }
  | _ -> decl

let rec strengthen_signature path sig_ =
  (* First, collect all type names defined in this signature *)
  let type_names = collect_type_names sig_ in
  (* Then process each item *)
  List.map (fun item ->
    match item with
    | Module_types.SigType (name, decl) ->
      (* First qualify types inside the declaration (e.g., in variant constructors) *)
      let qualified_decl = qualify_declaration_in_signature path type_names decl in
      begin match qualified_decl.Types.declaration_manifest with
      | Some _ ->
        (* Type already has a manifest (it's a type alias) - keep it as is.
           Overwriting would create a cycle: type t = int -> type t = M.t *)
        Module_types.SigType (name, qualified_decl)
      | None ->
        (* Abstract type - make it concrete by binding to path.t *)
        let type_path = Types.PathDot (path, name) in
        let manifest = Types.TypeConstructor (type_path,
          List.map (fun tv -> Types.TypeVariable tv) qualified_decl.Types.declaration_parameters) in
        Module_types.SigType (name, { qualified_decl with declaration_manifest = Some manifest })
      end
    | Module_types.SigValue (name, desc) ->
      (* Qualify PathLocal references to types defined in this signature *)
      let qualified_type = qualify_scheme_in_signature path type_names desc.Module_types.value_type in
      Module_types.SigValue (name, { desc with value_type = qualified_type })
    | Module_types.SigModule (mod_name, mty) ->
      (* Recursively strengthen nested modules *)
      Module_types.SigModule (mod_name, strengthen_module_type (Types.PathDot (path, mod_name)) mty)
    | other -> other
  ) sig_

and strengthen_module_type path mty =
  match mty with
  | Module_types.ModTypeSig sig_ ->
    Module_types.ModTypeSig (strengthen_signature path sig_)
  | Module_types.ModTypeFunctor _ ->
    (* Don't strengthen functor bodies - they're not yet applied *)
    mty
  | Module_types.ModTypeIdent _ ->
    (* Named module types - could expand and strengthen *)
    mty

(** Result of signature matching *)
type match_error =
  | MissingValue of string
  | MissingType of string
  | MissingModule of string
  | TypeMismatch of string * type_expression * type_expression
  | ModuleTypeMismatch of string * string
  | WeakTypeEscape of string * type_expression  (** Weak type cannot satisfy polymorphic signature *)

type match_result = (unit, match_error) result

(** List of all match errors. Used by accumulating match functions. *)
type match_errors = match_error list

(** Create a fresh type variable for signature matching.
    Uses generic_level since these are free variables for structural comparison. *)
let fresh_match_var () =
  Types.new_type_variable_at_level Types.generic_level

(** Check if a type contains any weak type variables.
    Weak variables are non-generalizable and cannot satisfy polymorphic signatures. *)
let has_weak_variables = Type_traversal.has_weak_variables

(** Check if a type scheme has weak variables in its body.
    This is used to detect weak type escape at module boundaries. *)
let scheme_has_weak_variables scheme =
  has_weak_variables scheme.Types.body

(** Check if an implementation type is compatible with a specification type.
    The implementation type must be at least as general as the spec type.
    Also checks that weak types don't escape to polymorphic positions. *)
let check_type_compatibility ctx _loc impl_scheme spec_scheme =
  (* Check for weak type escape: if spec is polymorphic but impl has weak vars *)
  let spec_is_polymorphic = spec_scheme.Types.quantified_variables <> [] in
  if spec_is_polymorphic && scheme_has_weak_variables impl_scheme then
    Error (WeakTypeEscape ("", impl_scheme.Types.body))
  else begin
    (* Do structural comparison after instantiation *)
    let impl_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var impl_scheme in
    let spec_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var spec_scheme in
    (* Try to unify - if it succeeds, implementation is compatible *)
    try
      Unification.unify ~type_lookup:ctx.type_lookup Location.none impl_ty spec_ty;
      Ok ()
    with
    | Compiler_error.Error _ ->
      Error (TypeMismatch ("", impl_ty, spec_ty))
  end

(** Check if two manifests are compatible via unification. *)
let check_manifest_unification ctx name impl_decl spec_decl impl_manifest spec_manifest =
  let impl_scheme = {
    quantified_variables = impl_decl.declaration_parameters;
    body = impl_manifest
  } in
  let spec_scheme = {
    quantified_variables = spec_decl.declaration_parameters;
    body = spec_manifest
  } in

  try
    let impl_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var impl_scheme in
    let spec_ty = Type_scheme.instantiate ~fresh_var:fresh_match_var spec_scheme in
    Unification.unify ~type_lookup:ctx.type_lookup Location.none impl_ty spec_ty;
    Ok ()
  with Compiler_error.Error _ ->
    Error (TypeMismatch (name, impl_manifest, spec_manifest))

(** Check manifest compatibility between impl and spec type declarations. *)
let check_manifest_compatibility ctx name impl_decl spec_decl =
  match spec_decl.declaration_manifest, impl_decl.declaration_manifest with
  | None, _ ->
      Ok ()
  | Some spec_manifest, None ->
      Error (TypeMismatch (name, TypeConstructor (PathLocal name, []), spec_manifest))
  | Some spec_manifest, Some impl_manifest ->
      check_manifest_unification ctx name impl_decl spec_decl impl_manifest spec_manifest

(** Check if two type declarations are compatible.
    - Parameter counts must match
    - If spec has manifest, impl must have compatible manifest
    - If spec is abstract, impl can be anything *)
let check_type_declaration_compatibility ctx name impl_decl spec_decl =
  let impl_params = List.length impl_decl.declaration_parameters in
  let spec_params = List.length spec_decl.declaration_parameters in

  if impl_params <> spec_params then
    Error (TypeMismatch (name,
      TypeConstructor (PathLocal name, []),
      TypeConstructor (PathLocal name, [])))
  else
    check_manifest_compatibility ctx name impl_decl spec_decl

(** Build a type substitution from impl signature to replace spec's PathLocal types.
    For each type in spec, if impl has a manifest, map PathLocal(name) -> impl manifest. *)
let build_type_substitution impl_sig =
  List.filter_map (function
    | Module_types.SigType (name, impl_decl) ->
      begin match impl_decl.Types.declaration_manifest with
      | Some manifest -> Some (name, impl_decl.Types.declaration_parameters, manifest)
      | None -> None
      end
    | _ -> None
  ) impl_sig

(** Substitute PathLocal types in a type expression using the substitution. *)
let rec substitute_local_types subst ty =
  let ty = Types.representative ty in
  match ty with
  | Types.TypeConstructor (Types.PathLocal name, args) ->
    begin match List.find_opt (fun (n, _, _) -> n = name) subst with
    | Some (_, params, manifest) ->
      (* Substitute type parameters *)
      let substituted_args = List.map (substitute_local_types subst) args in
      Type_utils.substitute_type_params params substituted_args manifest
    | None -> ty
    end
  | Types.TypeConstructor (path, args) ->
    Types.TypeConstructor (path, List.map (substitute_local_types subst) args)
  | Types.TypeArrow (label, arg, res) ->
    Types.TypeArrow (label, substitute_local_types subst arg, substitute_local_types subst res)
  | Types.TypeTuple elems ->
    Types.TypeTuple (List.map (substitute_local_types subst) elems)
  | Types.TypeRecord row ->
    Types.TypeRecord (substitute_local_types_in_row subst row)
  | _ -> ty

and substitute_local_types_in_row subst row =
  let new_fields = List.map (fun (name, field) ->
    match field with
    | Types.RowFieldPresent ty ->
      (name, Types.RowFieldPresent (substitute_local_types subst ty))
  ) row.Types.row_fields in
  { Types.row_fields = new_fields;
    row_more = substitute_local_types subst row.Types.row_more }

(** Format a match error for display *)
let format_match_error = function
  | MissingValue name ->
    Printf.sprintf "Missing value: %s" name
  | MissingType name ->
    Printf.sprintf "Missing type: %s" name
  | MissingModule name ->
    Printf.sprintf "Missing module: %s" name
  | TypeMismatch (name, impl_ty, spec_ty) ->
    Printf.sprintf "Type mismatch for %s: implementation has %s, but signature expects %s"
      name (type_expression_to_string impl_ty) (type_expression_to_string spec_ty)
  | ModuleTypeMismatch (msg, _) ->
    msg
  | WeakTypeEscape (name, ty) ->
    let prefix = if name = "" then "" else Printf.sprintf "for %s: " name in
    Printf.sprintf "Weak type escape %s%s cannot satisfy polymorphic signature"
      prefix (type_expression_to_string ty)

(** {1 Generic Signature Matching}

    Parameterized signature matching that supports both first-error and all-errors semantics.
    This eliminates code duplication between match_signature/match_signature_all. *)

(** Error accumulation strategy for signature matching.

    This allows a single implementation to handle both:
    - First-error semantics (stop on first error, return Result)
    - All-errors semantics (collect all errors, return list) *)
type 'a error_accumulator = {
  empty : 'a;                           (** Initial value (no errors) *)
  singleton : match_error -> 'a;        (** Create accumulator from single error *)
  combine : 'a -> 'a -> 'a;            (** Combine two accumulators *)
  should_continue : 'a -> bool;        (** Whether to continue checking after error *)
}

(** Accumulator that stops on first error, returns Result. *)
let first_error_accumulator : match_result error_accumulator = {
  empty = Ok ();
  singleton = (fun error -> Error error);
  combine = (fun acc1 acc2 ->
    match acc1 with
    | Error _ -> acc1
    | Ok () -> acc2);
  should_continue = Result.is_ok;
}

(** Accumulator that collects all errors, returns list. *)
let all_errors_accumulator : match_errors error_accumulator = {
  empty = [];
  singleton = (fun error -> [error]);
  combine = (fun acc1 acc2 -> acc1 @ acc2);
  should_continue = (fun _ -> true);
}

(** Generic signature matching with parameterized error accumulation.

    @param acc The error accumulation strategy
    @param ctx Match context
    @param loc Source location
    @param impl_sig Implementation signature
    @param spec_sig Specification signature
    @return Accumulated errors according to the strategy *)
let rec match_signature_generic
    : type a. a error_accumulator -> match_context -> Common.Location.t ->
      Module_types.signature -> Module_types.signature -> a =
  fun acc ctx loc impl_sig spec_sig ->
  let type_subst = build_type_substitution impl_sig in
  let check_item (spec_item : Module_types.signature_item) =
    match spec_item with
    | Module_types.SigValue (name, spec_val) ->
      begin match Module_types.find_value_in_sig name impl_sig with
      | Some impl_val ->
        (* Substitute PathLocal types in spec with impl's manifest types *)
        let spec_body_substituted = substitute_local_types type_subst spec_val.Module_types.value_type.Types.body in
        let spec_val_substituted = { spec_val with
          Module_types.value_type = { spec_val.Module_types.value_type with Types.body = spec_body_substituted }
        } in
        begin match check_type_compatibility ctx loc impl_val.value_type spec_val_substituted.value_type with
        | Ok () -> acc.empty
        | Error (TypeMismatch (_, impl_ty, spec_ty)) ->
          acc.singleton (TypeMismatch (name, impl_ty, spec_ty))
        | Error error -> acc.singleton error
        end
      | None ->
        acc.singleton (MissingValue name)
      end

    | Module_types.SigType (name, spec_decl) ->
      begin match Module_types.find_type_in_sig name impl_sig with
      | Some impl_decl ->
        begin match check_type_declaration_compatibility ctx name impl_decl spec_decl with
        | Ok () -> acc.empty
        | Error error -> acc.singleton error
        end
      | None -> acc.singleton (MissingType name)
      end

    | Module_types.SigModule (name, spec_mty) ->
      begin match Module_types.find_module_in_sig name impl_sig with
      | Some impl_mty ->
        match_module_type_generic acc ctx loc impl_mty spec_mty
      | None ->
        acc.singleton (MissingModule name)
      end

    | Module_types.SigModuleType (_name, _spec_mty_opt) ->
      (* Module type declarations - skip for now *)
      acc.empty

    | Module_types.SigExtensionConstructor _ctor ->
      (* Extension constructors are checked when they are used, not during
         signature matching. The type extension must have been valid when
         the module was type-checked. *)
      acc.empty
  in
  (* Check all specification items *)
  List.fold_left (fun errors item ->
    if acc.should_continue errors then
      acc.combine errors (check_item item)
    else
      errors
  ) acc.empty spec_sig

(** Generic module type matching with parameterized error accumulation. *)
and match_module_type_generic
    : type a. a error_accumulator -> match_context -> Common.Location.t ->
      Module_types.module_type -> Module_types.module_type -> a =
  fun acc ctx loc impl_mty spec_mty ->
  let impl_mty = expand_module_type ctx impl_mty in
  let spec_mty = expand_module_type ctx spec_mty in
  match impl_mty, spec_mty with
  | Module_types.ModTypeSig impl_sig, Module_types.ModTypeSig spec_sig ->
    match_signature_generic acc ctx loc impl_sig spec_sig

  | Module_types.ModTypeFunctor (param_impl, result_impl),
    Module_types.ModTypeFunctor (param_spec, result_spec) ->
    (* Parameters: contravariant, Results: covariant *)
    let param_result = match param_impl, param_spec with
      | Module_types.FunctorParamNamed impl_p, Module_types.FunctorParamNamed spec_p ->
          (* Named functors: check parameter types contravariantly *)
          match_module_type_generic acc ctx loc spec_p.parameter_type impl_p.parameter_type
      | Module_types.FunctorParamUnit, Module_types.FunctorParamUnit ->
          (* Generative functors: both unit, compatible *)
          acc.empty
      | Module_types.FunctorParamNamed _, Module_types.FunctorParamUnit
      | Module_types.FunctorParamUnit, Module_types.FunctorParamNamed _ ->
          (* Mismatch: one is generative, one is applicative *)
          acc.singleton (ModuleTypeMismatch ("functor parameter mismatch: generative vs applicative", ""))
    in
    if acc.should_continue param_result then
      acc.combine param_result (match_module_type_generic acc ctx loc result_impl result_spec)
    else
      param_result

  | Module_types.ModTypeIdent path1, Module_types.ModTypeIdent path2 ->
    if Types.path_equal path1 path2 then acc.empty
    else acc.singleton (ModuleTypeMismatch (
      Printf.sprintf "module type %s is not compatible with %s"
        (Types.path_to_string path1) (Types.path_to_string path2), ""))

  | Module_types.ModTypeIdent path, _ ->
    acc.singleton (ModuleTypeMismatch (
      Printf.sprintf "could not expand module type %s" (Types.path_to_string path), ""))

  | _, Module_types.ModTypeIdent path ->
    acc.singleton (ModuleTypeMismatch (
      Printf.sprintf "could not expand module type %s" (Types.path_to_string path), ""))

  | _ ->
    acc.singleton (ModuleTypeMismatch ("module type mismatch", ""))

(** {1 Public Matching Functions} *)

(** Check if an implementation signature satisfies a specification signature.
    Stops on first error. *)
let match_signature ctx loc impl_sig spec_sig : match_result =
  match_signature_generic first_error_accumulator ctx loc impl_sig spec_sig

(** Check if an implementation module type matches a specification module type.
    Stops on first error. *)
let match_module_type ctx loc impl_mty spec_mty : match_result =
  match_module_type_generic first_error_accumulator ctx loc impl_mty spec_mty

(** Check signature, collecting all errors. Useful for LSP diagnostics. *)
let match_signature_all ctx loc impl_sig spec_sig : match_errors =
  match_signature_generic all_errors_accumulator ctx loc impl_sig spec_sig

(** Check module type, collecting all errors. Useful for LSP diagnostics. *)
let match_module_type_all ctx loc impl_mty spec_mty : match_errors =
  match_module_type_generic all_errors_accumulator ctx loc impl_mty spec_mty

(** Convert accumulated errors to a single match_result for backward compatibility *)
let errors_to_result errors =
  match errors with
  | [] -> Ok ()
  | first_error :: _ -> Error first_error
