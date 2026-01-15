(** Internal representation of module types for the type checker.

    This module defines the semantic representation of modules and module types,
    as opposed to the syntactic representation in Syntax_tree.
*)

open Common

(** Use unified path type from Types *)
type path = Types.path

let path_to_string = Types.path_to_string
let path_equal = Types.path_equal

(** Value description in a signature *)
type value_description = {
  value_type : Types.type_scheme;
  value_location : Location.t;
}

(** Functor parameter: either named (applicative) or unit (generative) *)
type functor_parameter =
  | FunctorParamNamed of {
      parameter_name : string;
      parameter_id : Common.Identifier.t;  (** Runtime identifier for this parameter *)
      parameter_type : module_type;
    }
  | FunctorParamUnit  (** () - generative functor, types are fresh each application *)

(** Module types (signatures) *)
and module_type =
  | ModTypeSig of signature                (** sig ... end *)
  | ModTypeFunctor of functor_parameter * module_type  (** functor (X : S) -> MT *)
  | ModTypeIdent of path                   (** Named module type: S, M.S *)

(** Signature: a list of signature items *)
and signature = signature_item list

(** Individual items in a signature *)
and signature_item =
  | SigValue of string * value_description
      (** val x : t *)
  | SigType of string * Types.type_declaration
      (** type t = ... *)
  | SigModule of string * module_type
      (** module M : S *)
  | SigModuleType of string * module_type option
      (** module type S [= MT] *)
  | SigExtensionConstructor of Types.constructor_info
      (** Extension constructor from type t += Ctor *)

(** Module expressions (typed) *)
type module_expr =
  | ModExprStruct of signature             (** struct ... end with inferred signature *)
  | ModExprFunctor of functor_parameter * module_expr  (** functor (X : S) -> ME *)
  | ModExprApply of module_expr * module_expr  (** F(A) *)
  | ModExprIdent of path                   (** M, M.N *)
  | ModExprConstraint of module_expr * module_type  (** (ME : MT) *)

(** Module binding in the environment *)
type module_binding = {
  binding_name : string;
  binding_id : Common.Identifier.t;  (** Runtime identifier for this module *)
  binding_type : module_type;
  binding_alias : path option;  (** If this is an alias, the original path *)
}

(** {1 Module Binding Smart Constructors} *)

(** Create a module binding with explicit name.

    @param name The module name
    @param id The runtime identifier
    @param mty The module type
    @param alias Optional path for module alias *)
let make_binding ~name ~id ~mty ?alias () =
  { binding_name = name; binding_id = id; binding_type = mty; binding_alias = alias }

(** Create a module binding using the identifier's name.

    @param id The runtime identifier (name is extracted from it)
    @param mty The module type *)
let make_binding_from_id id mty =
  make_binding ~name:(Identifier.name id) ~id ~mty ()

(** Pretty printing *)

let rec pp_module_type fmt = function
  | ModTypeSig items ->
    Format.fprintf fmt "sig@[<v 2>";
    List.iter (fun item -> Format.fprintf fmt "@,%a" pp_signature_item item) items;
    Format.fprintf fmt "@]@,end"
  | ModTypeFunctor (param, result) ->
    begin match param with
    | FunctorParamNamed { parameter_name; parameter_type; _ } ->
      Format.fprintf fmt "functor (%s : %a) -> %a"
        parameter_name pp_module_type parameter_type pp_module_type result
    | FunctorParamUnit ->
      Format.fprintf fmt "functor () -> %a" pp_module_type result
    end
  | ModTypeIdent path ->
    Format.fprintf fmt "%s" (path_to_string path)

and pp_signature_item fmt = function
  | SigValue (name, desc) ->
    Format.fprintf fmt "val %s : %a" name Types.pp_type_scheme desc.value_type
  | SigType (name, _decl) ->
    Format.fprintf fmt "type %s" name
  | SigModule (name, mty) ->
    Format.fprintf fmt "module %s : %a" name pp_module_type mty
  | SigModuleType (name, None) ->
    Format.fprintf fmt "module type %s" name
  | SigModuleType (name, Some mty) ->
    Format.fprintf fmt "module type %s = %a" name pp_module_type mty
  | SigExtensionConstructor ctor ->
    Format.fprintf fmt "extension %s" ctor.Types.constructor_name

let module_type_to_string mty =
  Format.asprintf "%a" pp_module_type mty

(** {1 Signature Lookup Helpers}

    These functions search for items within a signature by name.
    Uses a generic finder internally to avoid code duplication. *)

(** Generic signature item finder *)
let find_in_sig (matcher : signature_item -> 'a option) (sig_ : signature) : 'a option =
  List.find_map matcher sig_

let find_value_in_sig name =
  find_in_sig (function
    | SigValue (n, desc) when n = name -> Some desc
    | _ -> None)

let find_type_in_sig name =
  find_in_sig (function
    | SigType (n, decl) when n = name -> Some decl
    | _ -> None)

let find_module_in_sig name =
  find_in_sig (function
    | SigModule (n, mty) when n = name -> Some mty
    | _ -> None)

let find_module_type_in_sig name =
  find_in_sig (function
    | SigModuleType (n, mty) when n = name -> Some mty
    | _ -> None)

(** Find a constructor in a signature by searching through all type declarations
    and extension constructors. Returns the constructor_info if found. *)
let find_constructor_in_sig name sig_ =
  let find_in_type_decl decl =
    match decl.Types.declaration_kind with
    | Types.DeclarationVariant constructors ->
      List.find_opt (fun ctor -> ctor.Types.constructor_name = name) constructors
    | _ -> None
  in
  List.find_map (function
    | SigType (_, decl) -> find_in_type_decl decl
    | SigExtensionConstructor ctor when ctor.Types.constructor_name = name -> Some ctor
    | _ -> None
  ) sig_
