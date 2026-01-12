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

(** Functor parameter *)
type functor_parameter = {
  parameter_name : string;
  parameter_id : Common.Identifier.t;  (** Runtime identifier for this parameter *)
  parameter_type : module_type;
}

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

(** Pretty printing *)

let rec pp_module_type fmt = function
  | ModTypeSig items ->
    Format.fprintf fmt "sig@[<v 2>";
    List.iter (fun item -> Format.fprintf fmt "@,%a" pp_signature_item item) items;
    Format.fprintf fmt "@]@,end"
  | ModTypeFunctor (param, result) ->
    Format.fprintf fmt "functor (%s : %a) -> %a"
      param.parameter_name pp_module_type param.parameter_type pp_module_type result
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
