(** Internal representation of module types for the type checker.

    This module defines the semantic representation of modules and module types,
    as opposed to the syntactic representation in Syntax_tree.
*)

open Common

(** Module identity for applicative functor semantics.
    Each module binding gets a unique stamp. *)
type module_ident = {
  ident_name : string;
  ident_stamp : int;
}

let next_module_stamp = ref 0

let fresh_module_ident name =
  let stamp = !next_module_stamp in
  incr next_module_stamp;
  { ident_name = name; ident_stamp = stamp }

let reset_module_stamps () =
  next_module_stamp := 0

(** Paths to modules, used for tracking type identity across module boundaries. *)
type path =
  | PathIdent of module_ident              (** Simple module: M *)
  | PathDot of path * string               (** Module member: M.N *)
  | PathApply of path * path               (** Functor application: F(A) *)

let rec path_to_string = function
  | PathIdent id -> id.ident_name
  | PathDot (p, name) -> path_to_string p ^ "." ^ name
  | PathApply (p1, p2) -> path_to_string p1 ^ "(" ^ path_to_string p2 ^ ")"

(** Value description in a signature *)
type value_description = {
  val_type : Types.type_scheme;
  val_location : Location.t;
}

(** Functor parameter *)
type functor_parameter = {
  param_name : string;
  param_id : module_ident;
  param_type : module_type;
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
  mod_id : module_ident;
  mod_type : module_type;
  mod_alias : path option;  (** If this is an alias, the original path *)
}

(** Pretty printing *)

let rec pp_module_type fmt = function
  | ModTypeSig items ->
    Format.fprintf fmt "sig@[<v 2>";
    List.iter (fun item -> Format.fprintf fmt "@,%a" pp_signature_item item) items;
    Format.fprintf fmt "@]@,end"
  | ModTypeFunctor (param, result) ->
    Format.fprintf fmt "functor (%s : %a) -> %a"
      param.param_name pp_module_type param.param_type pp_module_type result
  | ModTypeIdent path ->
    Format.fprintf fmt "%s" (path_to_string path)

and pp_signature_item fmt = function
  | SigValue (name, desc) ->
    Format.fprintf fmt "val %s : %a" name Types.pp_type_scheme desc.val_type
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

(** Lookup helpers for signatures *)

let find_value_in_sig name sig_ =
  List.find_map (function
    | SigValue (n, desc) when n = name -> Some desc
    | _ -> None
  ) sig_

let find_type_in_sig name sig_ =
  List.find_map (function
    | SigType (n, decl) when n = name -> Some decl
    | _ -> None
  ) sig_

let find_module_in_sig name sig_ =
  List.find_map (function
    | SigModule (n, mty) when n = name -> Some mty
    | _ -> None
  ) sig_

let find_module_type_in_sig name sig_ =
  List.find_map (function
    | SigModuleType (n, mty) when n = name -> Some mty
    | _ -> None
  ) sig_
