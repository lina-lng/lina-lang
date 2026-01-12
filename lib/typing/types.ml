type level = int

(** Level for fully generalized (polymorphic) type variables. *)
let generic_level = 100_000_000

(** Global counter for unique type variable IDs.
    This is the only global state remaining in Types - it's harmless as it
    just generates unique identifiers. All level management is done through
    Typing_context. *)
let next_type_variable_id = ref 0

let fresh_type_variable_id () =
  let id = !next_type_variable_id in
  incr next_type_variable_id;
  id

(** Reset the type variable ID counter.
    Used for testing to ensure reproducible type variable names.
    Should not be used in production code. *)
let reset_type_variable_id () =
  next_type_variable_id := 0

type type_variable = {
  id : int;
  mutable level : level;
  mutable link : type_expression option;
  mutable weak : bool;  (** True if value restriction blocks generalization *)
}

and type_expression =
  | TypeVariable of type_variable
  | TypeConstructor of path * type_expression list
  | TypeTuple of type_expression list
  | TypeArrow of type_expression * type_expression
  | TypeRecord of row
  | TypeRowEmpty

and row = {
  row_fields : (string * row_field) list;  (* Sorted by label *)
  row_more : type_expression;               (* Row variable or TypeRowEmpty *)
}

and row_field =
  | RowFieldPresent of type_expression

(* Unified path type for both types and modules *)
and path =
  | PathBuiltin of builtin_type           (* int, bool, string, etc. *)
  | PathLocal of string                   (* Local name for types: t *)
  | PathIdent of Common.Identifier.t      (* Runtime module reference: M *)
  | PathDot of path * string              (* Qualified: M.t, M.N.t *)
  | PathApply of path * path              (* Functor application: F(M).t *)

and builtin_type =
  | BuiltinInt
  | BuiltinFloat
  | BuiltinString
  | BuiltinBool
  | BuiltinUnit
  | BuiltinRef  (** Mutable reference type *)

(** Variance of a type parameter.

    Variance describes how a type constructor relates to subtyping
    (or in ML, how it affects generalization under the value restriction):
    - [Covariant]: parameter appears in output positions only
    - [Contravariant]: parameter appears in input positions only
    - [Invariant]: parameter appears in both positions
    - [Bivariant]: parameter doesn't appear (phantom type) *)
type variance =
  | Covariant
  | Contravariant
  | Invariant
  | Bivariant
[@@deriving show, eq]

let new_type_variable_at_level level =
  TypeVariable { id = fresh_type_variable_id (); level; link = None; weak = false }

(* NOTE: new_type_variable() has been removed. All type variable creation should
   go through Typing_context.new_type_variable which manages levels explicitly. *)

let type_int = TypeConstructor (PathBuiltin BuiltinInt, [])
let type_float = TypeConstructor (PathBuiltin BuiltinFloat, [])
let type_string = TypeConstructor (PathBuiltin BuiltinString, [])
let type_bool = TypeConstructor (PathBuiltin BuiltinBool, [])
let type_unit = TypeConstructor (PathBuiltin BuiltinUnit, [])

(** [type_ref content_type] creates a reference type [content_type ref]. *)
let type_ref content_type = TypeConstructor (PathBuiltin BuiltinRef, [content_type])

(** [type_record_closed fields] creates a closed record type.
    The record has exactly the given fields and no others. *)
let type_record_closed fields =
  TypeRecord {
    row_fields = List.sort compare fields;
    row_more = TypeRowEmpty;
  }

(** [type_record_open fields ~row_var] creates an open record type.
    The record has at least the given fields, with [row_var] as the tail
    to allow additional fields. *)
let type_record_open fields ~row_var =
  TypeRecord {
    row_fields = List.sort compare fields;
    row_more = row_var;
  }

let rec representative ty =
  match ty with
  | TypeVariable tv ->
    begin match tv.link with
    | Some linked_ty ->
      let repr = representative linked_ty in
      tv.link <- Some repr;
      repr
    | None -> ty
    end
  | _ -> ty

type type_scheme = {
  quantified_variables : type_variable list;
  body : type_expression;
}

let trivial_scheme ty = { quantified_variables = []; body = ty }

(* NOTE: Type scheme operations (generalize, instantiate, mark_as_weak) have been
   moved to Type_scheme module to avoid circular dependency with Type_traversal.
   Use Type_scheme.generalize, Type_scheme.instantiate, etc. instead. *)

type constructor_info = {
  constructor_name : string;
  constructor_tag_index : int;  (* 0-based index within the type *)
  constructor_type_name : string;  (* Name of the parent type, e.g., "option" *)
  constructor_argument_type : type_expression option;
  constructor_result_type : type_expression;
  constructor_type_parameters : type_variable list;
}

type type_declaration = {
  declaration_name : string;
  declaration_parameters : type_variable list;
  declaration_variances : variance list;  (** Variance of each type parameter *)
  declaration_manifest : type_expression option;  (* Type alias: type t = int *)
  declaration_kind : type_declaration_kind;
}

and type_declaration_kind =
  | DeclarationAbstract
  | DeclarationVariant of constructor_info list
  | DeclarationRecord of (string * type_expression) list  (* Record types *)

let rec pp_type_expression fmt ty =
  match representative ty with
  | TypeVariable tv ->
    if tv.weak then
      Format.fprintf fmt "'_t%d" tv.id  (* Weak variable: cannot be generalized *)
    else
      Format.fprintf fmt "'t%d" tv.id
  | TypeConstructor (path, []) ->
    pp_path fmt path
  | TypeConstructor (path, [arg]) ->
    Format.fprintf fmt "%a %a" pp_type_expression arg pp_path path
  | TypeConstructor (path, args) ->
    Format.fprintf fmt "(%a) %a"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_type_expression) args
      pp_path path
  | TypeTuple elements ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ") pp_type_expression) elements
  | TypeArrow (arg, result) ->
    Format.fprintf fmt "(%a -> %a)" pp_type_expression arg pp_type_expression result
  | TypeRecord row ->
    pp_row fmt row
  | TypeRowEmpty ->
    Format.fprintf fmt "{}"

and pp_row fmt row =
  Format.fprintf fmt "{ ";
  List.iteri (fun i (name, field) ->
    if i > 0 then Format.fprintf fmt "; ";
    match field with
    | RowFieldPresent ty -> Format.fprintf fmt "%s : %a" name pp_type_expression ty
  ) row.row_fields;
  begin match representative row.row_more with
  | TypeRowEmpty ->
    if row.row_fields <> [] then Format.fprintf fmt " }"
    else Format.fprintf fmt "}"
  | TypeVariable _ ->
    if row.row_fields <> [] then Format.fprintf fmt "; .. }"
    else Format.fprintf fmt ".. }"
  | _ ->
    if row.row_fields <> [] then Format.fprintf fmt "; .. }"
    else Format.fprintf fmt ".. }"
  end

and pp_path fmt = function
  | PathBuiltin BuiltinInt -> Format.fprintf fmt "int"
  | PathBuiltin BuiltinFloat -> Format.fprintf fmt "float"
  | PathBuiltin BuiltinString -> Format.fprintf fmt "string"
  | PathBuiltin BuiltinBool -> Format.fprintf fmt "bool"
  | PathBuiltin BuiltinUnit -> Format.fprintf fmt "unit"
  | PathBuiltin BuiltinRef -> Format.fprintf fmt "ref"
  | PathLocal name -> Format.fprintf fmt "%s" name
  | PathIdent id -> Format.fprintf fmt "%s" (Common.Identifier.name id)
  | PathDot (parent, name) ->
    Format.fprintf fmt "%a.%s" pp_path parent name
  | PathApply (func, arg) ->
    Format.fprintf fmt "%a(%a)" pp_path func pp_path arg

(* Path equality *)
let rec path_equal p1 p2 =
  match p1, p2 with
  | PathBuiltin b1, PathBuiltin b2 -> b1 = b2
  | PathLocal n1, PathLocal n2 -> String.equal n1 n2
  | PathIdent id1, PathIdent id2 -> Common.Identifier.equal id1 id2
  | PathDot (p1, n1), PathDot (p2, n2) -> String.equal n1 n2 && path_equal p1 p2
  | PathApply (f1, a1), PathApply (f2, a2) -> path_equal f1 f2 && path_equal a1 a2
  | _ -> false

(* Path to string *)
let rec path_to_string = function
  | PathBuiltin BuiltinInt -> "int"
  | PathBuiltin BuiltinFloat -> "float"
  | PathBuiltin BuiltinString -> "string"
  | PathBuiltin BuiltinBool -> "bool"
  | PathBuiltin BuiltinUnit -> "unit"
  | PathBuiltin BuiltinRef -> "ref"
  | PathLocal name -> name
  | PathIdent id -> Common.Identifier.name id
  | PathDot (parent, name) -> path_to_string parent ^ "." ^ name
  | PathApply (func, arg) -> path_to_string func ^ "(" ^ path_to_string arg ^ ")"

(* For backward compatibility *)
let pp_type_path = pp_path

let pp_type_scheme fmt scheme =
  if scheme.quantified_variables = [] then
    pp_type_expression fmt scheme.body
  else begin
    Format.fprintf fmt "forall";
    List.iter (fun tv -> Format.fprintf fmt " 't%d" tv.id) scheme.quantified_variables;
    Format.fprintf fmt ". %a" pp_type_expression scheme.body
  end

let type_expression_to_string ty =
  Format.asprintf "%a" pp_type_expression ty
