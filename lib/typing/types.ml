type level = int

let generic_level = 100_000_000
let current_level_ref = ref 1

let current_level () = !current_level_ref
let enter_level () = incr current_level_ref
let leave_level () = decr current_level_ref
let reset_level () = current_level_ref := 1
let set_level level = current_level_ref := level

let next_type_variable_id = ref 0
let set_next_type_variable_id id = next_type_variable_id := id

let fresh_type_variable_id () =
  let id = !next_type_variable_id in
  incr next_type_variable_id;
  id

type type_variable = {
  id : int;
  mutable level : level;
  mutable link : type_expression option;
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

let new_type_variable_at_level level =
  TypeVariable { id = fresh_type_variable_id (); level; link = None }

let new_type_variable () =
  new_type_variable_at_level (current_level ())

let type_int = TypeConstructor (PathBuiltin BuiltinInt, [])
let type_float = TypeConstructor (PathBuiltin BuiltinFloat, [])
let type_string = TypeConstructor (PathBuiltin BuiltinString, [])
let type_bool = TypeConstructor (PathBuiltin BuiltinBool, [])
let type_unit = TypeConstructor (PathBuiltin BuiltinUnit, [])

(* Create a closed record type *)
let type_record_closed fields =
  TypeRecord {
    row_fields = List.sort compare fields;
    row_more = TypeRowEmpty;
  }

(* Create an open record type with a fresh row variable *)
let type_record_open fields =
  TypeRecord {
    row_fields = List.sort compare fields;
    row_more = new_type_variable ();
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

let generalize ty =
  let current = current_level () in
  let generalized_vars = ref [] in
  let rec collect ty =
    match representative ty with
    | TypeVariable tv when tv.level > current ->
      if not (List.exists (fun v -> v.id = tv.id) !generalized_vars) then begin
        tv.level <- generic_level;
        generalized_vars := tv :: !generalized_vars
      end
    | TypeVariable _ -> ()
    | TypeConstructor (_, args) -> List.iter collect args
    | TypeTuple elements -> List.iter collect elements
    | TypeArrow (arg, result) ->
      collect arg;
      collect result
    | TypeRecord row -> collect_row row
    | TypeRowEmpty -> ()
  and collect_row row =
    List.iter (fun (_, field) ->
      match field with
      | RowFieldPresent ty -> collect ty
    ) row.row_fields;
    collect row.row_more
  in
  collect ty;
  { quantified_variables = !generalized_vars; body = ty }

let instantiate scheme =
  if scheme.quantified_variables = [] then scheme.body
  else begin
    let substitution =
      List.map (fun tv ->
        (tv.id, new_type_variable ())
      ) scheme.quantified_variables
    in
    let rec copy ty =
      match representative ty with
      | TypeVariable tv ->
        begin match List.assoc_opt tv.id substitution with
        | Some fresh_ty -> fresh_ty
        | None -> ty
        end
      | TypeConstructor (path, args) ->
        TypeConstructor (path, List.map copy args)
      | TypeTuple elements ->
        TypeTuple (List.map copy elements)
      | TypeArrow (arg, result) ->
        TypeArrow (copy arg, copy result)
      | TypeRecord row ->
        TypeRecord (copy_row row)
      | TypeRowEmpty ->
        TypeRowEmpty
    and copy_row row = {
      row_fields = List.map (fun (name, field) ->
        (name, match field with
          | RowFieldPresent ty -> RowFieldPresent (copy ty))
      ) row.row_fields;
      row_more = copy row.row_more;
    }
    in
    copy scheme.body
  end

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
