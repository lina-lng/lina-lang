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

(** Argument labels for function types and applications.
    Mirrors OCaml's Asttypes.arg_label. *)
type arg_label =
  | Nolabel                 (** Regular unlabeled argument *)
  | Labelled of string      (** Labeled argument: [~label:] *)
  | Optional of string      (** Optional argument: [?label:] *)

type type_variable = {
  id : int;
  mutable level : level;
  mutable link : type_expression option;
  mutable weak : bool;  (** True if value restriction blocks generalization *)
  mutable rigid : bool;  (** True for locally abstract types - don't unify, extract equations *)
}

and type_expression =
  | TypeVariable of type_variable
  | TypeConstructor of path * type_expression list
  | TypeTuple of type_expression list
  | TypeArrow of arg_label * type_expression * type_expression  (** [label:arg -> result] *)
  | TypeRecord of row
  | TypePolyVariant of poly_variant_row  (** Polymorphic variant type *)
  | TypePackage of package_type  (** First-class module type: (module S) *)
  | TypeRowEmpty

(** Package type for first-class modules.
    Contains the module type signature that the packed module must satisfy. *)
and package_type = {
  package_path : path;  (** The module type path (for printing) *)
  package_signature : (string * type_expression) list;  (** Flattened type constraints *)
}

and row = {
  row_fields : (string * row_field) list;  (* Sorted by label *)
  row_more : type_expression;               (* Row variable or TypeRowEmpty *)
}

and row_field =
  | RowFieldPresent of type_expression

(** Polymorphic variant row type.

    Like record rows but for sum types. Contains:
    - A list of tags with their argument types (if any)
    - A row variable or closed marker
    - Openness marker for type inference bounds *)
and poly_variant_row = {
  pv_fields : (string * poly_variant_field) list;  (** Tag fields, sorted by name *)
  pv_more : type_expression;  (** Row variable or TypeRowEmpty *)
  pv_closed : bool;  (** False for [> ...], True for [< ...] or exact *)
}

(** A single polymorphic variant field. *)
and poly_variant_field =
  | PVFieldPresent of type_expression option  (** Tag is present, with optional argument *)
  | PVFieldAbsent  (** Tag is explicitly absent (for closed variants) *)

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
  | BuiltinRef    (** Mutable reference type *)
  | BuiltinArray  (** Mutable array type *)
  | BuiltinDict   (** Immutable dictionary type *)
  | BuiltinSet    (** Immutable set type *)

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
  TypeVariable { id = fresh_type_variable_id (); level; link = None; weak = false; rigid = false }

(* NOTE: new_type_variable() has been removed. All type variable creation should
   go through Typing_context.new_type_variable which manages levels explicitly. *)

let type_int = TypeConstructor (PathBuiltin BuiltinInt, [])
let type_float = TypeConstructor (PathBuiltin BuiltinFloat, [])
let type_string = TypeConstructor (PathBuiltin BuiltinString, [])
let type_bool = TypeConstructor (PathBuiltin BuiltinBool, [])
let type_unit = TypeConstructor (PathBuiltin BuiltinUnit, [])

(** [type_ref content_type] creates a reference type [content_type ref]. *)
let type_ref content_type = TypeConstructor (PathBuiltin BuiltinRef, [content_type])

(** [type_array element_type] creates an array type [element_type array]. *)
let type_array element_type = TypeConstructor (PathBuiltin BuiltinArray, [element_type])

(** [type_dict key_type value_type] creates a dictionary type [(key_type, value_type) dict]. *)
let type_dict key_type value_type = TypeConstructor (PathBuiltin BuiltinDict, [key_type; value_type])

(** [type_set element_type] creates a set type [element_type set]. *)
let type_set element_type = TypeConstructor (PathBuiltin BuiltinSet, [element_type])

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

(** [type_poly_variant_at_least fields ~row_var] creates an open poly variant type [\[> `A | `B \]].
    The variant has at least the given tags, with [row_var] as the tail for additional tags. *)
let type_poly_variant_at_least fields ~row_var =
  TypePolyVariant {
    pv_fields = List.sort compare fields;
    pv_more = row_var;
    pv_closed = false;
  }

(** [type_poly_variant_exact fields] creates a closed exact poly variant type [\[ `A | `B \]]. *)
let type_poly_variant_exact fields =
  TypePolyVariant {
    pv_fields = List.sort compare fields;
    pv_more = TypeRowEmpty;
    pv_closed = true;
  }

let rec representative ty =
  match ty with
  | TypeVariable tv ->
    begin match tv.link with
    | Some linked_ty ->
      let repr = representative linked_ty in
      (* Path compression optimization: flatten the link chain.
         However, don't compress past rigid variables - they may be temporarily
         linked for GADT type refinement and will be unlinked later.
         Compressing past them would bypass the unlink. *)
      begin match linked_ty with
      | TypeVariable linked_tv when linked_tv.rigid ->
        (* Don't compress - keep the chain through the rigid variable *)
        ()
      | _ ->
        tv.link <- Some repr
      end;
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
  constructor_is_gadt : bool;  (** True if constructor has explicit return type (GADT) *)
  constructor_existentials : type_variable list;  (** Type variables in argument but not in result *)
}

(** A type parameter constraint.

    Pairs a type variable with the type it must unify with.
    Example: constraint 'a = 'b * 'c *)
type type_constraint = {
  constraint_variable : type_variable;  (** The constrained type variable *)
  constraint_type : type_expression;  (** The type the variable must equal *)
}

type type_declaration = {
  declaration_name : string;
  declaration_parameters : type_variable list;
  declaration_variances : variance list;  (** Variance of each type parameter *)
  declaration_injectivities : bool list;  (** Injectivity of each type parameter *)
  declaration_manifest : type_expression option;  (* Type alias: type t = int *)
  declaration_kind : type_declaration_kind;
  declaration_private : bool;  (** True if private (pattern match ok, construction blocked) *)
  declaration_constraints : type_constraint list;  (** Type parameter constraints *)
}

and type_declaration_kind =
  | DeclarationAbstract
  | DeclarationVariant of constructor_info list
  | DeclarationRecord of (string * type_expression) list  (* Record types *)
  | DeclarationExtensible  (** Extensible variant: type t = .. *)

(** Convert a path to its string representation. *)
let rec path_to_string = function
  | PathBuiltin BuiltinInt -> "int"
  | PathBuiltin BuiltinFloat -> "float"
  | PathBuiltin BuiltinString -> "string"
  | PathBuiltin BuiltinBool -> "bool"
  | PathBuiltin BuiltinUnit -> "unit"
  | PathBuiltin BuiltinRef -> "ref"
  | PathBuiltin BuiltinArray -> "array"
  | PathBuiltin BuiltinDict -> "dict"
  | PathBuiltin BuiltinSet -> "set"
  | PathLocal name -> name
  | PathIdent id -> Common.Identifier.name id
  | PathDot (parent, name) -> path_to_string parent ^ "." ^ name
  | PathApply (func, arg) -> path_to_string func ^ "(" ^ path_to_string arg ^ ")"

let pp_path fmt path =
  Format.pp_print_string fmt (path_to_string path)

(** Pretty-printing context for normalized type variable names.
    Assigns sequential names ('a, 'b, ...) to type variables on first encounter. *)
module Pretty_print = struct
  type context = {
    mutable next_index : int;
    mapping : (int, int) Hashtbl.t;
  }

  let create () = { next_index = 0; mapping = Hashtbl.create 16 }

  let letter_of_int index =
    if index < 26 then
      String.make 1 (Char.chr (Char.code 'a' + index))
    else
      let base = index mod 26 in
      let suffix = index / 26 in
      String.make 1 (Char.chr (Char.code 'a' + base)) ^ string_of_int suffix

  let get_name ctx var_id =
    match Hashtbl.find_opt ctx.mapping var_id with
    | Some index -> index
    | None ->
      let index = ctx.next_index in
      ctx.next_index <- ctx.next_index + 1;
      Hashtbl.add ctx.mapping var_id index;
      index
end

(** Precedence-based type printing.

    Uses OCaml's approach with multiple precedence levels:
    - Top level: arrows without parens
    - Tuple level: tuples without parens
    - Application level: type constructors with arguments
    - Simple level: atomic types (variables, nullary constructors, etc.)

    Parentheses are only added when needed:
    - Arrow as function argument: [('a -> 'b) -> 'c]
    - Tuple in type application: [('a * 'b) list] *)

let rec pp_type_top ctx fmt ty =
  match representative ty with
  | TypeArrow (_, arg, result) ->
    Format.fprintf fmt "%a -> %a"
      (pp_type_arrow_arg ctx) arg
      (pp_type_top ctx) result
  | _ -> pp_type_tuple ctx fmt ty

and pp_type_arrow_arg ctx fmt ty =
  match representative ty with
  | TypeArrow _ -> Format.fprintf fmt "(%a)" (pp_type_top ctx) ty
  | _ -> pp_type_tuple ctx fmt ty

and pp_type_tuple ctx fmt ty =
  match representative ty with
  | TypeTuple elements ->
    Format.fprintf fmt "%a"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ")
        (pp_type_simple ctx))
      elements
  | _ -> pp_type_app ctx fmt ty

and pp_type_app ctx fmt ty =
  match representative ty with
  | TypeConstructor (path, [arg]) ->
    Format.fprintf fmt "%a %a" (pp_type_simple ctx) arg pp_path path
  | TypeConstructor (path, args) when args <> [] ->
    Format.fprintf fmt "(%a) %a"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
        (pp_type_top ctx))
      args pp_path path
  | _ -> pp_type_simple ctx fmt ty

and pp_type_simple ctx fmt ty =
  match representative ty with
  | TypeVariable tv ->
    let index = Pretty_print.get_name ctx tv.id in
    let name = Pretty_print.letter_of_int index in
    if tv.weak then Format.fprintf fmt "'_%s" name
    else Format.fprintf fmt "'%s" name
  | TypeConstructor (path, []) -> pp_path fmt path
  | TypeRecord row -> pp_row ctx fmt row
  | TypePolyVariant pv_row -> pp_poly_variant_row ctx fmt pv_row
  | TypePackage pkg -> Format.fprintf fmt "(module %a)" pp_path pkg.package_path
  | TypeRowEmpty -> Format.fprintf fmt "{}"
  | TypeArrow _ | TypeTuple _ | TypeConstructor _ ->
    Format.fprintf fmt "(%a)" (pp_type_top ctx) ty

and pp_poly_variant_row ctx fmt pv_row =
  let open_marker = if pv_row.pv_closed then "" else "> " in
  Format.fprintf fmt "[%s" open_marker;
  List.iteri (fun index (tag, field) ->
    if index > 0 then Format.fprintf fmt " | ";
    match field with
    | PVFieldPresent None -> Format.fprintf fmt "`%s" tag
    | PVFieldPresent (Some ty) -> Format.fprintf fmt "`%s of %a" tag (pp_type_top ctx) ty
    | PVFieldAbsent -> ()
  ) pv_row.pv_fields;
  begin match representative pv_row.pv_more with
  | TypeRowEmpty -> Format.fprintf fmt " ]"
  | TypeVariable _ -> Format.fprintf fmt " | .. ]"
  | _ -> Format.fprintf fmt " | .. ]"
  end

and pp_row ctx fmt row =
  Format.fprintf fmt "{ ";
  List.iteri (fun index (name, field) ->
    if index > 0 then Format.fprintf fmt "; ";
    match field with
    | RowFieldPresent ty -> Format.fprintf fmt "%s : %a" name (pp_type_top ctx) ty
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

let pp_type_expression fmt ty =
  let ctx = Pretty_print.create () in
  pp_type_top ctx fmt ty

let rec path_equal p1 p2 =
  match p1, p2 with
  | PathBuiltin b1, PathBuiltin b2 -> b1 = b2
  | PathLocal n1, PathLocal n2 -> String.equal n1 n2
  | PathIdent id1, PathIdent id2 -> Common.Identifier.equal id1 id2
  | PathDot (p1, n1), PathDot (p2, n2) -> String.equal n1 n2 && path_equal p1 p2
  | PathApply (f1, a1), PathApply (f2, a2) -> path_equal f1 f2 && path_equal a1 a2
  | _ -> false

(* For backward compatibility *)
let pp_type_path = pp_path

let pp_type_scheme fmt scheme =
  let ctx = Pretty_print.create () in
  if scheme.quantified_variables = [] then
    pp_type_top ctx fmt scheme.body
  else begin
    Format.fprintf fmt "forall";
    List.iter (fun tv ->
      let index = Pretty_print.get_name ctx tv.id in
      Format.fprintf fmt " '%s" (Pretty_print.letter_of_int index)
    ) scheme.quantified_variables;
    Format.fprintf fmt ". %a" (pp_type_top ctx) scheme.body
  end

let type_expression_to_string ty =
  Format.asprintf "%a" pp_type_expression ty
