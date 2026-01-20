open Common

(* Simplified patterns for exhaustiveness analysis.
   Strips away type annotations and location info, keeping only structure. *)

type simple_pattern =
  | PatWildcard
  | PatConstructor of string * simple_pattern option  (* ctor name, optional arg *)
  | PatTuple of simple_pattern list
  | PatConstant of constant_value
  | PatRecord of (string * simple_pattern) list * bool  (* fields, is_open *)
  | PatOr of simple_pattern * simple_pattern  (* or-pattern: p1 | p2 *)

and constant_value =
  | ConstInt of int
  | ConstFloat of float
  | ConstString of string
  | ConstBool of bool

type pattern_vector = simple_pattern list
type pattern_matrix = pattern_vector list

(* Witness for counter-example generation *)
type witness =
  | WitWildcard
  | WitConstructor of string * witness option
  | WitTuple of witness list
  | WitRecord of (string * witness) list
  | WitConstant of constant_value

(* Constructor signature - all constructors of a type *)
type constructor_signature =
  | SigVariant of (string * Types.type_expression option) list  (* (name, arg_type) pairs *)
  | SigBool
  | SigTuple of int * Types.type_expression list  (* arity, element types *)
  | SigInfinite  (* int, float, string *)

(* Convert typed pattern to simple pattern *)
let rec simplify_pattern (pat : Typed_tree.typed_pattern) : simple_pattern =
  match pat.pattern_desc with
  | Typed_tree.TypedPatternVariable _ -> PatWildcard
  | Typed_tree.TypedPatternWildcard -> PatWildcard
  | Typed_tree.TypedPatternConstant c ->
    PatConstant (simplify_constant c)
  | Typed_tree.TypedPatternTuple pats ->
    PatTuple (List.map simplify_pattern pats)
  | Typed_tree.TypedPatternConstructor (ctor_info, arg) ->
    PatConstructor (ctor_info.Types.constructor_name, Option.map simplify_pattern arg)
  | Typed_tree.TypedPatternRecord (fields, is_open) ->
    let simplified_fields = List.map (fun (f : Typed_tree.typed_record_pattern_field) ->
      (f.typed_pattern_field_name, simplify_pattern f.typed_pattern_field_pattern)
    ) fields in
    PatRecord (simplified_fields, is_open)
  | Typed_tree.TypedPatternAlias (inner, _id) ->
    (* Alias patterns: the inner pattern determines matching behavior *)
    simplify_pattern inner
  | Typed_tree.TypedPatternOr (left, right) ->
    (* Or-patterns: preserve structure for exhaustiveness *)
    PatOr (simplify_pattern left, simplify_pattern right)
  | Typed_tree.TypedPatternLocallyAbstract _ ->
    (* Locally abstract types are wildcards for exhaustiveness checking *)
    PatWildcard
  | Typed_tree.TypedPatternPolyVariant (tag, arg) ->
    (* Polymorphic variants are treated as constructors with backtick-prefixed names *)
    PatConstructor ("`" ^ tag, Option.map simplify_pattern arg)
  | Typed_tree.TypedPatternError _ ->
    (* Error patterns are treated as wildcards for exhaustiveness checking *)
    PatWildcard

and simplify_constant = function
  | Parsing.Syntax_tree.ConstantInteger n -> ConstInt n
  | Parsing.Syntax_tree.ConstantFloat f -> ConstFloat f
  | Parsing.Syntax_tree.ConstantString s -> ConstString s
  | Parsing.Syntax_tree.ConstantBoolean b -> ConstBool b
  | Parsing.Syntax_tree.ConstantUnit -> ConstBool true  (* unit has one value *)

(* Pretty-print a witness for error messages *)
let rec witness_to_string = function
  | WitWildcard -> "_"
  | WitConstructor (name, None) -> name
  | WitConstructor (name, Some arg) ->
    Printf.sprintf "%s %s" name (witness_to_string_parens arg)
  | WitTuple wits ->
    Printf.sprintf "(%s)" (String.concat ", " (List.map witness_to_string wits))
  | WitRecord fields ->
    let field_strs = List.map (fun (name, wit) ->
      Printf.sprintf "%s = %s" name (witness_to_string wit)
    ) fields in
    Printf.sprintf "{ %s }" (String.concat "; " field_strs)
  | WitConstant c -> constant_to_string c

and witness_to_string_parens wit =
  match wit with
  | WitTuple _ | WitConstructor (_, Some _) | WitRecord _ ->
    Printf.sprintf "(%s)" (witness_to_string wit)
  | _ -> witness_to_string wit

and constant_to_string = function
  | ConstInt n -> string_of_int n
  | ConstFloat f -> string_of_float f
  | ConstString s -> Printf.sprintf "\"%s\"" s
  | ConstBool b -> string_of_bool b

(** Check if a GADT constructor can produce the scrutinee type.
    For non-GADTs, always returns true. For GADTs, checks if the
    constructor's result type can unify with the scrutinee type.
    Uses fresh type copies to avoid polluting the original types. *)
let is_constructor_reachable env scrutinee_type (ctor : Types.constructor_info) =
  if not ctor.constructor_is_gadt then
    true  (* Non-GADT constructors are always reachable *)
  else
    let fresh_var () = Types.new_type_variable_at_level Types.generic_level in
    let _, instantiated_result = Type_utils.instantiate_constructor ~fresh_var ctor in
    let scrutinee_copy = Type_scheme.instantiate_all_fresh ~fresh_var scrutinee_type in

    try
      let type_lookup path = Environment.find_type_by_path path env in
      Unification.unify ~type_lookup Location.none
        scrutinee_copy instantiated_result;
      true
    with Unification.Unification_error _ ->
      false

(** Instantiate a constructor's argument type for pattern checking.
    For non-GADTs, substitutes type parameters directly from the scrutinee.
    For GADTs, uses unification to determine the proper instantiation. *)
let instantiate_ctor_arg_type env scrutinee_type (ctor : Types.constructor_info) =
  if not ctor.constructor_is_gadt then
    let type_args = match Types.representative scrutinee_type with
      | Types.TypeConstructor (_, args) -> args
      | _ -> []
    in
    match ctor.constructor_argument_type with
    | None -> None
    | Some arg_ty ->
      if List.length ctor.constructor_type_parameters = List.length type_args then
        Some (Type_utils.substitute_type_params
                ctor.constructor_type_parameters type_args arg_ty)
      else
        ctor.constructor_argument_type
  else
    let fresh_var () = Types.new_type_variable_at_level Types.generic_level in
    let instantiated_arg, instantiated_result =
      Type_utils.instantiate_constructor ~fresh_var ctor
    in
    let scrutinee_copy = Type_scheme.instantiate_all_fresh ~fresh_var scrutinee_type in
    let type_lookup path = Environment.find_type_by_path path env in
    (try
       Unification.unify ~type_lookup Location.none scrutinee_copy instantiated_result;
       instantiated_arg
     with Unification.Unification_error _ ->
       ctor.constructor_argument_type)

(** Build a variant signature from constructors, filtering out unreachable GADT cases.
    Instantiates constructor argument types with the actual type arguments from the
    scrutinee type. This is crucial for nested patterns like [Some (a, b)] matching
    against [(int * int) option] - we need the instantiated type [(int * int)] not
    the generic type parameter ['a]. *)
let make_variant_signature env scrutinee_type ctors =
  let reachable = List.filter (is_constructor_reachable env scrutinee_type) ctors in

  let ctor_pairs = List.map (fun (ctor : Types.constructor_info) ->
    let instantiated_arg_type = instantiate_ctor_arg_type env scrutinee_type ctor in
    (ctor.constructor_name, instantiated_arg_type)
  ) reachable in

  SigVariant ctor_pairs

(* Get constructor signature from environment.
   For GADT types, filters out constructors that cannot produce the scrutinee type. *)
let get_signature env (scrutinee_type : Types.type_expression) : constructor_signature =
  match Types.representative scrutinee_type with
  | Types.TypeConstructor (Types.PathBuiltin Types.BuiltinBool, _) ->
    SigBool
  | Types.TypeConstructor (Types.PathBuiltin _, _) ->
    SigInfinite  (* int, float, string, unit *)
  | Types.TypeConstructor (Types.PathLocal name, _) ->
    begin match Environment.find_type_constructors name env with
    | Some ctors -> make_variant_signature env scrutinee_type ctors
    | None -> SigInfinite
    end
  | Types.TypeConstructor (Types.PathIdent _, _) ->
    (* Handle module-qualified type paths *)
    begin match Environment.find_type_by_path (match Types.representative scrutinee_type with
      | Types.TypeConstructor (path, _) -> path
      | _ -> Types.PathLocal "") env
    with
    | Some decl ->
      begin match decl.Types.declaration_kind with
      | Types.DeclarationVariant ctors -> make_variant_signature env scrutinee_type ctors
      | _ -> SigInfinite
      end
    | None -> SigInfinite
    end
  | Types.TypeTuple elements -> SigTuple (List.length elements, elements)
  | _ -> SigInfinite

let rec is_wildcard = function
  | PatWildcard -> true
  | PatOr (left, right) -> is_wildcard left && is_wildcard right
  | _ -> false

(* Extract head constructors from a pattern (for or-patterns, recursively extract from both sides) *)
let rec extract_heads (pat : simple_pattern) : simple_pattern list =
  match pat with
  | PatOr (left, right) ->
    extract_heads left @ extract_heads right
  | PatWildcard -> []
  | other -> [other]

(* Extract head constructors from first column of matrix *)
let head_constructors (matrix : pattern_matrix) : simple_pattern list =
  let heads = List.concat_map (fun row ->
    match row with
    | [] -> []
    | first :: _ -> extract_heads first
  ) matrix in
  let rec unique acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if List.mem x acc then unique acc xs
      else unique (x :: acc) xs
  in
  unique [] heads

let is_complete_signature (heads : simple_pattern list) (sig_ : constructor_signature) : bool =
  match sig_ with
  | SigInfinite -> false
  | SigBool ->
    let has_true = List.exists (function PatConstant (ConstBool true) -> true | _ -> false) heads in
    let has_false = List.exists (function PatConstant (ConstBool false) -> true | _ -> false) heads in
    has_true && has_false
  | SigTuple _ ->
    (* Tuple has one constructor, complete if any tuple pattern present *)
    List.exists (function PatTuple _ -> true | _ -> false) heads
  | SigVariant ctors ->
    List.for_all (fun (constructor_name, _arg_type) ->
      List.exists (function
        | PatConstructor (name, _) -> name = constructor_name
        | _ -> false
      ) heads
    ) ctors

(* Get arity of a head constructor *)
let rec head_arity (head : simple_pattern) (sig_ : constructor_signature) : int =
  match head, sig_ with
  | PatTuple pats, _ -> List.length pats
  | PatConstructor (name, _), SigVariant ctors ->
    if List.exists (fun (ctor_name, arg_type) -> ctor_name = name && Option.is_some arg_type) ctors then 1
    else 0
  | PatConstructor (_, Some _), _ -> 1
  | PatConstructor (_, None), _ -> 0
  | PatConstant (ConstBool _), SigBool -> 0
  | PatConstant _, _ -> 0
  | PatRecord (fields, _), _ -> List.length fields
  | PatOr (left, _), _ -> head_arity left sig_  (* Use left branch's arity *)
  | PatWildcard, _ -> 0

(* Expand a pattern to sub-patterns when specializing.
   Returns a list of possible expansions (normally one, but or-patterns can produce multiple). *)
let rec expand_pattern (pat : simple_pattern) (head : simple_pattern) (arity : int) : simple_pattern list option =
  match pat, head with
  | PatWildcard, _ ->
    Some (List.init arity (fun _ -> PatWildcard))
  | PatOr (left, right), _ ->
    (* Or-pattern: try expanding left side first, then right side *)
    begin match expand_pattern left head arity with
    | Some _ as result -> result
    | None -> expand_pattern right head arity
    end
  | PatTuple pats, PatTuple _ when List.length pats = arity ->
    Some pats
  | PatConstructor (name1, arg), PatConstructor (name2, _) when name1 = name2 ->
    begin match arg with
    | Some p -> Some [p]
    | None -> Some []
    end
  | PatConstructor _, PatConstructor _ ->
    None  (* Different constructors *)
  | PatConstant c1, PatConstant c2 when c1 = c2 ->
    Some []
  | PatConstant _, PatConstant _ ->
    None  (* Different constants *)
  | PatRecord (fields1, _), PatRecord (fields2, _) ->
    (* Match fields by name *)
    let sub_pats = List.map (fun (name, _) ->
      match List.assoc_opt name fields1 with
      | Some p -> p
      | None -> PatWildcard
    ) fields2 in
    Some sub_pats
  | _ -> None

(* Expand or-patterns in first position into multiple rows *)
let rec _expand_or_pattern_rows (row : pattern_vector) : pattern_vector list =
  match row with
  | [] -> [[]]
  | PatOr (left, right) :: rest ->
    (* Expand or-pattern into two rows *)
    let left_rows = _expand_or_pattern_rows (left :: rest) in
    let right_rows = _expand_or_pattern_rows (right :: rest) in
    left_rows @ right_rows
  | first :: rest ->
    (* Non-or patterns: keep as is *)
    List.map (fun expanded_rest -> first :: expanded_rest) (_expand_or_pattern_rows rest)

(* Specialize matrix for a head constructor.
   Or-patterns are expanded into multiple rows during specialization. *)
let specialize (matrix : pattern_matrix) (head : simple_pattern) (arity : int) : pattern_matrix =
  (* First expand any or-patterns in first column *)
  let expanded_matrix = List.concat_map (fun row ->
    match row with
    | [] -> [[]]
    | first :: rest ->
      List.map (fun expanded_first -> expanded_first :: rest)
        (let rec expand_or = function
          | PatOr (left, right) -> expand_or left @ expand_or right
          | p -> [p]
        in expand_or first)
  ) matrix in
  (* Then apply standard specialization *)
  List.filter_map (fun row ->
    match row with
    | [] -> None
    | first :: rest ->
      match expand_pattern first head arity with
      | Some sub_pats -> Some (sub_pats @ rest)
      | None -> None
  ) expanded_matrix

let rec or_contains_wildcard = function
  | PatWildcard -> true
  | PatOr (left, right) -> or_contains_wildcard left || or_contains_wildcard right
  | _ -> false

(* Default matrix - keep rows where first column is wildcard or contains a wildcard branch *)
let default_matrix (matrix : pattern_matrix) : pattern_matrix =
  List.filter_map (fun row ->
    match row with
    | [] -> None
    | first :: rest ->
      if or_contains_wildcard first then Some rest
      else None
  ) matrix

(* Get all constructors from a signature with their argument types *)
let all_constructors (sig_ : constructor_signature) : (simple_pattern * int * Types.type_expression option) list =
  match sig_ with
  | SigBool ->
    [(PatConstant (ConstBool true), 0, None); (PatConstant (ConstBool false), 0, None)]
  | SigTuple (arity, _element_types) ->
    [(PatTuple (List.init arity (fun _ -> PatWildcard)), arity, None)]
  | SigVariant ctors ->
    List.map (fun (name, arg_type) ->
      let arg = if Option.is_some arg_type then Some PatWildcard else None in
      (PatConstructor (name, arg), (if Option.is_some arg_type then 1 else 0), arg_type)
    ) ctors
  | SigInfinite ->
    []

(* Find a missing constructor not covered by heads, returning (pattern, arity, arg_type option) *)
let find_missing_constructor (heads : simple_pattern list) (sig_ : constructor_signature) : (simple_pattern * int * Types.type_expression option) option =
  let all = all_constructors sig_ in
  List.find_opt (fun (ctor, _arity, _arg_type) ->
    not (List.exists (fun head ->
      match ctor, head with
      | PatConstant c1, PatConstant c2 -> c1 = c2
      | PatConstructor (n1, _), PatConstructor (n2, _) -> n1 = n2
      | PatTuple _, PatTuple _ -> true
      | _ -> false
    ) heads)
  ) all

(* Core usefulness algorithm with witness generation *)
let rec is_useful_with_witness env (types : Types.type_expression list) (matrix : pattern_matrix) (vector : pattern_vector) : witness option =
  match vector, types with
  | [], _ ->
    (* Empty vector: useful iff matrix has no rows *)
    if matrix = [] then Some WitWildcard else None

  | first_pat :: rest_vector, first_ty :: rest_types ->
    let sig_ = get_signature env first_ty in
    let heads = head_constructors matrix in

    if is_complete_signature heads sig_ then
      (* Complete signature: try each constructor *)
      try_all_constructors env first_ty rest_types sig_ matrix vector
    else
      (* Incomplete: check default matrix first *)
      begin match is_useful_with_witness env rest_types (default_matrix matrix) rest_vector with
      | Some sub_witness ->
        (* Found! Build witness with missing constructor *)
        begin match find_missing_constructor heads sig_ with
        | Some (missing, arity, _arg_type) ->
          let wit = make_witness missing arity sub_witness in
          Some wit
        | None ->
          (* No specific missing constructor (infinite type), use wildcard *)
          Some (combine_witness WitWildcard sub_witness)
        end
      | None ->
        (* Also need to check existing constructors with wildcards *)
        if is_wildcard first_pat then
          try_existing_constructors env first_ty rest_types heads matrix vector
        else
          (* Non-wildcard pattern in incomplete signature *)
          let head = first_pat in
          let arity = head_arity head sig_ in
          let arg_type = get_constructor_arg_type head sig_ in
          let specialized = specialize matrix head arity in
          match expand_pattern first_pat head arity with
          | Some sub_pats ->
            let sub_types = make_sub_types env first_ty arg_type arity in
            begin match is_useful_with_witness env (sub_types @ rest_types) specialized (sub_pats @ rest_vector) with
            | Some sub_witness -> Some (extract_witness head arity sub_witness)
            | None -> None
            end
          | None -> None
      end

  | _, [] ->
    (* Types exhausted but patterns remain - shouldn't happen *)
    None

and try_constructor_with_witness env ty rest_types matrix vector head arity arg_type =
  let specialized = specialize matrix head arity in
  match expand_pattern (List.hd vector) head arity with
  | Some sub_pats ->
    let sub_types = make_sub_types env ty arg_type arity in
    begin match is_useful_with_witness env (sub_types @ rest_types) specialized (sub_pats @ List.tl vector) with
    | Some sub_witness -> Some (extract_witness head arity sub_witness)
    | None -> None
    end
  | None -> None

and try_all_constructors env ty rest_types sig_ matrix vector =
  let all = all_constructors sig_ in
  List.find_map (fun (ctor, arity, arg_type) ->
    try_constructor_with_witness env ty rest_types matrix vector ctor arity arg_type
  ) all

and get_constructor_arg_type (head : simple_pattern) (sig_ : constructor_signature) : Types.type_expression option =
  match head with
  | PatConstructor (name, _) ->
    begin match sig_ with
    | SigVariant ctors ->
      begin match List.find_opt (fun (ctor_name, _) -> ctor_name = name) ctors with
      | Some (_, arg_type) -> arg_type
      | None -> None
      end
    | _ -> None
    end
  | _ -> None

and try_existing_constructors env ty rest_types heads matrix vector =
  List.find_map (fun head ->
    let sig_ = get_signature env ty in
    let arity = head_arity head sig_ in
    let arg_type = get_constructor_arg_type head sig_ in
    try_constructor_with_witness env ty rest_types matrix vector head arity arg_type
  ) heads

and make_sub_types env scrutinee_type arg_type_opt arity =
  match arg_type_opt with
  | Some arg_ty ->
    (* Constructor has argument - use its type for the single sub-pattern *)
    [arg_ty]
  | None ->
    (* No explicit argument type - check if scrutinee is a tuple *)
    match get_signature env scrutinee_type with
    | SigTuple (_n, element_types) -> element_types
    | _ ->
      (* Fallback: replicate scrutinee type (works for non-variant cases) *)
      List.init arity (fun _ -> scrutinee_type)

and make_witness (ctor : simple_pattern) (arity : int) (sub_witness : witness) : witness =
  match ctor with
  | PatConstructor (name, _) ->
    if arity > 0 then WitConstructor (name, Some sub_witness)
    else WitConstructor (name, None)
  | PatConstant c -> WitConstant c
  | PatTuple _ -> sub_witness  (* Tuple witness is built differently *)
  | PatRecord (fields, _) ->
    (* Build record witness with field names and wildcards *)
    let field_names = List.map fst fields in
    let field_wits = List.map (fun name -> (name, WitWildcard)) field_names in
    WitRecord field_wits
  | _ -> sub_witness

and extract_witness (head : simple_pattern) (arity : int) (sub_witness : witness) : witness =
  match head with
  | PatConstructor (name, _) ->
    if arity > 0 then
      (* Extract first sub-witness for constructor arg *)
      match sub_witness with
      | WitTuple (first :: _) -> WitConstructor (name, Some first)
      | _ -> WitConstructor (name, Some sub_witness)
    else
      WitConstructor (name, None)
  | PatTuple pats ->
    let n = List.length pats in
    (* Build tuple witness from sub-witness *)
    let wits = match sub_witness with
      | WitTuple wits when List.length wits >= n -> List.filteri (fun i _ -> i < n) wits
      | _ -> List.init n (fun _ -> WitWildcard)
    in
    WitTuple wits
  | PatRecord (fields, _) ->
    (* Build record witness from sub-witnesses *)
    let n = List.length fields in
    let field_names = List.map fst fields in
    let field_wits = match sub_witness with
      | WitTuple wits when List.length wits >= n ->
        List.map2 (fun name wit -> (name, wit)) field_names (List.filteri (fun i _ -> i < n) wits)
      | _ -> List.map (fun name -> (name, WitWildcard)) field_names
    in
    WitRecord field_wits
  | PatConstant c -> WitConstant c
  | _ -> sub_witness

and combine_witness (w1 : witness) (w2 : witness) : witness =
  match w1, w2 with
  | WitTuple ws1, WitTuple ws2 -> WitTuple (ws1 @ ws2)
  | WitTuple ws, w -> WitTuple (ws @ [w])
  | w, WitTuple ws -> WitTuple (w :: ws)
  | w, WitWildcard -> w
  | WitWildcard, w -> w
  | w1, _ -> w1

(* Simple usefulness check without witness *)
let is_useful env types matrix vector =
  Option.is_some (is_useful_with_witness env types matrix vector)

(* Check exhaustiveness *)
let check_exhaustiveness env (scrutinee_type : Types.type_expression) (arms : Typed_tree.typed_match_arm list) : witness option =
  (* Build matrix from arms, excluding guarded patterns for exhaustiveness *)
  let matrix = List.filter_map (fun (arm : Typed_tree.typed_match_arm) ->
    if Option.is_some arm.typed_arm_guard then
      None  (* Guarded patterns don't guarantee coverage *)
    else
      Some [simplify_pattern arm.typed_arm_pattern]
  ) arms in

  (* Check if wildcard pattern is useful (= match is non-exhaustive) *)
  let wildcard_vector = [PatWildcard] in
  is_useful_with_witness env [scrutinee_type] matrix wildcard_vector

(* Check for redundant patterns *)
let check_redundancy env (scrutinee_type : Types.type_expression) (arms : Typed_tree.typed_match_arm list) : Location.t list =
  let _, redundant = List.fold_left (fun (matrix, redundant_locs) (arm : Typed_tree.typed_match_arm) ->
    let pat = simplify_pattern arm.typed_arm_pattern in
    let is_redundant = not (is_useful env [scrutinee_type] matrix [pat]) in
    let new_matrix = matrix @ [[pat]] in
    if is_redundant then
      (new_matrix, arm.typed_arm_location :: redundant_locs)
    else
      (new_matrix, redundant_locs)
  ) ([], []) arms in
  List.rev redundant

(* Main entry point - check match and emit warnings *)
let check_match env (match_loc : Location.t) (scrutinee_type : Types.type_expression) (arms : Typed_tree.typed_match_arm list) : unit =
  (* Check exhaustiveness *)
  begin match check_exhaustiveness env scrutinee_type arms with
  | Some witness ->
    let witness_str = witness_to_string witness in
    Compiler_error.emit_warning (Compiler_error.NonExhaustiveMatch witness_str) match_loc
  | None -> ()
  end;

  (* Check redundancy *)
  let redundant_locs = check_redundancy env scrutinee_type arms in
  List.iter (fun loc ->
    Compiler_error.emit_warning Compiler_error.RedundantPattern loc
  ) redundant_locs
