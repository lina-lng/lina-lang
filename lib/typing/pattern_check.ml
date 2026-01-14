open Common

(* Simplified patterns for exhaustiveness analysis.
   Strips away type annotations and location info, keeping only structure. *)

type simple_pattern =
  | PatWildcard
  | PatConstructor of string * simple_pattern option  (* ctor name, optional arg *)
  | PatTuple of simple_pattern list
  | PatConstant of constant_value
  | PatRecord of (string * simple_pattern) list * bool  (* fields, is_open *)

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
  | WitConstant c -> constant_to_string c

and witness_to_string_parens wit =
  match wit with
  | WitTuple _ | WitConstructor (_, Some _) ->
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
    (* Create fresh type variables at generic level for copies *)
    let fresh_var () = Types.new_type_variable_at_level Types.generic_level in
    (* Instantiate constructor with fresh variables *)
    let _, instantiated_result =
      Type_utils.instantiate_constructor ~fresh_var ctor in
    (* Create a fresh copy of scrutinee to avoid polluting it *)
    let scrutinee_copy =
      Type_scheme.instantiate_all_fresh ~fresh_var scrutinee_type in
    (* Try unification - if it fails, constructor is unreachable *)
    try
      let type_lookup path = Environment.find_type_by_path path env in
      Unification.unify ~type_lookup Location.none
        scrutinee_copy instantiated_result;
      true
    with Unification.Unification_error _ ->
      false

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
    | Some ctors ->
      (* Filter out type-unreachable GADT constructors *)
      let reachable_ctors = List.filter
        (is_constructor_reachable env scrutinee_type) ctors in
      let ctor_info = List.map (fun (c : Types.constructor_info) ->
        (c.constructor_name, c.constructor_argument_type)
      ) reachable_ctors in
      SigVariant ctor_info
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
      | Types.DeclarationVariant ctors ->
        let reachable_ctors = List.filter
          (is_constructor_reachable env scrutinee_type) ctors in
        let ctor_info = List.map (fun (c : Types.constructor_info) ->
          (c.constructor_name, c.constructor_argument_type)
        ) reachable_ctors in
        SigVariant ctor_info
      | _ -> SigInfinite
      end
    | None -> SigInfinite
    end
  | Types.TypeTuple elements -> SigTuple (List.length elements, elements)
  | _ -> SigInfinite

(* Check if a pattern is a wildcard (matches anything) *)
let is_wildcard = function
  | PatWildcard -> true
  | _ -> false

(* Extract head constructors from first column of matrix *)
let head_constructors (matrix : pattern_matrix) : simple_pattern list =
  let heads = List.filter_map (fun row ->
    match row with
    | [] -> None
    | first :: _ ->
      if is_wildcard first then None
      else Some first
  ) matrix in
  (* Remove duplicates *)
  let rec unique acc = function
    | [] -> List.rev acc
    | x :: xs ->
      if List.mem x acc then unique acc xs
      else unique (x :: acc) xs
  in
  unique [] heads

(* Check if head constructors form a complete signature *)
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
let head_arity (head : simple_pattern) (sig_ : constructor_signature) : int =
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
  | PatWildcard, _ -> 0

(* Expand a pattern to sub-patterns when specializing *)
let expand_pattern (pat : simple_pattern) (head : simple_pattern) (arity : int) : simple_pattern list option =
  match pat, head with
  | PatWildcard, _ ->
    (* Wildcard expands to n wildcards *)
    Some (List.init arity (fun _ -> PatWildcard))
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

(* Specialize matrix for a head constructor *)
let specialize (matrix : pattern_matrix) (head : simple_pattern) (arity : int) : pattern_matrix =
  List.filter_map (fun row ->
    match row with
    | [] -> None
    | first :: rest ->
      match expand_pattern first head arity with
      | Some sub_pats -> Some (sub_pats @ rest)
      | None -> None
  ) matrix

(* Default matrix - keep rows where first column is wildcard *)
let default_matrix (matrix : pattern_matrix) : pattern_matrix =
  List.filter_map (fun row ->
    match row with
    | [] -> None
    | first :: rest ->
      if is_wildcard first then Some rest
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

and try_all_constructors env ty rest_types sig_ matrix vector =
  let all = all_constructors sig_ in
  List.find_map (fun (ctor, arity, arg_type) ->
    let specialized = specialize matrix ctor arity in
    match expand_pattern (List.hd vector) ctor arity with
    | Some sub_pats ->
      let sub_types = make_sub_types env ty arg_type arity in
      begin match is_useful_with_witness env (sub_types @ rest_types) specialized (sub_pats @ List.tl vector) with
      | Some sub_witness ->
        Some (extract_witness ctor arity sub_witness)
      | None -> None
      end
    | None ->
      (* Pattern doesn't match this constructor, skip *)
      None
  ) all

(* Look up constructor argument type from signature by pattern *)
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
    let specialized = specialize matrix head arity in
    match expand_pattern (List.hd vector) head arity with
    | Some sub_pats ->
      let sub_types = make_sub_types env ty arg_type arity in
      begin match is_useful_with_witness env (sub_types @ rest_types) specialized (sub_pats @ List.tl vector) with
      | Some sub_witness ->
        Some (extract_witness head arity sub_witness)
      | None -> None
      end
    | None -> None
  ) heads

(* Create sub-pattern types from constructor argument type or tuple elements *)
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
