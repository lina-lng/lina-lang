open Common

(* Occurrence: path from scrutinee to sub-value *)
type occurrence_step =
  | OccTupleField of int
  | OccConstructorArg
  | OccRecordField of string

type occurrence = occurrence_step list

(* Head constructor for switching - uses Parsing constant to avoid Lambda dependency *)
type head_constructor =
  | HCWildcard
  | HCConstant of Parsing.Syntax_tree.constant
  | HCTuple of int  (* arity *)
  | HCConstructor of string * int  (* name, tag_index *)
  | HCRecord of string list  (* field names *)
  | HCPolyVariant of string  (* polymorphic variant tag *)

(* Clause in pattern matrix *)
type clause = {
  clause_id : int; [@warning "-69"]
  clause_patterns : Typing.Typed_tree.typed_pattern list;
  clause_guard : Typing.Typed_tree.typed_expression option;
  clause_action : Typing.Typed_tree.typed_expression;
  clause_bindings : (Identifier.t * occurrence) list;
}

(* Pattern matrix *)
type pattern_matrix = {
  occurrences : occurrence list;
  clauses : clause list;
}

(* Decision tree output - uses inline records for cleaner pattern matching *)
type decision_tree =
  | DTFail
  | DTLeaf of {
      leaf_bindings : (Identifier.t * occurrence) list;
      leaf_action : Typing.Typed_tree.typed_expression;
    }
  | DTSwitch of {
      switch_occurrence : occurrence;
      switch_cases : (head_constructor * decision_tree) list;
      switch_default : decision_tree option;
    }
  | DTGuard of {
      guard_bindings : (Identifier.t * occurrence) list;
      guard_condition : Typing.Typed_tree.typed_expression;
      guard_then : decision_tree;
      guard_else : decision_tree;
    }

(** {1 List Split Utilities}

    These helpers perform single-pass list splitting, avoiding the O(n)
    double traversal of using List.filteri twice. *)

(** [split_at_remove idx lst] splits [lst] at index [idx], removing the element
    at that index.

    Returns [(before, after)] where [before] contains elements with index < [idx]
    and [after] contains elements with index > [idx].

    Example: [split_at_remove 2 [a;b;c;d;e]] returns [([a;b], [d;e])]

    Time complexity: O(n) single pass instead of O(2n) with two List.filteri calls. *)
let split_at_remove idx lst =
  let rec go i before = function
    | [] -> (List.rev before, [])
    | _ :: xs when i = idx -> (List.rev before, xs)
    | x :: xs -> go (i + 1) (x :: before) xs
  in
  go 0 [] lst

(** [remove_at idx lst] removes the element at index [idx] from [lst].

    Equivalent to [List.filteri (fun i _ -> i <> idx) lst] but single pass. *)
let remove_at idx lst =
  let before, after = split_at_remove idx lst in
  before @ after

(** Extract the head constructor from a typed pattern. *)
let head_of_pattern (pat : Typing.Typed_tree.typed_pattern) : head_constructor =
  let open Typing.Typed_tree in
  match pat.pattern_desc with
  | TypedPatternVariable _ -> HCWildcard
  | TypedPatternWildcard -> HCWildcard
  | TypedPatternConstant c -> HCConstant c
  | TypedPatternTuple pats -> HCTuple (List.length pats)
  | TypedPatternConstructor (ctor_info, _) ->
      HCConstructor (ctor_info.Typing.Types.constructor_name,
                     ctor_info.Typing.Types.constructor_tag_index)
  | TypedPatternRecord (fields, _) ->
      HCRecord (List.map (fun f -> f.typed_pattern_field_name) fields)
  | TypedPatternLocallyAbstract _ -> HCWildcard
  | TypedPatternPolyVariant (tag, _) -> HCPolyVariant tag
  | TypedPatternError _ -> HCWildcard

(* Compare constants for equality *)
let constants_equal c1 c2 =
  match c1, c2 with
  | Parsing.Syntax_tree.ConstantInteger n1, Parsing.Syntax_tree.ConstantInteger n2 -> n1 = n2
  | Parsing.Syntax_tree.ConstantFloat f1, Parsing.Syntax_tree.ConstantFloat f2 -> f1 = f2
  | Parsing.Syntax_tree.ConstantString s1, Parsing.Syntax_tree.ConstantString s2 -> s1 = s2
  | Parsing.Syntax_tree.ConstantBoolean b1, Parsing.Syntax_tree.ConstantBoolean b2 -> b1 = b2
  | Parsing.Syntax_tree.ConstantUnit, Parsing.Syntax_tree.ConstantUnit -> true
  | _ -> false

(** Check if pattern is a wildcard (matches anything without testing). *)
let is_wildcard (pat : Typing.Typed_tree.typed_pattern) : bool =
  let open Typing.Typed_tree in
  match pat.pattern_desc with
  | TypedPatternVariable _ | TypedPatternWildcard | TypedPatternLocallyAbstract _ -> true
  | _ -> false

(* Extend an occurrence with an additional step *)
let extend_occurrence (occ : occurrence) (step : occurrence_step) : occurrence =
  occ @ [step]

(** Collect bindings from the pattern head only.
    Sub-patterns are added to the matrix and their bindings collected later. *)
let collect_head_bindings (pat : Typing.Typed_tree.typed_pattern) (occ : occurrence) : (Identifier.t * occurrence) list =
  let open Typing.Typed_tree in
  match pat.pattern_desc with
  | TypedPatternVariable id -> [(id, occ)]
  | _ -> []


(* Create initial matrix from match arms *)
let initial_matrix (arms : Typing.Typed_tree.typed_match_arm list) : pattern_matrix =
  let clauses = List.mapi (fun i (arm : Typing.Typed_tree.typed_match_arm) ->
    {
      clause_id = i;
      clause_patterns = [arm.typed_arm_pattern];
      clause_guard = arm.typed_arm_guard;
      clause_action = arm.typed_arm_expression;
      clause_bindings = [];
    }
  ) arms in
  { occurrences = [[]]; clauses }  (* Root occurrence is empty path *)

(* Get arity of a head constructor based on the pattern *)
let head_arity (head : head_constructor) : int =
  match head with
  | HCWildcard -> 0
  | HCConstant _ -> 0
  | HCTuple arity -> arity
  | HCConstructor _ -> 1  (* Constructors have at most 1 argument in Lina *)
  | HCPolyVariant _ -> 1  (* Poly variants have at most 1 argument *)
  | HCRecord fields -> List.length fields

(* Create wildcard pattern with same type *)
let make_wildcard (ty : Typing.Types.type_expression) (loc : Location.t) : Typing.Typed_tree.typed_pattern =
  { pattern_desc = Typing.Typed_tree.TypedPatternWildcard;
    pattern_type = ty;
    pattern_location = loc }

(** Expand a wildcard pattern into sub-patterns based on head constructor arity. *)
let expand_wildcard_for_head pat head arity =
  let wildcard () = make_wildcard pat.Typing.Typed_tree.pattern_type pat.pattern_location in
  match head with
  | HCConstant _ | HCWildcard -> Some []
  | HCRecord fields -> Some (List.map (fun _ -> wildcard ()) fields)
  | HCTuple _ | HCConstructor _ | HCPolyVariant _ -> Some (List.init arity (fun _ -> wildcard ()))

(** Get sub-patterns when specializing.
    Wildcards expand to n wildcards based on arity.
    Concrete patterns must match the head constructor. *)
let sub_patterns (pat : Typing.Typed_tree.typed_pattern) (head : head_constructor) (arity : int) : Typing.Typed_tree.typed_pattern list option =
  if is_wildcard pat then
    expand_wildcard_for_head pat head arity
  else
    let open Typing.Typed_tree in
    match pat.pattern_desc, head with
    | TypedPatternTuple pats, HCTuple _ ->
        Some pats

    | TypedPatternConstructor (ctor_info, arg), HCConstructor (name', _)
      when ctor_info.Typing.Types.constructor_name = name' ->
        (match arg with Some p -> Some [p] | None -> Some [])

    | TypedPatternConstructor _, HCConstructor _ ->
        None

    | TypedPatternConstant c, HCConstant c' when constants_equal c c' ->
        Some []

    | TypedPatternConstant _, HCConstant _ ->
        None

    | TypedPatternRecord (fields, _), HCRecord expected_fields ->
        let sub_pats = List.map (fun name ->
          match List.find_opt (fun f -> f.typed_pattern_field_name = name) fields with
          | Some f -> f.typed_pattern_field_pattern
          | None -> make_wildcard pat.pattern_type pat.pattern_location
        ) expected_fields in
        Some sub_pats

    | TypedPatternPolyVariant (tag, arg), HCPolyVariant tag' when tag = tag' ->
        (match arg with Some p -> Some [p] | None -> Some [])

    | TypedPatternPolyVariant _, HCPolyVariant _ ->
        None

    | _ ->
        None

(* Compute new occurrences for sub-patterns *)
let sub_occurrences (occ : occurrence) (head : head_constructor) (arity : int) : occurrence list =
  match head with
  | HCWildcard -> []
  | HCConstant _ -> []
  | HCTuple _ ->
    List.init arity (fun i -> extend_occurrence occ (OccTupleField i))
  | HCConstructor _ ->
    if arity > 0 then [extend_occurrence occ OccConstructorArg]
    else []
  | HCPolyVariant _ ->
    if arity > 0 then [extend_occurrence occ OccConstructorArg]
    else []
  | HCRecord fields ->
    List.map (fun name -> extend_occurrence occ (OccRecordField name)) fields

(* Specialize matrix for a head constructor at given column with given arity *)
let specialize (matrix : pattern_matrix) (col : int) (head : head_constructor) (arity : int) : pattern_matrix =
  let occ = List.nth matrix.occurrences col in
  let new_sub_occs = sub_occurrences occ head arity in

  (* Replace column with expanded sub-occurrences (single-pass split) *)
  let new_occurrences =
    let before, after = split_at_remove col matrix.occurrences in
    before @ new_sub_occs @ after
  in

  (* Filter and transform clauses *)
  let new_clauses = List.filter_map (fun clause ->
    let pat = List.nth clause.clause_patterns col in
    match sub_patterns pat head arity with
    | None -> None  (* Clause doesn't match this constructor *)
    | Some sub_pats ->
      (* Collect only head bindings - sub-pattern bindings collected when they're processed *)
      let new_bindings = collect_head_bindings pat occ @ clause.clause_bindings in
      (* Replace pattern column with sub-patterns (single-pass split) *)
      let before_pats, after_pats = split_at_remove col clause.clause_patterns in
      Some {
        clause with
        clause_patterns = before_pats @ sub_pats @ after_pats;
        clause_bindings = new_bindings;
      }
  ) matrix.clauses in

  { occurrences = new_occurrences; clauses = new_clauses }

(* Default matrix: keep rows where column is wildcard *)
let default_matrix (matrix : pattern_matrix) (col : int) : pattern_matrix =
  let occ = List.nth matrix.occurrences col in

  (* Remove the column from occurrences (single-pass) *)
  let new_occurrences = remove_at col matrix.occurrences in

  (* Keep only rows with wildcard at column *)
  let new_clauses = List.filter_map (fun clause ->
    let pat = List.nth clause.clause_patterns col in
    if is_wildcard pat then
      (* For wildcards (variables/wildcards), collect_head_bindings captures the variable binding *)
      let new_bindings = collect_head_bindings pat occ @ clause.clause_bindings in
      let new_patterns = remove_at col clause.clause_patterns in
      Some { clause with
        clause_patterns = new_patterns;
        clause_bindings = new_bindings
      }
    else
      None
  ) matrix.clauses in

  { occurrences = new_occurrences; clauses = new_clauses }

(* Column selection: pick leftmost column where first row is not wildcard *)
let select_column (matrix : pattern_matrix) : int =
  match matrix.clauses with
  | [] -> 0
  | first_clause :: _ ->
    let rec find_needed idx patterns =
      match patterns with
      | [] -> 0  (* All wildcards - pick first *)
      | pat :: rest ->
        if not (is_wildcard pat) then idx
        else find_needed (idx + 1) rest
    in
    find_needed 0 first_clause.clause_patterns

(** Generate a unique key for a head constructor for deduplication.
    This allows O(1) hash-based duplicate detection instead of O(n) list search. *)
let rec head_constructor_key = function
  | HCWildcard -> "W"
  | HCConstructor (name, _) -> "C:" ^ name
  | HCPolyVariant tag -> "P:" ^ tag
  | HCConstant c -> "K:" ^ constant_to_string c
  | HCTuple arity -> "T:" ^ string_of_int arity
  | HCRecord fields -> "R:" ^ String.concat "," fields

and constant_to_string = function
  | Parsing.Syntax_tree.ConstantInteger i -> "i" ^ string_of_int i
  | Parsing.Syntax_tree.ConstantFloat f -> "f" ^ string_of_float f
  | Parsing.Syntax_tree.ConstantString s -> "s" ^ s
  | Parsing.Syntax_tree.ConstantBoolean b -> "b" ^ string_of_bool b
  | Parsing.Syntax_tree.ConstantUnit -> "u"

(* Collect head constructors from a column *)
let collect_head_constructors (matrix : pattern_matrix) (col : int) : head_constructor list =
  let heads = List.filter_map (fun clause ->
    let pat = List.nth clause.clause_patterns col in
    let head = head_of_pattern pat in
    match head with
    | HCWildcard -> None
    | _ -> Some head
  ) matrix.clauses in
  (* Remove duplicates using hash-based deduplication (O(n) instead of O(n²)) *)
  let seen = Hashtbl.create 16 in
  List.filter (fun h ->
    let key = head_constructor_key h in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)
  ) heads

(* Adjust arity for constructor based on whether it has an arg in any pattern *)
let adjust_constructor_arity (matrix : pattern_matrix) (col : int) (head : head_constructor) : int =
  match head with
  | HCConstructor (name, _) ->
    (* Check if any pattern with this constructor has an argument *)
    let has_arg = List.exists (fun clause ->
      let pat = List.nth clause.clause_patterns col in
      match pat.pattern_desc with
      | Typing.Typed_tree.TypedPatternConstructor (ctor_info, Some _) when ctor_info.Typing.Types.constructor_name = name -> true
      | _ -> false
    ) matrix.clauses in
    if has_arg then 1 else 0
  | HCPolyVariant tag ->
    (* Check if any pattern with this poly variant tag has an argument *)
    let has_arg = List.exists (fun clause ->
      let pat = List.nth clause.clause_patterns col in
      match pat.pattern_desc with
      | Typing.Typed_tree.TypedPatternPolyVariant (t, Some _) when t = tag -> true
      | _ -> false
    ) matrix.clauses in
    if has_arg then 1 else 0
  | _ -> head_arity head

(* Main compilation function *)
let rec compile_matrix (matrix : pattern_matrix) : decision_tree =
  (* Base case 1: Empty matrix - match failure *)
  if matrix.clauses = [] then
    DTFail

  (* Base case 2: Empty pattern row - all patterns matched *)
  else if matrix.occurrences = [] then
    let clause = List.hd matrix.clauses in
    match clause.clause_guard with
    | None ->
      DTLeaf {
        leaf_bindings = clause.clause_bindings;
        leaf_action = clause.clause_action;
      }
    | Some guard ->
      DTGuard {
        guard_bindings = clause.clause_bindings;
        guard_condition = guard;
        guard_then = DTLeaf {
          leaf_bindings = clause.clause_bindings;
          leaf_action = clause.clause_action;
        };
        guard_else = compile_matrix { matrix with clauses = List.tl matrix.clauses };
      }

  (* Recursive case: select column and compile *)
  else
    let col = select_column matrix in
    let occ = List.nth matrix.occurrences col in
    let heads = collect_head_constructors matrix col in

    (* Build switch cases for each head constructor *)
    let cases = List.map (fun head ->
      let arity = adjust_constructor_arity matrix col head in
      let specialized = specialize matrix col head arity in
      (head, compile_matrix specialized)
    ) heads in

    (* Build default case *)
    let default_mat = default_matrix matrix col in
    let default =
      if default_mat.clauses = [] then None
      else Some (compile_matrix default_mat)
    in

    DTSwitch {
      switch_occurrence = occ;
      switch_cases = cases;
      switch_default = default;
    }

(* Compile match arms to decision tree *)
let compile_match (arms : Typing.Typed_tree.typed_match_arm list) : decision_tree =
  let matrix = initial_matrix arms in
  compile_matrix matrix
