(** Stdlib module loader.

    Loads the Lina standard library modules and makes them available
    in the typing environment. *)

open Common

(** Stdlib module definition: source code and module name. *)
type stdlib_module = {
  name : string;
  source : string;
}

(** Embedded stdlib sources.

    These are the actual contents of the stdlib .lina files, embedded
    directly to avoid runtime file dependencies. *)
let fn_source = {|
(** Function combinators and operators. *)

(** Identity function. *)
let id x = x

(** Constant function - always returns the first argument. *)
let const x _ = x

(** Flip the arguments of a binary function. *)
let flip f x y = f y x

(** Pipe operator: passes a value through a function.
    [x |> f] is equivalent to [f x]. *)
let ( |> ) x f = f x

(** Reverse application: applies a function to a value.
    [f @@ x] is equivalent to [f x]. *)
let ( @@ ) f x = f x

(** Forward composition: [(f >> g) x] is [g (f x)]. *)
let ( >> ) f g x = g (f x)

(** Backward composition: [(f << g) x] is [f (g x)]. *)
let ( << ) f g x = f (g x)

(** Tap: apply a side-effecting function and return the original value. *)
let tap f x =
  let _ = f x in
  x

(** Forward composition: [compose f g x] is [g (f x)].
    Named version of [>>]. *)
let compose f g x = g (f x)

(** Backward composition: [compose_left f g x] is [f (g x)].
    Named version of [<<]. *)
let compose_left f g x = f (g x)

(** Negate a predicate. *)
let negate pred x = if pred x then false else true

(** Apply a function to a value. *)
let apply f x = f x

(** Pipe a value through a function. Named version of [|>]. *)
let pipe x f = f x

(** Ignore a value, returning unit. *)
let ignore _ = ()
|}

let result_source = {|
(** Result utilities for error handling.

    The result type is built-in: type ('a, 'e) result = Ok of 'a | Error of 'e *)

(** {1 Constructors} *)

let ok x = Ok x

let error e = Error e

(** {1 Predicates} *)

let is_ok r = match r with
  | Ok _ -> true
  | Error _ -> false

let is_error r = match r with
  | Ok _ -> false
  | Error _ -> true

(** {1 Extracting} *)

let get_or r default = match r with
  | Ok x -> x
  | Error _ -> default

let get_or_else r f = match r with
  | Ok x -> x
  | Error e -> f e

let get_error_or r default = match r with
  | Ok _ -> default
  | Error e -> e

(** {1 Transforming} *)

let map f r = match r with
  | Ok x -> Ok (f x)
  | Error e -> Error e

let map_error f r = match r with
  | Ok x -> Ok x
  | Error e -> Error (f e)

let flat_map f r = match r with
  | Ok x -> f x
  | Error e -> Error e

let flatten r = match r with
  | Ok inner -> inner
  | Error e -> Error e

(** {1 Combining} *)

let or_ r1 r2 = match r1 with
  | Ok _ -> r1
  | Error _ -> r2

let and_ r1 r2 = match r1 with
  | Ok _ -> r2
  | Error _ -> r1

let map2 f r1 r2 = match r1 with
  | Error e -> Error e
  | Ok x -> match r2 with
    | Error e -> Error e
    | Ok y -> Ok (f x y)

(** {1 Folding} *)

let fold ok_fn error_fn r = match r with
  | Ok x -> ok_fn x
  | Error e -> error_fn e

(** {1 Iteration} *)

let iter f r = match r with
  | Ok x -> f x
  | Error _ -> ()

let iter_error f r = match r with
  | Ok _ -> ()
  | Error e -> f e

(** {1 Conversion} *)

let to_option r = match r with
  | Ok x -> Some x
  | Error _ -> None

let of_option opt error_value = match opt with
  | Some x -> Ok x
  | None -> Error error_value

(** {1 Comparison} *)

let equal ok_eq err_eq r1 r2 = match (r1, r2) with
  | (Ok x1, Ok x2) -> ok_eq x1 x2
  | (Error e1, Error e2) -> err_eq e1 e2
  | _ -> false

(** {1 Binding Operators} *)

let ( let* ) r f = flat_map f r

let ( and* ) r1 r2 = map2 (fun a b -> (a, b)) r1 r2

let ( let+ ) r f = map f r

let ( and+ ) = ( and* )
|}

let option_source = {|
(** Option utilities for optional values.

    The option type is built-in: type 'a option = None | Some of 'a *)

(** {1 Constructors} *)

let none = None

let some value = Some value

(** {1 Predicates} *)

let is_some opt = match opt with
  | Some _ -> true
  | None -> false

let is_none opt = match opt with
  | Some _ -> false
  | None -> true

let contains value opt = match opt with
  | Some inner -> inner == value
  | None -> false

let for_all predicate opt = match opt with
  | None -> true
  | Some value -> predicate value

let exists predicate opt = match opt with
  | None -> false
  | Some value -> predicate value

(** {1 Extracting} *)

let get_or opt default = match opt with
  | Some value -> value
  | None -> default

let get_or_else opt compute_default = match opt with
  | Some value -> value
  | None -> compute_default ()

let get_exn opt = match opt with
  | Some value -> value
  | None -> assert false

let expect message opt = match opt with
  | Some value -> value
  | None ->
      let _ = print message in
      assert false

(** {1 Transforming} *)

let map f opt = match opt with
  | None -> None
  | Some value -> Some (f value)

let flat_map f opt = match opt with
  | None -> None
  | Some value -> f value

let bind opt f = match opt with
  | None -> None
  | Some value -> f value

let filter predicate opt = match opt with
  | None -> None
  | Some value -> if predicate value then Some value else None

let flatten opt = match opt with
  | Some inner -> inner
  | None -> None

let join opt = flatten opt

(** {1 Combining} *)

let or_ opt1 opt2 = match opt1 with
  | Some _ -> opt1
  | None -> opt2

let or_else opt compute_alternative = match opt with
  | Some _ -> opt
  | None -> compute_alternative ()

let and_ opt1 opt2 = match opt1 with
  | Some _ -> opt2
  | None -> None

let map2 f opt1 opt2 = match opt1 with
  | None -> None
  | Some value1 -> match opt2 with
    | None -> None
    | Some value2 -> Some (f value1 value2)

let zip opt1 opt2 = match opt1 with
  | None -> None
  | Some value1 -> match opt2 with
    | None -> None
    | Some value2 -> Some (value1, value2)

let product opt1 opt2 = zip opt1 opt2

let blend merge_fn opt1 opt2 = match (opt1, opt2) with
  | (None, None) -> None
  | (Some value, None) -> Some value
  | (None, Some value) -> Some value
  | (Some value1, Some value2) -> Some (merge_fn value1 value2)

(** {1 Folding and Iteration} *)

let fold default_value some_fn opt = match opt with
  | None -> default_value
  | Some value -> some_fn value

let iter f opt = match opt with
  | Some value -> f value
  | None -> ()

(** {1 Comparison} *)

let equal eq_fn opt1 opt2 = match (opt1, opt2) with
  | (None, None) -> true
  | (Some value1, Some value2) -> eq_fn value1 value2
  | _ -> false

let compare cmp_fn opt1 opt2 = match (opt1, opt2) with
  | (None, None) -> 0
  | (None, Some _) -> 0 - 1
  | (Some _, None) -> 1
  | (Some value1, Some value2) -> cmp_fn value1 value2

(** {1 Conversion} *)

let to_result error_value opt = match opt with
  | Some value -> Ok value
  | None -> Error error_value

let of_result result = match result with
  | Ok value -> Some value
  | Error _ -> None

(** {1 Binding Operators} *)

let ( let* ) opt f = flat_map f opt

let ( and* ) opt1 opt2 = product opt1 opt2

let ( let+ ) opt f = map f opt

let ( and+ ) = ( and* )
|}

let list_source = {|
(** List utilities for immutable linked lists.

    The list type is built-in: type 'a list = Nil | Cons of 'a * 'a list

    List syntax: [[]] for empty list, [[1; 2; 3]] for list literals,
    [x :: xs] for cons pattern matching. *)

(** {1 Construction} *)

(** The empty list. *)
let empty = Nil

(** Create a list with a single element. *)
let singleton x = Cons (x, Nil)

(** Prepend an element to a list. *)
let cons x xs = Cons (x, xs)

(** Create a list of integers from [start] to [stop] (inclusive).
    Returns empty list if [start > stop]. *)
let rec range start stop =
  if start > stop then Nil
  else Cons (start, range (start + 1) stop)

(** Create a list with [n] copies of [x].
    Returns empty list if [n <= 0].
    Named [replicate] instead of [repeat] to avoid Lua keyword conflict. *)
let rec replicate n x =
  if n <= 0 then Nil
  else Cons (x, replicate (n - 1) x)

(** Create a list of length [n] where element at index [i] is [f i].
    Returns empty list if [n <= 0]. *)
let init n f =
  let rec go i =
    if i >= n then Nil
    else Cons (f i, go (i + 1))
  in
  go 0

(** {1 Basic Operations} *)

(** Return the length of a list. O(n). *)
let length lst =
  let rec go acc xs = match xs with
    | Nil -> acc
    | Cons (_, rest) -> go (acc + 1) rest
  in
  go 0 lst

(** Return [true] if the list is empty. *)
let is_empty lst = match lst with
  | Nil -> true
  | Cons _ -> false

(** Return the first element, or [None] if empty. *)
let head lst = match lst with
  | Nil -> None
  | Cons (x, _) -> Some x

(** Return the list without its first element, or [None] if empty. *)
let tail lst = match lst with
  | Nil -> None
  | Cons (_, xs) -> Some xs

(** Return the last element, or [None] if empty. O(n). *)
let rec last lst = match lst with
  | Nil -> None
  | Cons (x, Nil) -> Some x
  | Cons (_, xs) -> last xs

(** Return the element at index [n], or [None] if out of bounds.
    Index is 0-based. O(n). *)
let rec nth n lst =
  if n < 0 then None
  else match lst with
    | Nil -> None
    | Cons (x, xs) ->
        if n == 0 then Some x
        else nth (n - 1) xs

(** {1 Transformations} *)

(** Apply [f] to each element, returning a new list. *)
let rec map f lst = match lst with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

(** Apply [f] to each element with its index. *)
let mapi f lst =
  let rec go i xs = match xs with
    | Nil -> Nil
    | Cons (x, rest) -> Cons (f i x, go (i + 1) rest)
  in
  go 0 lst

(** Keep only elements satisfying [predicate]. *)
let rec filter predicate lst = match lst with
  | Nil -> Nil
  | Cons (x, xs) ->
      if predicate x then Cons (x, filter predicate xs)
      else filter predicate xs

(** Apply [f] to each element and keep [Some] results. *)
let rec filter_map f lst = match lst with
  | Nil -> Nil
  | Cons (x, xs) -> match f x with
    | None -> filter_map f xs
    | Some y -> Cons (y, filter_map f xs)

(** Reverse a list. O(n). *)
let reverse lst =
  let rec go acc xs = match xs with
    | Nil -> acc
    | Cons (x, rest) -> go (Cons (x, acc)) rest
  in
  go Nil lst

(** Concatenate two lists. O(length of first list). *)
let rec append lst1 lst2 = match lst1 with
  | Nil -> lst2
  | Cons (x, xs) -> Cons (x, append xs lst2)

(** Concatenate a list of lists. *)
let rec concat lists = match lists with
  | Nil -> Nil
  | Cons (x, xs) -> append x (concat xs)

(** Map then flatten. [flat_map f lst] is [concat (map f lst)]. *)
let flat_map f lst = concat (map f lst)

(** {1 Folding} *)

(** Left fold: [fold_left f init [x1; x2; x3]] is [f (f (f init x1) x2) x3].
    Tail-recursive, prefer this for large lists. *)
let rec fold_left f acc lst = match lst with
  | Nil -> acc
  | Cons (x, xs) -> fold_left f (f acc x) xs

(** Right fold: [fold_right f [x1; x2; x3] init] is [f x1 (f x2 (f x3 init))].
    Not tail-recursive, may stack overflow on large lists. *)
let rec fold_right f lst acc = match lst with
  | Nil -> acc
  | Cons (x, xs) -> f x (fold_right f xs acc)

(** {1 Searching} *)

(** Find the first element satisfying [predicate], or [None]. *)
let rec find predicate lst = match lst with
  | Nil -> None
  | Cons (x, xs) ->
      if predicate x then Some x
      else find predicate xs

(** Find the index of the first element satisfying [predicate], or [None]. *)
let find_index predicate lst =
  let rec go i xs = match xs with
    | Nil -> None
    | Cons (x, rest) ->
        if predicate x then Some i
        else go (i + 1) rest
  in
  go 0 lst

(** Return [true] if any element satisfies [predicate]. *)
let rec exists predicate lst = match lst with
  | Nil -> false
  | Cons (x, xs) ->
      if predicate x then true
      else exists predicate xs

(** Return [true] if all elements satisfy [predicate]. *)
let rec for_all predicate lst = match lst with
  | Nil -> true
  | Cons (x, xs) ->
      if predicate x then for_all predicate xs
      else false

(** Return [true] if [element] is in the list. Uses [==] for comparison. *)
let rec mem element lst = match lst with
  | Nil -> false
  | Cons (x, xs) ->
      if x == element then true
      else mem element xs

(** {1 Sorting} *)

(** Split a list into two halves. *)
let split_half lst =
  let rec go slow fast = match fast with
    | Nil -> (Nil, slow)
    | Cons (_, Nil) -> (Nil, slow)
    | Cons (_, Cons (_, fast_rest)) -> match slow with
      | Nil -> (Nil, Nil)
      | Cons (y, slow_rest) ->
          let (left, right) = go slow_rest fast_rest in
          (Cons (y, left), right)
  in
  go lst lst

(** Merge two sorted lists using comparison function [cmp].
    [cmp a b] should return negative if [a < b], 0 if equal, positive if [a > b]. *)
let rec merge cmp lst1 lst2 = match (lst1, lst2) with
  | (Nil, ys) -> ys
  | (xs, Nil) -> xs
  | (Cons (x, xs), Cons (y, ys)) ->
      if cmp x y <= 0 then Cons (x, merge cmp xs lst2)
      else Cons (y, merge cmp lst1 ys)

(** Sort a list using comparison function [cmp]. Uses merge sort, O(n log n).
    [cmp a b] should return negative if [a < b], 0 if equal, positive if [a > b]. *)
let rec sort cmp lst = match lst with
  | Nil -> Nil
  | Cons (_, Nil) -> lst
  | _ ->
      let (left, right) = split_half lst in
      merge cmp (sort cmp left) (sort cmp right)

(** Sort a list by comparing keys extracted with [key_fn].
    Uses merge sort, O(n log n). *)
let sort_by key_fn lst =
  sort (fun a b ->
    let ka = key_fn a in
    let kb = key_fn b in
    if ka < kb then 0 - 1
    else if ka > kb then 1
    else 0
  ) lst

(** {1 Iteration} *)

(** Apply [f] to each element for side effects. *)
let rec iter f lst = match lst with
  | Nil -> ()
  | Cons (x, xs) ->
      let _ = f x in
      iter f xs

(** Apply [f] to each element with its index for side effects. *)
let iteri f lst =
  let rec go i xs = match xs with
    | Nil -> ()
    | Cons (x, rest) ->
        let _ = f i x in
        go (i + 1) rest
  in
  go 0 lst

(** {1 Zipping} *)

(** Combine two lists into a list of pairs.
    Stops at the shorter list. *)
let rec zip lst1 lst2 = match (lst1, lst2) with
  | (Nil, _) -> Nil
  | (_, Nil) -> Nil
  | (Cons (x, xs), Cons (y, ys)) -> Cons ((x, y), zip xs ys)

(** Split a list of pairs into two lists. *)
let unzip lst =
  let rec go acc1 acc2 xs = match xs with
    | Nil -> (reverse acc1, reverse acc2)
    | Cons ((a, b), rest) -> go (Cons (a, acc1)) (Cons (b, acc2)) rest
  in
  go Nil Nil lst

(** {1 Comparison} *)

(** Return [true] if lists are equal using [eq_fn] for element comparison. *)
let rec equal eq_fn lst1 lst2 = match (lst1, lst2) with
  | (Nil, Nil) -> true
  | (Cons (x, xs), Cons (y, ys)) ->
      if eq_fn x y then equal eq_fn xs ys
      else false
  | _ -> false

(** Compare two lists lexicographically using [cmp_fn].
    Returns negative if [lst1 < lst2], 0 if equal, positive if [lst1 > lst2]. *)
let rec compare cmp_fn lst1 lst2 = match (lst1, lst2) with
  | (Nil, Nil) -> 0
  | (Nil, Cons _) -> 0 - 1
  | (Cons _, Nil) -> 1
  | (Cons (x, xs), Cons (y, ys)) ->
      let c = cmp_fn x y in
      if c != 0 then c
      else compare cmp_fn xs ys

(** {1 Additional Utilities} *)

(** Take the first [n] elements. Returns fewer if list is shorter. *)
let rec take n lst =
  if n <= 0 then Nil
  else match lst with
    | Nil -> Nil
    | Cons (x, xs) -> Cons (x, take (n - 1) xs)

(** Drop the first [n] elements. Returns empty if list is shorter. *)
let rec drop n lst =
  if n <= 0 then lst
  else match lst with
    | Nil -> Nil
    | Cons (_, xs) -> drop (n - 1) xs

(** Split at index [n]: [(take n lst, drop n lst)]. *)
let split_at n lst = (take n lst, drop n lst)

(** Partition a list into elements that satisfy [predicate] and those that don't. *)
let partition predicate lst =
  let rec go yes no xs = match xs with
    | Nil -> (reverse yes, reverse no)
    | Cons (x, rest) ->
        if predicate x then go (Cons (x, yes)) no rest
        else go yes (Cons (x, no)) rest
  in
  go Nil Nil lst

(** Insert [separator] between each element. *)
let intersperse separator lst = match lst with
  | Nil -> Nil
  | Cons (x, xs) ->
      let rec go ys = match ys with
        | Nil -> Nil
        | Cons (y, rest) -> Cons (separator, Cons (y, go rest))
      in
      Cons (x, go xs)
|}

(** All stdlib modules in load order.
    Order matters if there are dependencies between modules. *)
let stdlib_modules = [
  { name = "Fn"; source = fn_source };
  { name = "Result"; source = result_source };
  { name = "Option"; source = option_source };
  { name = "List"; source = list_source };
]

(** Use the existing signature extraction from Structure_infer *)
let signature_of_typed_structure = Typing.Structure_infer.signature_of_typed_structure

(** Compile a single stdlib module and return both its binding and Lua code.

    @param env The current environment (with builtins)
    @param stdlib_mod The stdlib module to load
    @return (name, binding, lua_code) *)
let compile_stdlib_module env stdlib_mod =
  let filename = Printf.sprintf "<stdlib/%s>" stdlib_mod.name in

  (* Parse the source *)
  let ast = Parsing.Parse.structure_from_string ~filename stdlib_mod.source in

  (* Reset type variable IDs for clean inference *)
  Typing.Types.reset_type_variable_id ();

  (* Create typing context and infer types *)
  let ctx = Typing.Typing_context.create env in
  let typed_ast, _ctx = Typing.Inference.infer_structure ctx ast in

  (* Clear any warnings generated during stdlib compilation *)
  let _ = Compiler_error.get_warnings () in

  (* Extract signature from typed structure *)
  let signature = signature_of_typed_structure typed_ast in

  (* Create the module binding *)
  let module_type = Typing.Module_types.ModTypeSig signature in
  let module_id = Identifier.create stdlib_mod.name in
  let binding = Typing.Module_types.{
    binding_name = stdlib_mod.name;
    binding_id = module_id;
    binding_type = module_type;
    binding_alias = None;
  } in

  (* Generate Lua code for the module *)
  let lambda = Lambda.translate_structure typed_ast in
  let lua_ast = Lua.Codegen.generate lambda in
  let lua_code = Lua.Printer.print_chunk lua_ast in

  (stdlib_mod.name, binding, lua_code)

(** Compiled stdlib result: environment and Lua prelude. *)
type stdlib_result = {
  env : Typing.Environment.t;
  lua_prelude : string;
}

(** Extract exported value names from a module signature. *)
let exported_names_of_signature signature =
  List.filter_map (function
    | Typing.Module_types.SigValue (name, _) -> Some name
    | _ -> None
  ) signature

(** Mangle a Lina name to its Lua representation.
    Uses the same algorithm as Lua.Identifier_mangle.sanitize_name. *)
let mangle_for_lua name =
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> Buffer.add_char buf c
    | '(' -> Buffer.add_string buf "_lp_"
    | ')' -> Buffer.add_string buf "_rp_"
    | '*' -> Buffer.add_string buf "_star_"
    | '+' -> Buffer.add_string buf "_plus_"
    | '-' -> Buffer.add_string buf "_minus_"
    | '/' -> Buffer.add_string buf "_slash_"
    | '&' -> Buffer.add_string buf "_amp_"
    | '|' -> Buffer.add_string buf "_bar_"
    | '!' -> Buffer.add_string buf "_bang_"
    | '?' -> Buffer.add_string buf "_qmark_"
    | '=' -> Buffer.add_string buf "_eq_"
    | '<' -> Buffer.add_string buf "_lt_"
    | '>' -> Buffer.add_string buf "_gt_"
    | '@' -> Buffer.add_string buf "_at_"
    | '^' -> Buffer.add_string buf "_caret_"
    | '~' -> Buffer.add_string buf "_tilde_"
    | '.' -> Buffer.add_string buf "_dot_"
    | ':' -> Buffer.add_string buf "_colon_"
    | '$' -> Buffer.add_string buf "_dollar_"
    | '%' -> Buffer.add_string buf "_percent_"
    | ' ' -> ()
    | _ -> Buffer.add_char buf '_'
  ) name;
  let result = Buffer.contents buf in
  if String.length result = 0 then "_anon"
  else if result.[0] >= '0' && result.[0] <= '9' then "_" ^ result
  else result

(** Generate the Lua code to define a module as a local table.

    Example: module name "Fn" with lua_code and exports [id; const] generates:
    local Fn = (function()
      ... lua_code ...
      return { id = id, const = const }
    end)()
*)
let wrap_module_lua name lua_code exports =
  let return_fields = List.map (fun export_name ->
    let lua_name = mangle_for_lua export_name in
    Printf.sprintf "[\"%s\"] = %s" export_name lua_name
  ) exports in
  let return_table = "return {" ^ String.concat ", " return_fields ^ "}" in
  Printf.sprintf "local %s = (function()\n%s\n%s\nend)();\n" name lua_code return_table

(** Load all stdlib modules and return environment + Lua prelude.

    @param base_env The base environment (typically Environment.initial)
    @return stdlib_result with env and lua_prelude *)
let load_stdlib_with_prelude base_env =
  let env, lua_codes = List.fold_left (fun (env, codes) stdlib_mod ->
    let name, binding, lua_code = compile_stdlib_module env stdlib_mod in
    let env = Typing.Environment.add_module name binding env in

    (* Extract exported names from the signature *)
    let exports = match binding.Typing.Module_types.binding_type with
      | Typing.Module_types.ModTypeSig signature ->
        exported_names_of_signature signature
      | _ -> []
    in

    let wrapped_code = wrap_module_lua name lua_code exports in
    (env, wrapped_code :: codes)
  ) (base_env, []) stdlib_modules in

  let lua_prelude = String.concat "\n" (List.rev lua_codes) in
  { env; lua_prelude }

(** Cached stdlib result to avoid recompilation. *)
let cached_stdlib = ref None

(** Get the stdlib result (environment + prelude), using cache. *)
let get_stdlib () =
  match !cached_stdlib with
  | Some result -> result
  | None ->
    let result = load_stdlib_with_prelude Typing.Environment.initial in
    cached_stdlib := Some result;
    result

(** Get the initial environment with stdlib modules loaded. *)
let initial_with_stdlib () =
  (get_stdlib ()).env

(** Get the Lua prelude code for stdlib modules. *)
let stdlib_prelude () =
  (get_stdlib ()).lua_prelude
