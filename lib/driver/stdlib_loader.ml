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

let ord_source = {|
(** Ordering utilities for type-safe comparison.

    Provides a three-valued ordering type as an alternative to integer
    comparison conventions (-1, 0, 1). *)

type ordering = Less | Equal | Greater

(** {1 Constructors} *)

let less = Less
let equal_ordering = Equal
let greater = Greater

(** {1 Conversion} *)

(** Convert an integer comparison result to ordering.
    Negative -> Less, zero -> Equal, positive -> Greater. *)
let of_int n =
  if n < 0 then Less
  else if n > 0 then Greater
  else Equal

(** Convert ordering to integer (-1, 0, 1). *)
let to_int ord = match ord with
  | Less -> 0 - 1
  | Equal -> 0
  | Greater -> 1

(** {1 Predicates} *)

let is_less ord = match ord with
  | Less -> true
  | _ -> false

let is_equal ord = match ord with
  | Equal -> true
  | _ -> false

let is_greater ord = match ord with
  | Greater -> true
  | _ -> false

(** {1 Combinators} *)

(** Reverse an ordering. *)
let flip ord = match ord with
  | Less -> Greater
  | Equal -> Equal
  | Greater -> Less

(** Chain two orderings: if first is Equal, return second.
    Useful for lexicographic comparison. *)
let then_ first second = match first with
  | Equal -> second
  | other -> other

(** {1 Comparison Helpers} *)

(** Compare two integers. *)
let int_compare a b =
  if a < b then Less
  else if a > b then Greater
  else Equal

(** Compare two booleans (false < true). *)
let bool_compare a b = match (a, b) with
  | (false, false) -> Equal
  | (true, true) -> Equal
  | (false, true) -> Less
  | (true, false) -> Greater

(** Compare two strings lexicographically. *)
let string_compare a b =
  if a < b then Less
  else if a > b then Greater
  else Equal

(** {1 Self-Comparison} *)

(** Compare two orderings (Less < Equal < Greater). *)
let compare ord1 ord2 =
  let rank ord = match ord with
    | Less -> 0
    | Equal -> 1
    | Greater -> 2
  in
  int_compare (rank ord1) (rank ord2)

(** Check equality of two orderings. *)
let equal ord1 ord2 = match (ord1, ord2) with
  | (Less, Less) -> true
  | (Equal, Equal) -> true
  | (Greater, Greater) -> true
  | _ -> false
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

(** [get_exn r] returns the value if [r] is [Ok v], raises error if [Error].
    Use only when you are certain the result is Ok. *)
let get_exn r = match r with
  | Ok x -> x
  | Error _ -> error "Result.get_exn: called on Error"

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

(** {1 Sequencing} *)

(** [sequence results] converts a list of results into a result of list.
    Returns [Ok] with all values if all are [Ok], or first [Error] encountered. *)
let sequence results =
  let rec go acc lst = match lst with
    | [] -> Ok (List.reverse acc)
    | r :: rest -> match r with
      | Ok x -> go (x :: acc) rest
      | Error e -> Error e
  in
  go [] results

(** {1 Protected Calls} *)

(** [try_with f] calls [f ()] wrapped in Lua's pcall.
    Returns [Ok result] if the function succeeds,
    [Error message] if it throws an exception.

    Example:
    {[
      let safe_parse s = Result.try_with (fun () -> parse s)
      (* Returns Ok parsed_value or Error "error message" *)
    ]} *)
@val @return(pcall)
external try_with : (unit -> 'a) -> ('a, string) result = "pcall"

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

let array_source = {|
(** Mutable array utilities.

    Arrays are 0-indexed, fixed-size, mutable sequences backed by Lua tables.
    For immutable sequences, use the List module instead.

    The array type is built-in: ['a array] *)

(** {1 Construction} *)

(** Create an array of [n] elements, all initialized to [value].
    Returns empty array if [n <= 0]. *)
let make n value =
  if n <= 0 then array_make 0 value
  else array_make n value

(** Create an array of length [n] where element at index [i] is [f i].
    Returns empty array if [n <= 0]. Note: [f 0] is always called. *)
let init n f =
  let first = f 0 in
  if n <= 0 then array_make 0 first
  else
    let arr = array_make n first in
    for i = 1 to n - 1 do
      let _ = array_unsafe_set arr i (f i) in ()
    done;
    arr

(** The empty array. Note: each call creates a fresh empty array. *)
let empty () = array_make 0 ()

(** {1 Basic Operations} *)

(** Return the length of the array. O(1). *)
let length arr = array_length arr

(** Return [true] if the array is empty. *)
let is_empty arr = array_length arr == 0

(** Get element at index [i], or [None] if out of bounds. O(1). *)
let get arr i =
  if i < 0 || i >= array_length arr then None
  else Some (array_unsafe_get arr i)

(** Get element at index [i], raising on out of bounds. O(1). *)
let get_exn arr i =
  if i < 0 || i >= array_length arr then
    error "Array.get_exn: index out of bounds"
  else
    array_unsafe_get arr i

(** Set element at index [i] to [v]. Does nothing if out of bounds. O(1). *)
let set arr i v =
  if i >= 0 && i < array_length arr then
    array_unsafe_set arr i v
  else
    ()

(** Set element at index [i] to [v], raising on out of bounds. O(1). *)
let set_exn arr i v =
  if i < 0 || i >= array_length arr then
    error "Array.set_exn: index out of bounds"
  else
    array_unsafe_set arr i v

(** {1 Transformations} *)

(** Create a new array by applying [f] to each element. *)
let map f arr =
  let len = array_length arr in
  if len == 0 then array_empty ()
  else
    let result = array_make len (f (array_unsafe_get arr 0)) in
    for i = 1 to len - 1 do
      let _ = array_unsafe_set result i (f (array_unsafe_get arr i)) in ()
    done;
    result

(** Create a new array by applying [f] to each element with its index. *)
let mapi f arr =
  let len = array_length arr in
  if len == 0 then array_empty ()
  else
    let result = array_make len (f 0 (array_unsafe_get arr 0)) in
    for i = 1 to len - 1 do
      let _ = array_unsafe_set result i (f i (array_unsafe_get arr i)) in ()
    done;
    result

(** Create a copy of the array. *)
let copy arr = map (fun x -> x) arr

(** {1 Folding} *)

(** Left fold: [fold_left f init arr] applies [f] to each element from left to right. *)
let fold_left f acc arr =
  let len = array_length arr in
  let result = ref acc in
  for i = 0 to len - 1 do
    result := f !result (array_unsafe_get arr i)
  done;
  !result

(** Right fold: [fold_right f arr init] applies [f] to each element from right to left. *)
let fold_right f arr acc =
  let len = array_length arr in
  let result = ref acc in
  for i = 0 to len - 1 do
    let idx = len - 1 - i in
    result := f (array_unsafe_get arr idx) !result
  done;
  !result

(** {1 Iteration} *)

(** Apply [f] to each element for side effects. *)
let iter f arr =
  let len = array_length arr in
  for i = 0 to len - 1 do
    let _ = f (array_unsafe_get arr i) in ()
  done

(** Apply [f] to each element with its index for side effects. *)
let iteri f arr =
  let len = array_length arr in
  for i = 0 to len - 1 do
    let _ = f i (array_unsafe_get arr i) in ()
  done

(** {1 Searching} *)

(** Return [true] if any element satisfies [predicate]. *)
let exists predicate arr =
  let len = array_length arr in
  let found = ref false in
  let i = ref 0 in
  while !i < len && not !found do
    if predicate (array_unsafe_get arr !i) then
      found := true
    else
      i := !i + 1
  done;
  !found

(** Return [true] if all elements satisfy [predicate]. *)
let for_all predicate arr =
  let len = array_length arr in
  let ok = ref true in
  let i = ref 0 in
  while !i < len && !ok do
    if not (predicate (array_unsafe_get arr !i)) then
      ok := false
    else
      i := !i + 1
  done;
  !ok

(** Find the first element satisfying [predicate], or [None]. *)
let find predicate arr =
  let len = array_length arr in
  let result = ref None in
  let i = ref 0 in
  while !i < len && Option.is_none !result do
    let elem = array_unsafe_get arr !i in
    if predicate elem then
      result := Some elem
    else
      i := !i + 1
  done;
  !result

(** Find the index of the first element satisfying [predicate], or [None]. *)
let find_index predicate arr =
  let len = array_length arr in
  let result = ref None in
  let i = ref 0 in
  while !i < len && Option.is_none !result do
    if predicate (array_unsafe_get arr !i) then
      result := Some !i
    else
      i := !i + 1
  done;
  !result

(** Return [true] if [element] is in the array. Uses [==] for comparison. *)
let mem element arr = exists (fun x -> x == element) arr

(** {1 Conversion} *)

(** Create an array from a list. *)
let of_list lst =
  match lst with
  | Nil -> array_empty ()
  | Cons (first, _) ->
      let len = List.length lst in
      let arr = array_make len first in
      let _ = List.fold_left (fun i x ->
        let _ = array_unsafe_set arr i x in
        i + 1
      ) 0 lst in
      arr

(** Convert an array to a list. *)
let to_list arr =
  fold_right (fun x acc -> Cons (x, acc)) arr Nil

(** {1 Comparison} *)

(** Compare two arrays element by element using [cmp].
    Returns negative if [arr1 < arr2], 0 if equal, positive if [arr1 > arr2]. *)
let compare cmp arr1 arr2 =
  let len1 = array_length arr1 in
  let len2 = array_length arr2 in
  let min_len = if len1 < len2 then len1 else len2 in
  let result = ref 0 in
  let i = ref 0 in
  while !i < min_len && !result == 0 do
    result := cmp (array_unsafe_get arr1 !i) (array_unsafe_get arr2 !i);
    i := !i + 1
  done;
  if !result != 0 then !result
  else if len1 < len2 then 0 - 1
  else if len1 > len2 then 1
  else 0

(** Test equality of two arrays using [eq] for elements. *)
let equal eq arr1 arr2 =
  let len1 = array_length arr1 in
  let len2 = array_length arr2 in
  if len1 != len2 then false
  else
    let ok = ref true in
    let i = ref 0 in
    while !i < len1 && !ok do
      if not (eq (array_unsafe_get arr1 !i) (array_unsafe_get arr2 !i)) then
        ok := false
      else
        i := !i + 1
    done;
    !ok

(** {1 Filtering} *)

(** [filter predicate arr] returns new array with elements satisfying [predicate]. *)
let filter predicate arr =
  let len = length arr in
  if len == 0 then empty ()
  else
    let result = ref [] in
    let _ = iteri (fun index elem ->
      if predicate elem then result := elem :: !result
      else ()
    ) arr in
    of_list (List.reverse !result)

(** [filter_map f arr] applies [f] to each element, keeping [Some] results. *)
let filter_map f arr =
  let len = length arr in
  if len == 0 then empty ()
  else
    let result = ref [] in
    let _ = iteri (fun index elem ->
      match f elem with
      | Some y -> result := y :: !result
      | None -> ()
    ) arr in
    of_list (List.reverse !result)

(** {1 Stack Operations} *)

(** [push arr elem] returns a new array with [elem] added at the end.
    Does not mutate the original array. O(n). *)
let push arr elem =
  let len = length arr in
  let new_arr = make (len + 1) elem in
  let _ = iteri (fun index x -> set_exn new_arr index x) arr in
  new_arr

(** [pop arr] returns [Some (last_element, new_array)] where [new_array] has
    the last element removed. Returns [None] if array is empty.
    Does not mutate the original array. O(n). *)
let pop arr =
  let len = length arr in
  if len == 0 then None
  else
    let last_elem = get_exn arr (len - 1) in
    let new_arr = init (len - 1) (fun index -> get_exn arr index) in
    Some (last_elem, new_arr)

(** {1 In-Place Operations} *)

(** [reverse_in_place arr] reverses array in place. Mutates [arr]. *)
let reverse_in_place arr =
  let len = length arr in
  let half = len / 2 in
  let rec swap i =
    if i >= half then ()
    else
      let j = len - 1 - i in
      let temp = get_exn arr i in
      let _ = set_exn arr i (get_exn arr j) in
      let _ = set_exn arr j temp in
      swap (i + 1)
  in
  swap 0

(** [sort_in_place cmp arr] sorts array in place using [cmp]. Mutates [arr].
    [cmp a b] should return negative if [a < b], 0 if equal, positive if [a > b]. *)
let sort_in_place cmp arr =
  let swap i j =
    let temp = get_exn arr i in
    let _ = set_exn arr i (get_exn arr j) in
    set_exn arr j temp
  in

  let rec partition low high =
    let pivot = get_exn arr high in
    let i = ref (low - 1) in
    let rec loop j =
      if j >= high then !i + 1
      else
        let _ = if cmp (get_exn arr j) pivot <= 0 then
          let _ = i := !i + 1 in
          swap !i j
        else () in
        loop (j + 1)
    in
    let pi = loop low in
    let _ = swap pi high in
    pi
  in

  let rec quicksort low high =
    if low < high then
      let pi = partition low high in
      let _ = quicksort low (pi - 1) in
      quicksort (pi + 1) high
    else ()
  in

  let len = length arr in
  if len > 1 then quicksort 0 (len - 1) else ()

(* FFI bindings to Lua table functions - 1-indexed *)
@val @scope("table")
external table_insert_raw : 'a array -> int -> 'a -> unit = "insert"

@val @scope("table") @return(nullable)
external table_remove_raw : 'a array -> int -> 'a option = "remove"

(** [insert_in_place arr index elem] inserts [elem] at position [index] (0-indexed),
    shifting subsequent elements to the right. Mutates [arr] in place.
    Valid indices: [0] to [length arr] (inclusive for append).
    Does nothing if [index] is out of bounds. O(n) worst case. *)
let insert_in_place arr index elem =
  let len = length arr in
  if index < 0 || index > len then ()
  else table_insert_raw arr (index + 1) elem

(** [insert_in_place_exn arr index elem] inserts [elem] at position [index].
    Raises if [index] is outside [0, length arr]. *)
let insert_in_place_exn arr index elem =
  let len = length arr in
  if index < 0 || index > len then
    error "Array.insert_in_place_exn: index out of bounds"
  else
    table_insert_raw arr (index + 1) elem

(** [remove_in_place arr index] removes and returns element at [index] (0-indexed).
    Returns [None] if [index] is out of bounds. O(n) worst case. *)
let remove_in_place arr index =
  let len = length arr in
  if index < 0 || index >= len then None
  else table_remove_raw arr (index + 1)

(** [remove_in_place_exn arr index] removes and returns element at [index].
    Raises if [index] is out of bounds. *)
let remove_in_place_exn arr index =
  let len = length arr in
  if index < 0 || index >= len then
    error "Array.remove_in_place_exn: index out of bounds"
  else
    match table_remove_raw arr (index + 1) with
    | Some v -> v
    | None -> error "Array.remove_in_place_exn: unexpected nil from table.remove"
|}

let dict_source = {|
(** Dict utilities for immutable key-value dictionaries.

    Dictionaries map keys to values. Operations are immutable:
    [set] and [remove] return new dictionaries.

    Keys can be any comparable type (strings, integers, booleans).
    The dict type is built-in: [('k, 'v) dict] *)

(** {1 Construction} *)

(** The empty dictionary. *)
let empty () = dict_empty ()

(** Create a dictionary with a single binding. *)
let singleton key value = dict_set key value (dict_empty ())

(** {1 Querying} *)

(** Get the value for [key], or [None] if not found. O(1) average. *)
let get key dict = dict_get key dict

(** Get the value for [key], or [default] if not found. *)
let get_or key default dict =
  Option.get_or (dict_get key dict) default

(** Return [true] if [key] exists in the dictionary. *)
let has key dict = dict_has key dict

(** Return the number of bindings. O(n). *)
let size dict = dict_size dict

(** Return [true] if the dictionary is empty. *)
let is_empty dict = dict_size dict == 0

(** {1 Modifying} *)

(** Set [key] to [value], returning a new dictionary.
    Overwrites existing binding if present. O(n). *)
let set key value dict = dict_set key value dict

(** Remove [key] from the dictionary, returning a new dictionary.
    No effect if [key] is not present. O(n). *)
let remove key dict = dict_remove key dict

(** {1 Accessing Collections} *)

(** Return all keys as a list. Order is not guaranteed. *)
let keys dict = dict_keys dict

(** Return all values as a list. Order is not guaranteed. *)
let values dict = List.map (fun pair -> match pair with (_, v) -> v) (dict_entries dict)

(** Return all (key, value) pairs as a list. Order is not guaranteed. *)
let entries dict = dict_entries dict

(** {1 Transformations} *)

(** Apply [f] to each value, returning a new dictionary with same keys. *)
let map f dict =
  List.fold_left (fun acc pair ->
    match pair with (k, v) -> dict_set k (f v) acc
  ) (dict_empty ()) (dict_entries dict)

(** Apply [f] to each key-value pair, returning a new dictionary with same keys. *)
let mapi f dict =
  List.fold_left (fun acc pair ->
    match pair with (k, v) -> dict_set k (f k v) acc
  ) (dict_empty ()) (dict_entries dict)

(** Keep only bindings where [predicate key value] returns [true]. *)
let filter predicate dict =
  List.fold_left (fun acc pair ->
    match pair with (k, v) ->
      if predicate k v then dict_set k v acc else acc
  ) (dict_empty ()) (dict_entries dict)

(** Apply [f] to each key-value pair, keeping [Some] results. *)
let filter_map f dict =
  List.fold_left (fun acc pair ->
    match pair with (k, v) ->
      match f k v with
      | None -> acc
      | Some new_v -> dict_set k new_v acc
  ) (dict_empty ()) (dict_entries dict)

(** {1 Folding} *)

(** Fold over all bindings. [fold f dict init] applies [f key value acc]
    for each binding. Order is not guaranteed. *)
let fold f dict init =
  List.fold_left (fun acc pair ->
    match pair with (k, v) -> f k v acc
  ) init (dict_entries dict)

(** {1 Iteration} *)

(** Apply [f] to each binding for side effects. Order is not guaranteed. *)
let iter f dict =
  List.iter (fun pair -> match pair with (k, v) -> f k v) (dict_entries dict)

(** {1 Merging} *)

(** Merge two dictionaries. Bindings in [dict2] override those in [dict1]. *)
let merge dict1 dict2 =
  List.fold_left (fun acc pair ->
    match pair with (k, v) -> dict_set k v acc
  ) dict1 (dict_entries dict2)

(** {1 Conversion} *)

(** Create a dictionary from a list of (key, value) pairs.
    Later bindings override earlier ones for duplicate keys. *)
let of_list items =
  List.fold_left (fun acc pair ->
    match pair with (k, v) -> dict_set k v acc
  ) (dict_empty ()) items

(** Convert to a list of (key, value) pairs. *)
let to_list dict = dict_entries dict

(** {1 Comparison} *)

(** Test equality using [eq] for values.
    Two dictionaries are equal if they have the same keys with equal values. *)
let equal eq dict1 dict2 =
  if dict_size dict1 != dict_size dict2 then false
  else
    List.for_all (fun pair ->
      match pair with (k, v1) ->
        match dict_get k dict2 with
        | None -> false
        | Some v2 -> eq v1 v2
    ) (dict_entries dict1)

(** {1 Finding} *)

(** Find the first binding satisfying [predicate], or [None]. *)
let find predicate dict =
  List.find (fun pair -> match pair with (k, v) -> predicate k v) (dict_entries dict)

(** Return [true] if any binding satisfies [predicate]. *)
let exists predicate dict =
  List.exists (fun pair -> match pair with (k, v) -> predicate k v) (dict_entries dict)

(** Return [true] if all bindings satisfy [predicate]. *)
let for_all predicate dict =
  List.for_all (fun pair -> match pair with (k, v) -> predicate k v) (dict_entries dict)
|}

let tuple_source = {|
(** Tuple utilities for pairs (2-tuples).

    A pair [(a, b)] is a product type containing two values of potentially
    different types. This module provides functions for accessing, transforming,
    and comparing pairs without requiring pattern matching. *)

(** {1 Construction} *)

(** [make a b] creates a pair [(a, b)]. *)
let make a b = (a, b)

(** {1 Accessors} *)

(** [fst pair] returns the first element of [pair]. *)
let fst pair = match pair with (a, _) -> a

(** [snd pair] returns the second element of [pair]. *)
let snd pair = match pair with (_, b) -> b

(** {1 Transforming} *)

(** [swap pair] exchanges the elements: [(a, b)] becomes [(b, a)]. *)
let swap pair = match pair with (a, b) -> (b, a)

(** [map_fst f pair] applies [f] to the first element.
    [map_fst f (a, b)] is [(f a, b)]. *)
let map_fst f pair = match pair with (a, b) -> (f a, b)

(** [map_snd f pair] applies [f] to the second element.
    [map_snd f (a, b)] is [(a, f b)]. *)
let map_snd f pair = match pair with (a, b) -> (a, f b)

(** [map f g pair] applies [f] to the first element and [g] to the second.
    [map f g (a, b)] is [(f a, g b)]. *)
let map f g pair = match pair with (a, b) -> (f a, g b)

(** {1 Folding} *)

(** [fold f pair] combines both elements using [f].
    [fold f (a, b)] is [f a b]. *)
let fold f pair = match pair with (a, b) -> f a b

(** {1 Iteration} *)

(** [iter f pair] applies [f] to both elements for side effects.
    Both elements must have the same type. *)
let iter f pair = match pair with
  (a, b) ->
    let _ = f a in
    let _ = f b in
    ()

(** {1 Comparison} *)

(** [equal eq_fst eq_snd p1 p2] returns [true] if pairs are equal.
    Uses [eq_fst] to compare first elements and [eq_snd] for second. *)
let equal eq_fst eq_snd p1 p2 = match (p1, p2) with
  ((a1, b1), (a2, b2)) -> eq_fst a1 a2 && eq_snd b1 b2

(** [compare cmp_fst cmp_snd p1 p2] compares pairs lexicographically.
    First compares first elements; if equal, compares second elements.
    Returns negative if [p1 < p2], 0 if equal, positive if [p1 > p2]. *)
let compare cmp_fst cmp_snd p1 p2 = match (p1, p2) with
  ((a1, b1), (a2, b2)) ->
    let c = cmp_fst a1 a2 in
    if c != 0 then c
    else cmp_snd b1 b2

(** {1 Conversion} *)

(** [to_list pair] converts a pair to a two-element list.
    Both elements must have the same type. *)
let to_list pair = match pair with (a, b) -> [a; b]
|}

let set_source = {|
(** Set utilities for immutable sets of unique values.

    Sets are collections of unique elements. Operations are immutable:
    [add] and [remove] return new sets.

    Elements can be any comparable type (strings, integers, booleans).
    The set type is built-in: ['a set] *)

(** {1 Construction} *)

(** The empty set. *)
let empty () = set_empty ()

(** Create a set with a single element. *)
let singleton elem = set_add elem (set_empty ())

(** {1 Querying} *)

(** Return [true] if [elem] is in the set. O(1) average. *)
let mem elem set = set_mem elem set

(** Alias for [mem]. *)
let has elem set = set_mem elem set

(** Return the number of elements. O(n). *)
let size set = set_size set

(** Return [true] if the set is empty. *)
let is_empty set = set_size set == 0

(** {1 Modifying} *)

(** Add [elem] to the set, returning a new set.
    No effect if [elem] is already present. O(n). *)
let add elem set = set_add elem set

(** Remove [elem] from the set, returning a new set.
    No effect if [elem] is not present. O(n). *)
let remove elem set = set_remove elem set

(** {1 Set Operations} *)

(** Return the union of two sets (elements in either). *)
let union set1 set2 =
  List.fold_left (fun acc elem -> set_add elem acc) set1 (set_elements set2)

(** Return the intersection of two sets (elements in both). *)
let inter set1 set2 =
  List.fold_left (fun acc elem ->
    if set_mem elem set2 then set_add elem acc else acc
  ) (set_empty ()) (set_elements set1)

(** Return the difference of two sets (elements in first but not second). *)
let diff set1 set2 =
  List.fold_left (fun acc elem ->
    if set_mem elem set2 then acc else set_add elem acc
  ) (set_empty ()) (set_elements set1)

(** Return the symmetric difference (elements in exactly one set). *)
let sym_diff set1 set2 =
  let in_only_set1 = diff set1 set2 in
  let in_only_set2 = diff set2 set1 in
  union in_only_set1 in_only_set2

(** {1 Predicates} *)

(** Return [true] if [set1] is a subset of [set2]. *)
let subset set1 set2 =
  List.for_all (fun elem -> set_mem elem set2) (set_elements set1)

(** Return [true] if the sets have no elements in common. *)
let disjoint set1 set2 =
  List.for_all (fun elem -> not (set_mem elem set2)) (set_elements set1)

(** Return [true] if any element satisfies [predicate]. *)
let exists predicate set =
  List.exists predicate (set_elements set)

(** Return [true] if all elements satisfy [predicate]. *)
let for_all predicate set =
  List.for_all predicate (set_elements set)

(** {1 Transformations} *)

(** Apply [f] to each element, returning a new set. *)
let map f set =
  List.fold_left (fun acc elem -> set_add (f elem) acc) (set_empty ()) (set_elements set)

(** Keep only elements satisfying [predicate]. *)
let filter predicate set =
  List.fold_left (fun acc elem ->
    if predicate elem then set_add elem acc else acc
  ) (set_empty ()) (set_elements set)

(** Apply [f] to each element and keep [Some] results. *)
let filter_map f set =
  List.fold_left (fun acc elem ->
    match f elem with
    | None -> acc
    | Some new_elem -> set_add new_elem acc
  ) (set_empty ()) (set_elements set)

(** Partition elements by predicate: [(satisfying, not_satisfying)]. *)
let partition predicate set =
  List.fold_left (fun pair elem ->
    match pair with (yes, no) ->
      if predicate elem then (set_add elem yes, no)
      else (yes, set_add elem no)
  ) (set_empty (), set_empty ()) (set_elements set)

(** {1 Folding} *)

(** Fold over all elements. [fold f set init] applies [f elem acc]
    for each element. Order is not guaranteed. *)
let fold f set init =
  List.fold_left (fun acc elem -> f elem acc) init (set_elements set)

(** {1 Iteration} *)

(** Apply [f] to each element for side effects. Order is not guaranteed. *)
let iter f set =
  List.iter f (set_elements set)

(** {1 Searching} *)

(** Find an element satisfying [predicate], or [None]. *)
let find predicate set =
  List.find predicate (set_elements set)

(** {1 Conversion} *)

(** Return all elements as a list. Order is not guaranteed. *)
let elements set = set_elements set

(** Alias for [elements]. *)
let to_list set = set_elements set

(** Create a set from a list of elements.
    Duplicates are automatically removed. *)
let of_list items =
  List.fold_left (fun acc elem -> set_add elem acc) (set_empty ()) items

(** {1 Comparison} *)

(** Test equality of two sets. *)
let equal set1 set2 =
  if set_size set1 != set_size set2 then false
  else
    List.for_all (fun elem -> set_mem elem set2) (set_elements set1)

(** Compare set sizes. Returns negative if smaller, 0 if equal, positive if larger. *)
let compare set1 set2 =
  let s1 = set_size set1 in
  let s2 = set_size set2 in
  if s1 < s2 then 0 - 1
  else if s1 > s2 then 1
  else 0
|}

let string_source = {|
(** String manipulation (byte-based, like Lua).

    Lua strings are sequences of bytes. All operations work on bytes,
    not Unicode characters. This matches Lua's native string behavior
    and works on all Lua versions including LuaJIT.

    For ASCII text, bytes and characters are the same. For UTF-8 text,
    be aware that multi-byte characters will be treated as multiple bytes.

    Indices are 1-based (Lua convention) unless otherwise noted. *)

(* FFI declarations must come first *)

@send
external string_len : string -> int = "len"

@val @scope("string")
external string_sub : string -> int -> int -> string = "sub"

@send
external string_upper : string -> string = "upper"

@send
external string_lower : string -> string = "lower"

@val @scope("string")
external string_rep : string -> int -> string = "rep"

@val @scope("string")
external string_byte : string -> int -> int = "byte"

@val @scope("string")
external string_char : int -> string = "char"

@val @scope("string")
external string_gsub_raw : string -> string -> string -> string = "gsub"

@send @return(nullable)
external string_match_raw : string -> string -> string option = "match"

@send @return(nullable)
external string_find_start : string -> string -> int option = "find"

@val @scope("string")
external string_reverse : string -> string = "reverse"

@val @scope("table")
external table_concat : string array -> string -> string = "concat"

(** {1 Basic Operations} *)

(** Return the length of the string in bytes. *)
let length str = string_len str

(** Return [true] if string is empty. *)
let is_empty str = string_len str == 0

(** Get substring from byte index [start] to [stop] (inclusive, 1-based).
    Supports negative indices: -1 is the last byte, -2 is second to last, etc. *)
let sub str start stop = string_sub str start stop

(** Get the byte value at position [index] (1-based), or [None] if out of bounds.
    Returns the numeric byte value (0-255). *)
let get str index =
  if index < 1 || index > string_len str then None
  else Some (string_byte str index)

(** Get the byte value at position [index] (1-based), raising on out of bounds. *)
let get_exn str index =
  if index < 1 || index > string_len str then
    error "String.get_exn: index out of bounds"
  else
    string_byte str index

(** {1 Case Conversion} *)

(** Convert to uppercase (ASCII only). *)
let upper str = string_upper str

(** Convert to lowercase (ASCII only). *)
let lower str = string_lower str

(** Capitalize the first character (ASCII only). *)
let capitalize str =
  if string_len str == 0 then str
  else
    let first = string_upper (string_sub str 1 1) in
    let rest = string_sub str 2 (string_len str) in
    first ^ rest

(** Uncapitalize the first character (ASCII only). *)
let uncapitalize str =
  if string_len str == 0 then str
  else
    let first = string_lower (string_sub str 1 1) in
    let rest = string_sub str 2 (string_len str) in
    first ^ rest

(** {1 Building} *)

(** Repeat string [n] times. Returns empty string if [n <= 0]. *)
let rep str n =
  if n <= 0 then "" else string_rep str n

(** Create a string of [n] copies of byte value [byte]. *)
let make n byte =
  if n <= 0 then ""
  else string_rep (string_char byte) n

(** Join a list of strings with separator. *)
let join sep strings =
  let arr = Array.of_list strings in
  table_concat arr sep

(** Concatenate two strings. *)
let concat str1 str2 = str1 ^ str2

(** Reverse the string (by bytes). *)
let reverse str = string_reverse str

(** {1 Searching} *)

(** Return [true] if [pattern] is found in [str].
    Uses Lua pattern syntax (not regex). *)
let find str pattern =
  Option.is_some (string_find_start str pattern)

(** Return [true] if [str] contains [substring] (literal match, no patterns). *)
let contains str substring =
  let escaped = string_gsub_raw substring "([%^%$%(%)%%%.%[%]%*%+%-%?])" "%%%1" in
  Option.is_some (string_find_start str escaped)

(** Match pattern and return the first match, or [None] if not found.
    Uses Lua pattern syntax. *)
let match_ str pattern = string_match_raw str pattern

(** Global substitution: replace all matches of [pattern] with [replacement].
    Uses Lua pattern syntax. *)
let gsub str pattern replacement = string_gsub_raw str pattern replacement

(** {1 Predicates} *)

(** Return [true] if string starts with [prefix]. *)
let starts_with str prefix =
  let prefix_len = string_len prefix in
  let str_len = string_len str in
  if prefix_len > str_len then false
  else string_sub str 1 prefix_len == prefix

(** Return [true] if string ends with [suffix]. *)
let ends_with str suffix =
  let suffix_len = string_len suffix in
  let str_len = string_len str in
  if suffix_len > str_len then false
  else string_sub str (str_len - suffix_len + 1) str_len == suffix

(** {1 Trimming} *)

(** Remove leading and trailing whitespace. *)
let trim str =
  let trimmed = string_gsub_raw str "^%s+" "" in
  string_gsub_raw trimmed "%s+$" ""

(** Remove leading whitespace. *)
let trim_start str = string_gsub_raw str "^%s+" ""

(** Remove trailing whitespace. *)
let trim_end str = string_gsub_raw str "%s+$" ""

(** {1 Splitting} *)

(** Split string by literal separator.
    Returns list of substrings. Empty separator returns the original string. *)
let split str sep =
  if is_empty str then [str]
  else if is_empty sep then [str]
  else
    let sep_len = string_len sep in
    let str_len = string_len str in
    let escaped_sep = string_gsub_raw sep "([%^%$%(%)%%%.%[%]%*%+%-%?])" "%%%1" in
    let rec go acc pos =
      if pos > str_len then List.reverse acc
      else
        match string_match_raw (string_sub str pos str_len) ("^(.-)" ^ escaped_sep) with
        | None ->
            let remaining = string_sub str pos str_len in
            List.reverse (Cons (remaining, acc))
        | Some part ->
            let new_pos = pos + string_len part + sep_len in
            go (Cons (part, acc)) new_pos
    in
    go Nil 1

(** Split string into lines (by newline character). *)
let lines str = split str "\n"

(** {1 Byte Conversion} *)

(** Convert string to list of byte values (0-255). *)
let to_bytes str =
  let len = string_len str in
  List.init len (fun index -> string_byte str (index + 1))

(** Create string from list of byte values (0-255). *)
let of_bytes bytes =
  List.fold_left (fun acc byte -> acc ^ string_char byte) "" bytes

(** Get single character string from byte value. *)
let of_byte byte = string_char byte

(** {1 Comparison} *)

(** Compare two strings lexicographically.
    Returns negative if [str1 < str2], 0 if equal, positive if [str1 > str2].
    Use [Ord.string_compare] for ordering type result. *)
let compare str1 str2 =
  if str1 < str2 then 0 - 1
  else if str1 > str2 then 1
  else 0

(** Test equality of two strings. *)
let equal str1 str2 = str1 == str2

(** {1 Iteration} *)

(** Apply [f] to each byte value. *)
let iter f str =
  let len = string_len str in
  for index = 1 to len do
    let _ = f (string_byte str index) in ()
  done

(** Apply [f] to each byte with its index (1-based). *)
let iteri f str =
  let len = string_len str in
  for index = 1 to len do
    let _ = f index (string_byte str index) in ()
  done

(** Fold over bytes from left to right. *)
let fold_left f init str =
  let len = string_len str in
  let acc = ref init in
  for index = 1 to len do
    acc := f !acc (string_byte str index)
  done;
  !acc

(** Return [true] if all bytes satisfy [predicate]. *)
let for_all predicate str =
  let len = string_len str in
  let result = ref true in
  let index = ref 1 in
  while !index <= len && !result do
    if not (predicate (string_byte str !index)) then
      result := false
    else
      index := !index + 1
  done;
  !result

(** Return [true] if any byte satisfies [predicate]. *)
let exists predicate str =
  let len = string_len str in
  let result = ref false in
  let index = ref 1 in
  while !index <= len && not !result do
    if predicate (string_byte str !index) then
      result := true
    else
      index := !index + 1
  done;
  !result
|}

let math_source = {|
(** Mathematical functions and constants.

    Provides bindings to Lua's math library plus additional utilities.
    All trigonometric functions work in radians. *)

(* {1 FFI Declarations - Rounding} *)

@val @scope("math")
external floor_raw : float -> float = "floor"

@val @scope("math")
external ceil_raw : float -> float = "ceil"

(* {1 FFI Declarations - Arithmetic} *)

@val @scope("math")
external abs : float -> float = "abs"

@val @scope("math")
external fmod : float -> float -> float = "fmod"

@val @scope("math")
external min_raw : float -> float -> float = "min"

@val @scope("math")
external max_raw : float -> float -> float = "max"

(* {1 FFI Declarations - Exponential & Logarithmic} *)

@val @scope("math")
external exp : float -> float = "exp"

@val @scope("math")
external log : float -> float = "log"

@val @scope("math")
external sqrt : float -> float = "sqrt"

@val @scope("math")
external pow : float -> float -> float = "pow"

(* {1 FFI Declarations - Trigonometric} *)

@val @scope("math")
external sin : float -> float = "sin"

@val @scope("math")
external cos : float -> float = "cos"

@val @scope("math")
external tan : float -> float = "tan"

@val @scope("math")
external asin : float -> float = "asin"

@val @scope("math")
external acos : float -> float = "acos"

@val @scope("math")
external atan : float -> float = "atan"

@val @scope("math")
external atan2 : float -> float -> float = "atan2"

(* {1 FFI Declarations - Angle Conversion} *)

@val @scope("math")
external rad : float -> float = "rad"

@val @scope("math")
external deg : float -> float = "deg"

(* {1 FFI Declarations - Random} *)

@val @scope("math")
external random_unit : unit -> float = "random"

@val @scope("math")
external random_max : int -> int = "random"

@val @scope("math")
external random_range_raw : int -> int -> int = "random"

@val @scope("math")
external randomseed : int -> unit = "randomseed"

(* {1 Constants} *)

(** The mathematical constant pi. *)
let pi = 3.141592653589793

(** Positive infinity. *)
let huge = 1.0 /. 0.0

(* {1 Rounding Functions} *)

(** Round down to nearest integer. *)
let floor x = floor_raw x

(** Round up to nearest integer. *)
let ceil x = ceil_raw x

(** Round to nearest integer (half away from zero). *)
let round x =
  if x >= 0.0 then floor_raw (x +. 0.5)
  else ceil_raw (x -. 0.5)

(** Truncate toward zero (drop fractional part). *)
let trunc x =
  if x >= 0.0 then floor_raw x
  else ceil_raw x

(* {1 Arithmetic Functions} *)

(** Absolute value for integers. *)
let abs_int n =
  if n < 0 then 0 - n else n

(** Minimum of two floats. *)
let min a b = min_raw a b

(** Maximum of two floats. *)
let max a b = max_raw a b

(** Minimum of two integers. *)
let min_int a b =
  if a < b then a else b

(** Maximum of two integers. *)
let max_int a b =
  if a > b then a else b

(** Split float into integer and fractional parts.
    Returns [(int_part, frac_part)] where the fractional part
    has the same sign as the input. *)
let modf x =
  let int_part = trunc x in
  let frac_part = x -. int_part in
  (int_part, frac_part)

(* {1 Exponential & Logarithmic} *)

(** Base-10 logarithm. *)
let log10 x = log x /. log 10.0

(* {1 Random Numbers} *)

(** Random float in [0, 1). *)
let random () = random_unit ()

(** Random integer in [1, n]. *)
let random_int n = random_max n

(** Random integer in [m, n] (inclusive). *)
let random_range m n = random_range_raw m n
|}

let io_source = {|
(** File I/O operations.

    Provides bindings to Lua's io library for file operations.
    File handles are opaque types managed by the runtime. *)

(** File handle (opaque type). *)
type file

(* {1 FFI Declarations - Standard Streams} *)

@val @scope("io")
external stdin : file = "stdin"

@val @scope("io")
external stdout : file = "stdout"

@val @scope("io")
external stderr : file = "stderr"

(* {1 FFI Declarations - File Operations} *)

@val @scope("io") @return(nullable)
external open_raw : string -> string -> file option = "open"

@send
external close : file -> unit = "close"

@send
external flush : file -> unit = "flush"

(* {1 FFI Declarations - Reading} *)

@send @return(nullable)
external read_line_raw : file -> string option = "read"

@send
external read_all_raw : file -> string -> string = "read"

@send @return(nullable)
external read_bytes_raw : file -> int -> string option = "read"

(* {1 FFI Declarations - Writing} *)

@send
external write_raw : file -> string -> unit = "write"

(* {1 FFI Declarations - Positioning} *)

@send @return(nullable)
external seek_raw : file -> string -> int -> int option = "seek"

@send
external seek_cur : file -> int = "seek"

(* {1 File Operations} *)

(** Open a file with the given mode.
    Modes: "r" (read), "w" (write), "a" (append), "r+", "w+", "a+"
    Returns [None] if the file cannot be opened. *)
let open_ path mode = open_raw path mode

(** Open a file or raise an error if it cannot be opened. *)
let open_exn path mode =
  match open_raw path mode with
  | Some handle -> handle
  | None -> error ("Io.open_exn: cannot open file: " ^ path)

(* {1 Reading} *)

(** Read a single line from the file, or [None] if at end of file.
    The newline character is not included. *)
let read_line handle = read_line_raw handle

(** Read the entire contents of the file as a string. *)
let read_all handle = read_all_raw handle "*a"

(** Read up to [count] bytes from the file.
    Returns [None] if at end of file. *)
let read_bytes handle count = read_bytes_raw handle count

(* {1 Writing} *)

(** Write a string to the file. *)
let write handle content = write_raw handle content

(** Write a string followed by a newline to the file. *)
let write_line handle content =
  let _ = write_raw handle content in
  write_raw handle "\n"

(* {1 Positioning} *)

(** Set the file position.
    [whence] can be: "set" (from beginning), "cur" (from current), "end" (from end)
    Returns the new position or [None] on error. *)
let seek handle whence offset = seek_raw handle whence offset

(** Return the current file position. *)
let tell handle = seek_cur handle

(* {1 Convenience Functions} *)

(** Read the entire contents of a file by path.
    Opens, reads, and closes the file automatically. *)
let read_file path =
  match open_raw path "r" with
  | None -> Error ("Cannot open file: " ^ path)
  | Some handle ->
      let content = read_all_raw handle "*a" in
      let _ = close handle in
      Ok content

(** Write a string to a file by path.
    Creates the file if it doesn't exist, overwrites if it does. *)
let write_file path content =
  match open_raw path "w" with
  | None -> Error ("Cannot open file for writing: " ^ path)
  | Some handle ->
      let _ = write_raw handle content in
      let _ = close handle in
      Ok ()

(** Append a string to a file by path.
    Creates the file if it doesn't exist. *)
let append_file path content =
  match open_raw path "a" with
  | None -> Error ("Cannot open file for appending: " ^ path)
  | Some handle ->
      let _ = write_raw handle content in
      let _ = close handle in
      Ok ()

(** Open a file, apply an action, and close it automatically.
    Returns [Ok result] if successful, [Error msg] if the file couldn't be opened. *)
let with_file path mode action =
  match open_raw path mode with
  | None -> Error ("Cannot open file: " ^ path)
  | Some handle ->
      let result = action handle in
      let _ = close handle in
      Ok result
|}

let os_source = {|
(** Operating system facilities.

    Provides bindings to Lua's os library for date/time,
    environment, filesystem, and process control.
    Targets LuaJIT / Lua 5.1 semantics. *)

(* {1 FFI Declarations - Date and Time} *)

@val @scope("os")
external time_raw : unit -> int = "time"

@val @scope("os")
external clock : unit -> float = "clock"

@val @scope("os")
external difftime : int -> int -> int = "difftime"

@val @scope("os")
external date_raw : string -> string = "date"

@val @scope("os")
external date_of_raw : string -> int -> string = "date"

(* {1 FFI Declarations - Environment} *)

@val @scope("os") @return(nullable)
external getenv_raw : string -> string option = "getenv"

(* {1 FFI Declarations - File System}

   Note: os.remove and os.rename return true on success, nil on failure.
   The error message is not captured due to FFI limitations. *)

@val @scope("os") @return(nullable)
external remove_raw : string -> bool option = "remove"

@val @scope("os") @return(nullable)
external rename_raw : string -> string -> bool option = "rename"

@val @scope("os")
external tmpname : unit -> string = "tmpname"

(* {1 FFI Declarations - Process Control}

   LuaJIT: os.execute returns (true, "exit", code) on success,
   or (nil, "exit"|"signal", code) on failure.
   We capture only the first return value (bool or nil). *)

@val @scope("os") @return(nullable)
external execute_raw : string -> bool option = "execute"

@val @scope("os")
external exit_raw : int -> unit = "exit"

(* {1 Date and Time Functions} *)

(** Return the current Unix timestamp (seconds since epoch). *)
let time () = time_raw ()

(** Return the difference between two timestamps in seconds.
    [difftime t2 t1] returns [t2 - t1]. *)
(* difftime is already an FFI binding *)

(** Format the current time using a format string.
    Format specifiers follow Lua/C conventions:
    - %Y: 4-digit year, %m: month (01-12), %d: day (01-31)
    - %H: hour (00-23), %M: minute (00-59), %S: second (00-59)
    - %a: abbreviated weekday, %A: full weekday
    - %b: abbreviated month, %B: full month
    - %c: date and time, %x: date, %X: time
    Note: The "*t" table format is NOT supported. *)
let date format = date_raw format

(** Format a given timestamp using a format string. *)
let date_of format timestamp = date_of_raw format timestamp

(* {1 Environment Functions} *)

(** Get the value of an environment variable, or None if not set. *)
let getenv name = getenv_raw name

(* {1 File System Functions} *)

(** Delete a file.
    Returns Ok () on success, Error message on failure.
    Note: Specific error message not available due to FFI limitation. *)
let remove path =
  match remove_raw path with
  | Some _ -> Ok ()
  | None -> Error ("Cannot remove file: " ^ path)

(** Rename or move a file.
    Returns Ok () on success, Error message on failure.
    Note: Specific error message not available due to FFI limitation. *)
let rename old_path new_path =
  match rename_raw old_path new_path with
  | Some _ -> Ok ()
  | None -> Error ("Cannot rename: " ^ old_path ^ " to " ^ new_path)

(* {1 Process Control Functions} *)

(** Execute a shell command.
    Returns [true] if the command succeeded (exit code 0),
    [false] if it failed or could not be executed. *)
let execute command =
  match execute_raw command with
  | Some true -> true
  | _ -> false

(** Exit the program with the given exit code.
    This function never returns. Code 0 indicates success. *)
let exit code = exit_raw code
|}

let coroutine_source = {|
(** Coroutines for cooperative multitasking.

    Lua coroutines provide cooperative multitasking where execution can be
    suspended with yield and resumed later. Each coroutine has its own stack.

    {1 Simplified Type Model}

    This module uses a simplified type model where ['a thread] represents
    a coroutine that yields and returns values of type ['a]. The full Lua
    coroutine model supports bidirectional communication (passing values
    through resume/yield), which is not exposed in this simplified API.

    {1 Limitations}

    - Coroutine bodies take [unit], not arguments (use closures to capture state)
    - Values cannot be passed through [resume] to the coroutine
    - [wrap] raises on error instead of returning Result
    - [is_yieldable] is not available (Lua 5.3+ only)

    {1 Example}

    {[
      (* Generator pattern *)
      let numbers = Coroutine.create (fun () ->
        let _ = Coroutine.yield 1 in
        let _ = Coroutine.yield 2 in
        3
      ) in
      (* Coroutine.resume numbers returns Ok 1, then Ok 2, then Ok 3 *)
    ]} *)

(** Coroutine handle (opaque type).
    The type parameter ['a] is the type of values yielded and returned. *)
type 'a thread

(** Wrapped coroutine as a callable (opaque type). *)
type 'a generator

(** Coroutine status. *)
type status = Suspended | Running | Normal | Dead

(* FFI Declarations *)

@val @scope("coroutine")
external create : (unit -> 'a) -> 'a thread = "create"

@val @scope("coroutine")
external status_raw : 'a thread -> string = "status"

(** [yield value] suspends the coroutine and returns [value] to the caller.
    When the coroutine is resumed, [yield] returns (the same type is used
    for both the yielded value and the return value due to type simplification). *)
@val @scope("coroutine")
external yield : 'a -> 'a = "yield"

@val @scope("coroutine") @return(nullable)
external running_raw : unit -> 'a thread option = "running"

@val @scope("coroutine")
external wrap_raw : (unit -> 'a) -> 'a generator = "wrap"

@val
external call_generator : 'a generator -> 'a = "_lina_call_generator"

(* Resume uses the runtime helper to handle multiple return values *)
@val
external resume_raw : 'a thread -> ('a, string) result = "_lina_coroutine_resume"

(* Public API *)

(** Resume a suspended coroutine.
    Returns [Ok value] with the yielded or returned value,
    or [Error message] if the coroutine raised an error.

    Note: For unit-returning coroutines, [Ok ()] is returned on completion. *)
let resume co = resume_raw co

(** Get the status of a coroutine.
    - [Suspended]: Not yet started or yielded
    - [Running]: Currently executing
    - [Normal]: Resumed another coroutine (rare)
    - [Dead]: Finished execution or errored *)
let status co =
  match status_raw co with
  | "suspended" -> Suspended
  | "running" -> Running
  | "normal" -> Normal
  | _ -> Dead

(** Get the currently running coroutine, or [None] if in main thread. *)
let running () = running_raw ()

(** Create a generator from a coroutine body.
    The generator can be called repeatedly using [next] to get yielded values.

    Warning: Unlike [resume], generators raise errors. Use [resume] for
    error handling. *)
let wrap f = wrap_raw f

(** Get the next value from a generator.
    Each call resumes the underlying coroutine until the next yield or return.
    Raises an error if the generator is exhausted or encounters an error. *)
let next gen = call_generator gen
|}

let debug_source = {|
(** Debug utilities for introspection and error diagnosis.

    Provides bindings to Lua's debug library for stack traces,
    function introspection, and variable inspection.

    {1 Limitations}

    - Only read-only operations are exposed (no setlocal/setupvalue)
    - Stack levels are 1-indexed from the caller's perspective
    - Some fields may not be available for C functions

    {1 Example}

    {[
      (* Get a stack trace *)
      let trace = Debug.traceback () in
      print trace

      (* Inspect a function *)
      match Debug.getinfo 1 with
      | Some info ->
          print ("Function type: " ^ Debug.info_what info)
      | None -> ()
    ]} *)

(** {1 Types} *)

(** Function information (opaque type).
    Returned by getinfo, fields accessed via accessor functions. *)
type info

(** Local variable information. *)
type 'a local_info = { name : string; value : 'a }

(** Upvalue information. *)
type 'a upvalue_info = { name : string; value : 'a }

(** {1 FFI Declarations - Traceback} *)

@val @scope("debug")
external traceback_raw : unit -> string = "traceback"

@val @scope("debug")
external traceback_msg_raw : string -> int -> string = "traceback"

(** {1 FFI Declarations - Function Info} *)

@val @scope("debug") @return(nullable)
external getinfo_raw : int -> string -> info option = "getinfo"

(** {1 FFI Declarations - Info Accessors} *)

(** Source that created the function.
    Prefixed with '@' for files, '=' for C functions. *)
@get
external info_source : info -> string = "source"

(** Short version of source (max 60 chars). *)
@get
external info_short_src : info -> string = "short_src"

(** Line where function definition starts (0 for C functions). *)
@get
external info_linedefined : info -> int = "linedefined"

(** Line where function definition ends. *)
@get
external info_lastlinedefined : info -> int = "lastlinedefined"

(** Function type: "Lua", "C", "main", or "tail". *)
@get
external info_what : info -> string = "what"

(** Current executing line (-1 if not available). *)
@get
external info_currentline : info -> int = "currentline"

(** Number of upvalues. *)
@get
external info_nups : info -> int = "nups"

(** Function name if available (returns empty string if not). *)
@get
external info_name : info -> string = "name"

(** How name was found: "global", "local", "method", "field", or "". *)
@get
external info_namewhat : info -> string = "namewhat"

(** {1 FFI Declarations - Local Variables and Upvalues} *)

(** Runtime helper for getlocal (packages multiple returns). *)
@val @return(nullable)
external getlocal_raw : int -> int -> 'a local_info option = "_lina_debug_getlocal"

(** Runtime helper for getupvalue (packages multiple returns). *)
@val @return(nullable)
external getupvalue_raw : ('a -> 'b) -> int -> 'c upvalue_info option = "_lina_debug_getupvalue"

(** {1 Stack Traceback} *)

(** Get a stack traceback from the current position.
    Returns a string with one line per stack frame. *)
let traceback () = traceback_raw ()

(** Get a stack traceback with a custom message prefix.
    The message is prepended to the traceback output. *)
let traceback_msg msg = traceback_msg_raw msg 1

(** Get a stack traceback starting from a specific level.
    Level 1 is the function calling traceback_from,
    level 2 is its caller, etc. *)
let traceback_from level = traceback_msg_raw "" level

(** {1 Function Introspection} *)

(** Get information about the function at stack level.
    Level 1 is the function calling getinfo,
    level 2 is its caller, etc.
    Returns [None] if the level is invalid. *)
let getinfo level = getinfo_raw level "Slnuf"

(** {1 Variable Inspection} *)

(** Get a local variable at stack level and index.
    Level 1 is the function calling getlocal.
    Index 1 is the first local variable (including parameters).
    Returns [Some {name; value}] or [None] if invalid. *)
let getlocal level index = getlocal_raw level index

(** Get an upvalue from a function closure.
    Index 1 is the first upvalue.
    Returns [Some {name; value}] or [None] if invalid. *)
let getupvalue func index = getupvalue_raw func index
|}

(** Lua runtime helpers for coroutine and debug support.
    These are prepended to the stdlib prelude before any module definitions. *)
let lua_runtime_helpers = {|-- Lina runtime helpers for coroutine support
local function _lina_coroutine_resume(co)
  local ok, result = coroutine.resume(co)
  if ok then
    return {_tag = 0, _0 = result}
  else
    return {_tag = 1, _0 = result}
  end
end

-- Call a generator (wrapped coroutine) to get the next value
local function _lina_call_generator(gen)
  return gen()
end

-- Lina runtime helpers for debug support
-- Get local variable and package as table (nil if invalid)
-- Note: +4 offset accounts for helper + IIFE + curried wrappers
local function _lina_debug_getlocal(level, index)
  local name, value = debug.getlocal(level + 4, index)
  if name == nil then
    return nil
  else
    return {name = name, value = value}
  end
end

-- Get upvalue and package as table (nil if invalid)
local function _lina_debug_getupvalue(func, index)
  local name, value = debug.getupvalue(func, index)
  if name == nil then
    return nil
  else
    return {name = name, value = value}
  end
end
|}

(** All stdlib modules in load order.
    Order matters: modules are loaded sequentially and can only depend on
    modules that appear earlier in this list. *)
let stdlib_modules = [
  { name = "Fn"; source = fn_source };
  { name = "Ord"; source = ord_source };
  { name = "List"; source = list_source };     (* List before Result - Result.sequence uses List.reverse *)
  { name = "Result"; source = result_source };
  { name = "Option"; source = option_source };
  { name = "Array"; source = array_source };
  { name = "Tuple"; source = tuple_source };
  { name = "Dict"; source = dict_source };
  { name = "Set"; source = set_source };
  { name = "String"; source = string_source };
  { name = "Math"; source = math_source };
  { name = "Io"; source = io_source };
  { name = "Os"; source = os_source };
  { name = "Coroutine"; source = coroutine_source };
  { name = "Debug"; source = debug_source };
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

  let module_code = String.concat "\n" (List.rev lua_codes) in
  let lua_prelude = lua_runtime_helpers ^ "\n" ^ module_code in
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
