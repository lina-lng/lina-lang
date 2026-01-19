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

(** All stdlib modules in load order.
    Order matters if there are dependencies between modules. *)
let stdlib_modules = [
  { name = "Fn"; source = fn_source };
  { name = "Ord"; source = ord_source };
  { name = "Result"; source = result_source };
  { name = "Option"; source = option_source };
  { name = "List"; source = list_source };
  { name = "Array"; source = array_source };
  { name = "Tuple"; source = tuple_source };
  { name = "Dict"; source = dict_source };
  { name = "Set"; source = set_source };
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
