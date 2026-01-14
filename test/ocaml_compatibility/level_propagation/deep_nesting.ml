(* Level propagation: deep nesting works correctly *)
(* Expected: ACCEPT - deeply nested lets generalize correctly *)

let result =
  let a = fun x -> x in
  let b =
    let c = fun y -> y in
    let d =
      let e = fun z -> z in
      (* e is polymorphic at this level *)
      (e 1, e true)
    in
    (* c is polymorphic at this level *)
    (c "hello", d)
  in
  (* a is polymorphic at this level *)
  (a 42, b)

let () = print_int 0
