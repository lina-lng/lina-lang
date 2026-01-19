(** Test: Type mismatch using array element
    Expected: REJECT

    Element type must match expected type. *)

let arr = Array.make 3 "hello"
let x : int = Array.get arr 0
