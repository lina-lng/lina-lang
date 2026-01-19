(** Test: Type mismatch in array set
    Expected: REJECT

    Cannot set an element of wrong type in an array. *)

let arr = Array.make 3 0
let () = Array.set arr 0 "hello"
