(** Test: Array.mapi type inference
    Expected: ACCEPT

    Verifies Array.mapi has type (int -> 'a -> 'b) -> 'a array -> 'b array *)

let arr = Array.make 3 10
let indexed = Array.mapi (fun i x -> i + x) arr

let () = print_int (Array.get indexed 0)
let () = print_int (Array.get indexed 2)
