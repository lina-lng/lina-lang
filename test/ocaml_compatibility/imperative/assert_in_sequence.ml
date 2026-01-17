(* Assert: valid in sequence position *)
(* Expected: ACCEPT - assert followed by expression *)

let test_sequence x =
  let result =
    assert (x >= 0);
    x + 1
  in
  result

let _ = print_int (test_sequence 5)
