(* Raw string: custom delimiter *)
(* Expected: ACCEPT - delimited quoted string {id|...|id} *)

let sql = {sql|SELECT * FROM users WHERE name = 'test'|sql}
let lua = {lua|function() print("hello") end|lua}
let x = {a|contains |} inside|a}
let _ = print_endline sql
