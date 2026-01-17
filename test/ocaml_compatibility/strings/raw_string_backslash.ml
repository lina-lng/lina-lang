(* Raw string: backslashes preserved literally *)
(* Expected: ACCEPT - backslashes should not be escape sequences *)

let s = {|path\to\file|}
let t = {|C:\Users\name|}
let u = {|\n\t\r|}
let _ = print_endline s
