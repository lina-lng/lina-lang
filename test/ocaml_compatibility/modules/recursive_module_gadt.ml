(* Recursive module with GADT *)
(* Note: Uses integer-based environment instead of string keys
   for compatibility with Lina's = operator which only works on integers. *)

module rec Expr : sig
  type _ t =
    | Int : int -> int t
    | Add : int t * int t -> int t
    | Var : int -> int t
    | Let : int * int t * 'a t -> 'a t

  val eval : (int -> int) -> 'a t -> 'a
end = struct
  type _ t =
    | Int : int -> int t
    | Add : int t * int t -> int t
    | Var : int -> int t
    | Let : int * int t * 'a t -> 'a t

  let rec eval : type a. (int -> int) -> a t -> a = fun env e ->
    match e with
    | Int n -> n
    | Add (a, b) -> eval env a + eval env b
    | Var x -> env x
    | Let (x, v, body) ->
      let v' = eval env v in
      eval (fun y -> if y = x then v' else env y) body
end

let empty_env _ = 0
let e = Expr.Let (1, Expr.Int 10, Expr.Add (Expr.Var 1, Expr.Int 5))
let () = print_int (Expr.eval empty_env e); print_newline ()
