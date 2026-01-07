(** Singleton constructor registry for code generation.

    See {!Singleton_registry} for documentation. *)

open Common
open Lua_ast

module SingletonKey = struct
  type t = string * int  (* type_name, tag_index *)
  let compare = compare
end

module SingletonSet = Set.Make(SingletonKey)

type t = SingletonSet.t

let empty = SingletonSet.empty

let var_name type_name tag_index =
  Codegen_constants.singleton_var_name type_name tag_index

let register ctx type_name tag_index =
  SingletonSet.add (type_name, tag_index) ctx

let generate_preamble ctx =
  SingletonSet.fold (fun (type_name, tag_index) acc ->
    let name = var_name type_name tag_index in
    let table = ExpressionTable [FieldNamed ("_tag", ExpressionNumber (float_of_int tag_index))] in
    StatementLocal ([name], [table]) :: acc
  ) ctx []
