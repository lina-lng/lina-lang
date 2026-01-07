(** FFI attribute AST types.

    This module defines the surface syntax representation of FFI attributes
    that appear before external declarations. *)

open Common

type attribute_payload =
  | PayloadString of string
  | PayloadStringList of string list
  | PayloadIdent of string
[@@deriving show, eq]

type attribute = {
  attribute_name : string;
  attribute_payload : attribute_payload option;
  attribute_location : Location.t;
}
[@@deriving show, eq]
