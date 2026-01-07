(** FFI attribute AST types.

    This module defines the surface syntax representation of FFI attributes
    that appear before external declarations. These are parsed from source
    and later validated during type checking.

    Attribute syntax:
    - [@val] - attribute without payload
    - [@module("socket")] - attribute with string payload
    - [@scope(("a", "b"))] - attribute with string list payload *)

open Common

(** Payload of an attribute. *)
type attribute_payload =
  | PayloadString of string
      (** Single string argument: [@module("socket")] *)
  | PayloadStringList of string list
      (** Multiple string arguments: [@scope(("table", "insert"))] *)
  | PayloadIdent of string
      (** Identifier argument: [@return(nullable)] *)
[@@deriving show, eq]

(** A single attribute with optional payload. *)
type attribute = {
  attribute_name : string;
      (** The attribute name without [@] prefix *)
  attribute_payload : attribute_payload option;
      (** Optional payload in parentheses *)
  attribute_location : Location.t;
      (** Source location of the attribute *)
}
[@@deriving show, eq]
