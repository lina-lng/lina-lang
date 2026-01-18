(** Field location tracking for precise record error reporting.

    This module provides a simple mechanism to track the source locations
    of record field values during inference, so that type errors can point
    to the specific field with the wrong type.

    The tracking uses a global hashtable that should be populated before
    record type unification and consulted during error reporting. *)

open Common

(** Current record field locations: field name -> value expression location *)
let current_field_locations : (string, Location.t) Hashtbl.t = Hashtbl.create 16

(** Register field locations for a record literal being inferred.
    Clears any previous locations and stores the new ones. *)
let register fields =
  Hashtbl.clear current_field_locations;
  fields

(** Add a single field location. *)
let add_field name loc =
  Hashtbl.replace current_field_locations name loc

(** Look up the location of a field value by name.
    Returns [None] if the field is not tracked. *)
let find name =
  Hashtbl.find_opt current_field_locations name

(** Clear all tracked field locations. *)
let clear () =
  Hashtbl.clear current_field_locations
