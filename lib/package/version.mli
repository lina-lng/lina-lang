(** Semantic versioning for package dependencies.

    Supports standard semver (1.2.3) and LuaRocks versions (1.2.3-1).
    Provides constraint parsing for dependency specifications. *)

(** {1 Version Type} *)

(** A semantic version with optional LuaRocks revision. *)
type t = {
  major : int;
  minor : int;
  patch : int;
  revision : int option;  (** LuaRocks revision number (e.g., -1 in "1.0.2-1") *)
}
[@@deriving show, eq]

(** {1 Construction} *)

(** [make major minor patch] creates a version. *)
val make : int -> int -> int -> t

(** [make_with_revision major minor patch revision] creates a LuaRocks version. *)
val make_with_revision : int -> int -> int -> int -> t

(** {1 Parsing} *)

(** [parse str] parses a version string like "1.2.3" or "1.2.3-1".

    Returns [Error msg] if the string is not a valid version. *)
val parse : string -> (t, string) result

(** {1 Comparison} *)

(** [compare a b] compares two versions.
    Returns negative if [a < b], zero if [a = b], positive if [a > b]. *)
val compare : t -> t -> int

(** {1 Serialization} *)

(** [to_string version] converts a version to string form. *)
val to_string : t -> string

(** {1 Version Constraints} *)

(** A constraint on acceptable versions. *)
type constraint_ =
  | Any                           (** Any version (no constraint) *)
  | Exact of t                    (** Exactly this version *)
  | GreaterEqual of t             (** >= version *)
  | Greater of t                  (** > version *)
  | LessEqual of t                (** <= version *)
  | Less of t                     (** < version *)
  | Caret of t                    (** ^version (compatible with major) *)
  | Tilde of t                    (** ~version (compatible with minor) *)
  | And of constraint_ * constraint_  (** Both constraints must hold *)
[@@deriving show, eq]

(** [parse_constraint str] parses a constraint string.

    Supported formats:
    - "*" or "" for any version
    - "1.2.3" for exact match
    - ">=1.2.3", ">1.2.3", "<=1.2.3", "<1.2.3" for comparisons
    - "^1.2.3" for caret (compatible with 1.x.x)
    - "~1.2.3" for tilde (compatible with 1.2.x)
    - ">=1.0.0, <2.0.0" for combined constraints *)
val parse_constraint : string -> (constraint_, string) result

(** [satisfies version constraint] checks if [version] satisfies [constraint]. *)
val satisfies : t -> constraint_ -> bool

(** [constraint_to_string c] converts a constraint to string form. *)
val constraint_to_string : constraint_ -> string
