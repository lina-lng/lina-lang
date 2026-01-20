(** Lockfile handling for reproducible builds.

    The lockfile (lina.lock) records exact versions and checksums of
    installed packages to ensure reproducible builds across machines. *)

(** {1 Types} *)

(** A single locked package entry. *)
type entry = {
  name : string;
  version : string;
  checksum : string;  (** MD5 hash of installed files *)
}
[@@deriving show, eq]

(** Complete lockfile. *)
type t = {
  lockfile_version : int;  (** Format version (currently 1) *)
  packages : entry list;
}
[@@deriving show, eq]

(** {1 File Operations} *)

(** [load path] loads a lockfile from disk.
    Returns [Error msg] if the file cannot be read or parsed. *)
val load : string -> (t, string) result

(** [save path lockfile] writes a lockfile to disk.
    Returns [Error msg] if the file cannot be written. *)
val save : string -> t -> (unit, string) result

(** {1 Queries} *)

(** [empty] is an empty lockfile. *)
val empty : t

(** [find_package name lockfile] finds a package entry by name. *)
val find_package : string -> t -> entry option

(** [has_package name lockfile] checks if a package is locked. *)
val has_package : string -> t -> bool

(** {1 Modification} *)

(** [add_package entry lockfile] adds or updates a package entry. *)
val add_package : entry -> t -> t

(** [remove_package name lockfile] removes a package entry. *)
val remove_package : string -> t -> t
