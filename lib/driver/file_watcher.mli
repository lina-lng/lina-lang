(** File watching for continuous compilation.

    Monitors source files for changes and triggers rebuilds.
    Uses polling-based watching for cross-platform compatibility. *)

(** {1 Types} *)

(** File state for change detection. *)
type file_state = {
  path : string;
  mtime : float;
}

(** Watcher state. *)
type t

(** {1 Creating Watchers} *)

(** [create ~dir ~extensions] creates a watcher for files in [dir]
    with the given extensions (e.g., [".lina"]). *)
val create : dir:string -> extensions:string list -> t

(** {1 Watching} *)

(** [get_current_state watcher] returns the current state of all watched files. *)
val get_current_state : t -> file_state list

(** [detect_changes watcher ~previous] compares current state with previous
    and returns list of changed files. *)
val detect_changes : t -> previous:file_state list -> string list

(** [wait_for_changes watcher ~previous ~poll_interval_ms] blocks until
    changes are detected, returning list of changed files. *)
val wait_for_changes : t -> previous:file_state list -> poll_interval_ms:int -> string list

(** {1 Utilities} *)

(** [clear_screen ()] clears the terminal screen. *)
val clear_screen : unit -> unit
