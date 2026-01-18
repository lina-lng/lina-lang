(** Name suggestions using edit distance.

    This module provides functionality for suggesting similar names when a user
    makes a typo or references an undefined identifier. Uses Levenshtein edit
    distance with a tiered threshold based on name length:
    - 1-4 chars: threshold 1
    - 5-6 chars: threshold 2
    - 7+ chars: threshold 3 *)

(** {1 Edit Distance} *)

val edit_distance : string -> string -> int
(** [edit_distance s1 s2] computes the Levenshtein edit distance between
    two strings. This counts the minimum number of single-character edits
    (insertions, deletions, or substitutions) needed to transform [s1] into [s2].

    @return The edit distance, ranging from 0 (identical) to max(len(s1), len(s2)) *)

(** {1 Threshold Calculation} *)

val suggestion_threshold : string -> int
(** [suggestion_threshold target] returns the maximum edit distance for
    suggesting alternatives to [target].

    Uses a tiered approach:
    - 1-4 chars: threshold 1
    - 5-6 chars: threshold 2
    - 7+ chars: threshold 3 *)

val within_threshold : target:string -> candidate:string -> bool
(** [within_threshold ~target ~candidate] returns true if [candidate] is
    within the suggestion threshold for [target].

    This is equivalent to:
    [edit_distance target candidate <= suggestion_threshold target] *)

(** {1 Finding Suggestions} *)

val find_similar : target:string -> candidates:string list -> (string * int) list
(** [find_similar ~target ~candidates] finds all candidates within the
    suggestion threshold, sorted by edit distance (closest first).

    @param target The misspelled or unknown name
    @param candidates List of valid names to compare against
    @return List of (name, distance) pairs, sorted by distance ascending *)

val find_closest : target:string -> candidates:string list -> string option
(** [find_closest ~target ~candidates] returns the single closest match
    if one exists within the threshold.

    @return [Some name] for the closest match, or [None] if no suggestions *)

(** {1 Formatting} *)

val did_you_mean : target:string -> candidates:string list -> string option
(** [did_you_mean ~target ~candidates] formats a "Did you mean?" message
    for the closest match.

    @return [Some "Did you mean `closest`?"] or [None] if no suggestions *)

val format_suggestions : ?max_suggestions:int -> target:string -> candidates:string list -> unit -> string
(** [format_suggestions ~target ~candidates] formats multiple suggestions
    as a bulleted list.

    @param max_suggestions Maximum number to show (default: 3)
    @return Formatted string like "Did you mean one of these?\n  - foo\n  - fob"
            or empty string if no suggestions *)
