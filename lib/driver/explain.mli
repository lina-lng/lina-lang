(** Error code explanation system.

    Provides detailed explanations for error and warning codes,
    including examples of erroneous code, common causes, and how to fix. *)

(** [get_explanation code] returns a detailed explanation for the given error code,
    or [None] if no explanation is available. *)
val get_explanation : Common.Error_code.t -> string option

(** [get_explanation_by_string code_str] parses the code string (e.g., "E0001")
    and returns an explanation, or [None] if the code is invalid or has no explanation. *)
val get_explanation_by_string : string -> string option

(** [format_explanation ~color code explanation] formats an explanation
    for terminal display with optional color support. *)
val format_explanation : color:bool -> Common.Error_code.t -> string -> string
