open Common

type format =
  | Human
  | Short
  | Json

type color_choice =
  | Auto
  | Always
  | Never

type terminal_config = {
  use_color : bool;
  width : int;
  use_unicode : bool;
}

(* ANSI color codes *)
module Color = struct
  let reset = "\027[0m"
  let bold = "\027[1m"
  let yellow = "\027[33m"
  let blue = "\027[34m"
  let cyan = "\027[36m"
  let bright_red = "\027[91m"
  let bright_blue = "\027[94m"
  let bright_cyan = "\027[96m"

  let apply use_color codes text =
    if use_color then
      String.concat "" codes ^ text ^ reset
    else
      text

  let error use_color text = apply use_color [bold; bright_red] text
  let warning use_color text = apply use_color [bold; yellow] text
  let info use_color text = apply use_color [bold; bright_blue] text
  let hint use_color text = apply use_color [bold; bright_cyan] text
  let location use_color text = apply use_color [bold; blue] text
  let line_number use_color text = apply use_color [bright_blue] text
  let code use_color text = apply use_color [bold] text
  let note_label use_color text = apply use_color [bold; cyan] text
end

let detect_terminal color_choice =
  let no_color = Sys.getenv_opt "NO_COLOR" |> Option.is_some in
  let force_color = Sys.getenv_opt "FORCE_COLOR" |> Option.is_some in
  let is_tty = Unix.isatty Unix.stdout in
  let use_color = match color_choice with
    | Always -> true
    | Never -> false
    | Auto ->
      if no_color then false
      else if force_color then true
      else is_tty
  in
  let width =
    try
      let term_width = Sys.getenv "COLUMNS" |> int_of_string in
      max 40 (min 200 term_width)
    with _ -> 80
  in
  { use_color; width; use_unicode = true }

(* Source file cache *)
type source_cache = (string, string array) Hashtbl.t

let create_source_cache () : source_cache = Hashtbl.create 16

let add_source (cache : source_cache) ~path ~content =
  let lines = String.split_on_char '\n' content |> Array.of_list in
  Hashtbl.replace cache path lines

let get_line (cache : source_cache) ~path ~line =
  match Hashtbl.find_opt cache path with
  | None -> None
  | Some lines ->
    if line >= 1 && line <= Array.length lines then
      Some lines.(line - 1)
    else
      None

let load_source_file (cache : source_cache) path =
  try
    let ic = open_in path in
    let content = In_channel.input_all ic in
    close_in ic;
    add_source cache ~path ~content;
    Ok ()
  with
  | Sys_error msg -> Error msg

(* Human-readable rendering *)
module Human = struct
  let severity_prefix terminal severity code =
    let use_color = terminal.use_color in
    let severity_text = match severity with
      | Compiler_error.Error -> Color.error use_color "error"
      | Compiler_error.Warning -> Color.warning use_color "warning"
      | Compiler_error.Info -> Color.info use_color "info"
      | Compiler_error.Hint -> Color.hint use_color "hint"
    in
    match code with
    | Some code ->
      let code_str = Error_code.to_string code in
      Printf.sprintf "%s[%s]" severity_text (Color.code use_color code_str)
    | None -> severity_text

  let render_location terminal (loc : Location.t) =
    if Location.is_none loc then ""
    else
      let use_color = terminal.use_color in
      Printf.sprintf "%s %s:%d:%d"
        (Color.location use_color "-->")
        loc.start_pos.filename
        loc.start_pos.line
        loc.start_pos.column

  let render_context_line terminal gutter_width line_num line_content =
    let use_color = terminal.use_color in
    let line_num_str = string_of_int line_num in
    let line_num_padded =
      String.make (gutter_width - String.length line_num_str - 1) ' ' ^ line_num_str
    in
    let pipe = if terminal.use_unicode then "|" else "|" in
    Printf.sprintf "%s %s %s"
      (Color.line_number use_color line_num_padded)
      (Color.line_number use_color pipe)
      line_content

  let render_source_line terminal sources (label : Compiler_error.label) =
    let loc = label.label_span in
    if Location.is_none loc then []
    else
      let use_color = terminal.use_color in
      let line_num = loc.start_pos.line in
      let line_num_str = string_of_int line_num in
      let gutter_width = max 4 (String.length line_num_str + 1) in
      let gutter_padding = String.make gutter_width ' ' in
      let line_num_padded =
        String.make (gutter_width - String.length line_num_str - 1) ' ' ^ line_num_str
      in

      let pipe = if terminal.use_unicode then "|" else "|" in

      (* Add context line before if available *)
      let context_before =
        if line_num > 1 then
          match get_line sources ~path:loc.start_pos.filename ~line:(line_num - 1) with
          | Some content -> [render_context_line terminal gutter_width (line_num - 1) content]
          | None -> []
        else []
      in

      match get_line sources ~path:loc.start_pos.filename ~line:line_num with
      | None ->
        [Printf.sprintf "%s %s" gutter_padding (Color.line_number use_color pipe)]
      | Some line_content ->
        let start_col = max 0 (loc.start_pos.column - 1) in
        let end_col =
          if loc.start_pos.line = loc.end_pos.line then
            loc.end_pos.column - 1
          else
            String.length line_content
        in
        let underline_len = max 1 (end_col - start_col) in

        let underline_char = if label.is_primary then "^" else "-" in
        let underline = String.make underline_len underline_char.[0] in
        let underline_padding = String.make start_col ' ' in

        let colored_underline =
          if label.is_primary then
            Color.error use_color underline
          else
            Color.info use_color underline
        in

        let main_lines = [
          Printf.sprintf "%s %s"
            gutter_padding
            (Color.line_number use_color pipe);
          Printf.sprintf "%s %s %s"
            (Color.line_number use_color line_num_padded)
            (Color.line_number use_color pipe)
            line_content;
          Printf.sprintf "%s %s %s%s"
            gutter_padding
            (Color.line_number use_color pipe)
            underline_padding
            colored_underline;
        ] in

        let with_message = match label.label_message with
          | None -> main_lines
          | Some msg ->
            main_lines @ [
              Printf.sprintf "%s %s %s%s"
                gutter_padding
                (Color.line_number use_color pipe)
                underline_padding
                (Color.error use_color msg)
            ]
        in

        context_before @ with_message

  let render_note terminal note =
    let use_color = terminal.use_color in
    Printf.sprintf "%s: %s" (Color.note_label use_color "note") note

  let render_suggestion terminal sources (sugg : Compiler_error.suggestion) =
    let use_color = terminal.use_color in
    let lines = [
      Printf.sprintf "%s: %s" (Color.hint use_color "help") sugg.suggestion_message
    ] in

    (* Show the replacement if it's machine-applicable *)
    if sugg.applicability = Compiler_error.MachineApplicable &&
       sugg.replacement <> "" &&
       not (Location.is_none sugg.suggestion_span) then
      let loc = sugg.suggestion_span in
      match get_line sources ~path:loc.start_pos.filename ~line:loc.start_pos.line with
      | Some original_line ->
        let start_col = max 0 (loc.start_pos.column - 1) in
        let end_col =
          if loc.start_pos.line = loc.end_pos.line then loc.end_pos.column - 1
          else String.length original_line
        in
        let before = String.sub original_line 0 start_col in
        let after =
          if end_col < String.length original_line then
            String.sub original_line end_col (String.length original_line - end_col)
          else ""
        in
        let suggested_line = before ^ sugg.replacement ^ after in
        lines @ [
          Printf.sprintf "     %s %s"
            (Color.line_number use_color "|")
            (Color.info use_color suggested_line)
        ]
      | None -> lines
    else lines

  let render terminal sources (diag : Compiler_error.diagnostic) =
    let buf = Buffer.create 256 in
    let add_line line = Buffer.add_string buf line; Buffer.add_char buf '\n' in

    (* Header: error[E0001]: type mismatch *)
    let header = Printf.sprintf "%s: %s"
      (severity_prefix terminal diag.severity diag.code)
      diag.message
    in
    add_line header;

    (* Location line: --> src/main.lina:23:15 *)
    let primary_label = List.find_opt (fun l -> l.Compiler_error.is_primary) diag.labels in
    (match primary_label with
    | Some label when not (Location.is_none label.label_span) ->
      add_line (render_location terminal label.label_span)
    | _ -> ());

    (* Source snippets for each label *)
    List.iter (fun label ->
      List.iter add_line (render_source_line terminal sources label)
    ) diag.labels;

    (* Notes *)
    List.iter (fun note ->
      add_line "";
      add_line (render_note terminal note)
    ) diag.notes;

    (* Suggestions with fix preview *)
    List.iter (fun sugg ->
      add_line "";
      List.iter add_line (render_suggestion terminal sources sugg)
    ) diag.suggestions;

    Buffer.contents buf
end

(* Short format: one line per diagnostic *)
module Short = struct
  let render (diag : Compiler_error.diagnostic) =
    let severity = match diag.severity with
      | Compiler_error.Error -> "error"
      | Compiler_error.Warning -> "warning"
      | Compiler_error.Info -> "info"
      | Compiler_error.Hint -> "hint"
    in
    let code_str = match diag.code with
      | Some code -> Printf.sprintf "[%s] " (Error_code.to_string code)
      | None -> ""
    in
    let loc_str =
      match List.find_opt (fun l -> l.Compiler_error.is_primary) diag.labels with
      | Some label when not (Location.is_none label.label_span) ->
        let loc = label.label_span in
        Printf.sprintf "%s:%d:%d: " loc.start_pos.filename loc.start_pos.line loc.start_pos.column
      | _ -> ""
    in
    Printf.sprintf "%s%s: %s%s" loc_str severity code_str diag.message
end

(* JSON format *)
module Json = struct
  let applicability_to_string = function
    | Compiler_error.MachineApplicable -> "MachineApplicable"
    | Compiler_error.MaybeIncorrect -> "MaybeIncorrect"
    | Compiler_error.HasPlaceholders -> "HasPlaceholders"
    | Compiler_error.Unspecified -> "Unspecified"

  let severity_to_string = function
    | Compiler_error.Error -> "error"
    | Compiler_error.Warning -> "warning"
    | Compiler_error.Info -> "info"
    | Compiler_error.Hint -> "hint"

  let location_to_json (loc : Location.t) =
    if Location.is_none loc then `Null
    else
      `Assoc [
        "file", `String loc.start_pos.filename;
        "line_start", `Int loc.start_pos.line;
        "line_end", `Int loc.end_pos.line;
        "column_start", `Int loc.start_pos.column;
        "column_end", `Int loc.end_pos.column;
        "byte_start", `Int loc.start_pos.offset;
        "byte_end", `Int loc.end_pos.offset;
      ]

  let label_to_json (label : Compiler_error.label) =
    `Assoc [
      "span", location_to_json label.label_span;
      "message", (match label.label_message with Some m -> `String m | None -> `Null);
      "is_primary", `Bool label.is_primary;
    ]

  let suggestion_to_json (sugg : Compiler_error.suggestion) =
    `Assoc [
      "message", `String sugg.suggestion_message;
      "replacement", `String sugg.replacement;
      "span", location_to_json sugg.suggestion_span;
      "applicability", `String (applicability_to_string sugg.applicability);
    ]

  let diagnostic_to_json (diag : Compiler_error.diagnostic) =
    `Assoc [
      "type", `String "diagnostic";
      "level", `String (severity_to_string diag.severity);
      "code", (match diag.code with
        | Some code -> `Assoc ["code", `String (Error_code.to_string code)]
        | None -> `Null);
      "message", `String diag.message;
      "spans", `List (List.map label_to_json diag.labels);
      "notes", `List (List.map (fun n -> `String n) diag.notes);
      "suggestions", `List (List.map suggestion_to_json diag.suggestions);
    ]

  let render diag =
    Yojson.Safe.to_string (diagnostic_to_json diag)

  let render_list diags =
    let json = `List (List.map diagnostic_to_json diags) in
    Yojson.Safe.to_string json
end

(* Main rendering functions *)
let render_diagnostic ~format ~terminal ~sources diag =
  match format with
  | Human -> Human.render terminal sources diag
  | Short -> Short.render diag
  | Json -> Json.render diag

let render_diagnostics ~format ~terminal ~sources diags =
  match format with
  | Human ->
    let rendered = List.map (Human.render terminal sources) diags in
    let errors = List.filter (fun d -> d.Compiler_error.severity = Compiler_error.Error) diags in
    let warnings = List.filter (fun d -> d.Compiler_error.severity = Compiler_error.Warning) diags in
    let summary =
      if List.length errors > 0 || List.length warnings > 0 then
        Printf.sprintf "\n%s: %d error(s), %d warning(s) emitted\n"
          (if terminal.use_color then Color.bold ^ "Summary" ^ Color.reset else "Summary")
          (List.length errors)
          (List.length warnings)
      else ""
    in
    String.concat "\n" rendered ^ summary
  | Short ->
    String.concat "\n" (List.map Short.render diags)
  | Json ->
    Json.render_list diags

let render_human ~sources diag =
  let terminal = detect_terminal Auto in
  Human.render terminal sources diag

let render_json diag = Json.render diag
let render_json_list diags = Json.render_list diags

let render_error ~sources err =
  let terminal = detect_terminal Auto in
  let diag = Compiler_error.diagnostic_of_error err in
  Human.render terminal sources diag

let render_warning ~sources warn =
  let terminal = detect_terminal Auto in
  let diag = Compiler_error.diagnostic_of_warning warn in
  Human.render terminal sources diag
