type position = {
  filename : string;
  line : int;
  column : int;
  offset : int;
}
[@@deriving show, eq, ord]

type t = {
  start_pos : position;
  end_pos : position;
}
[@@deriving show, eq, ord]

let none =
  let pos = { filename = ""; line = 0; column = 0; offset = 0 } in
  { start_pos = pos; end_pos = pos }

let is_none loc = loc.start_pos.line = 0 && loc.end_pos.line = 0

let merge loc1 loc2 =
  if is_none loc1 then loc2
  else if is_none loc2 then loc1
  else { start_pos = loc1.start_pos; end_pos = loc2.end_pos }

let from_lexing_positions (start_p : Lexing.position) (end_p : Lexing.position) =
  {
    start_pos = {
      filename = start_p.pos_fname;
      line = start_p.pos_lnum;
      column = start_p.pos_cnum - start_p.pos_bol;
      offset = start_p.pos_cnum;
    };
    end_pos = {
      filename = end_p.pos_fname;
      line = end_p.pos_lnum;
      column = end_p.pos_cnum - end_p.pos_bol;
      offset = end_p.pos_cnum;
    };
  }

type 'a located = {
  value : 'a;
  location : t;
}
[@@deriving show, eq]

let locate value location = { value; location }
let map_located f loc = { value = f loc.value; location = loc.location }
let dummy_located value = { value; location = none }
