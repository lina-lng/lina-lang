(** FFI binding generator for LuaRocks packages. *)

let binding_module_name package_name =
  let stripped =
    if String.length package_name > 3 && String.sub package_name 0 3 = "lua"
    then String.sub package_name 3 (String.length package_name - 3)
    else package_name
  in
  String.capitalize_ascii stripped

let binding_filename package_name =
  binding_module_name package_name ^ ".lina"

type generation_result = {
  generated : string list;
  failed : (string * string) list;
}
[@@deriving show]

(** Built-in templates for common LuaRocks packages. *)
let templates = [

  ("lpeg", {|-- Auto-generated bindings for lpeg
-- LuaRocks package: lpeg

type pattern

module Lpeg = struct
  @module("lpeg")
  external p : string -> pattern = "P"

  @module("lpeg")
  external s : string -> pattern = "S"

  @module("lpeg")
  external r : string -> pattern = "R"

  @module("lpeg")
  external match_ : pattern -> string -> int option = "match"

  @module("lpeg")
  external c : pattern -> pattern = "C"

  @module("lpeg")
  external ct : pattern -> pattern = "Ct"
end
|});

  ("luasocket", {|-- Auto-generated bindings for luasocket
-- LuaRocks package: luasocket

type tcp_socket
type udp_socket

module Socket = struct
  module Tcp = struct
    @module("socket")
    external connect : string -> int -> tcp_socket = "connect"

    @send
    external send : tcp_socket -> string -> int = "send"

    @send @return(nullable)
    external receive : tcp_socket -> string option = "receive"

    @send
    external close : tcp_socket -> unit = "close"
  end

  module Udp = struct
    @module("socket")
    external create : unit -> udp_socket = "udp"

    @send
    external sendto : udp_socket -> string -> string -> int -> int = "sendto"

    @send @return(nullable)
    external receive : udp_socket -> string option = "receive"

    @send
    external close : udp_socket -> unit = "close"
  end
end
|});

  ("luafilesystem", {|-- Auto-generated bindings for luafilesystem
-- LuaRocks package: luafilesystem

module Lfs = struct
  @module("lfs")
  external currentdir : unit -> string = "currentdir"

  @module("lfs")
  external chdir : string -> bool = "chdir"

  @module("lfs")
  external mkdir : string -> bool = "mkdir"

  @module("lfs")
  external rmdir : string -> bool = "rmdir"

  @module("lfs") @return(nullable)
  external attributes : string -> string -> string option = "attributes"
end
|});

  ("penlight", {|-- Auto-generated bindings for penlight
-- LuaRocks package: penlight

module Pl = struct
  module Utils = struct
    @module("pl.utils")
    external split : string -> string -> string list = "split"

    @module("pl.utils")
    external printf : string -> unit = "printf"
  end

  module Path = struct
    @module("pl.path")
    external exists : string -> bool = "exists"

    @module("pl.path")
    external isdir : string -> bool = "isdir"

    @module("pl.path")
    external isfile : string -> bool = "isfile"

    @module("pl.path")
    external join : string -> string -> string = "join"

    @module("pl.path")
    external basename : string -> string = "basename"

    @module("pl.path")
    external dirname : string -> string = "dirname"
  end
end
|});

  ("cjson", {|-- Auto-generated bindings for lua-cjson
-- LuaRocks package: lua-cjson

type json_value

module Cjson = struct
  @module("cjson")
  external encode : 'a -> string = "encode"

  @module("cjson") @return(nullable)
  external decode : string -> json_value option = "decode"
end
|});

]

let has_template package_name =
  List.exists (fun (name, _) -> name = package_name) templates

let template_for package_name =
  List.assoc_opt package_name templates

(** Generate a minimal stub for packages without templates. *)
let generate_stub package_name =
  let module_name = binding_module_name package_name in
  Printf.sprintf {|-- Auto-generated stub bindings for %s
-- LuaRocks package: %s
--
-- This is a minimal stub. Add type declarations as needed.
-- See the Lina documentation for FFI binding syntax.

module %s = struct
  -- Add FFI declarations here
  -- Example:
  -- @module("%s")
  -- external some_function : string -> int = "some_function"
end
|} package_name package_name module_name package_name

let generate ~package_name ~output_dir () =
  let content = match template_for package_name with
    | Some template -> template
    | None -> generate_stub package_name
  in
  let filename = binding_filename package_name in
  let path = Filename.concat output_dir filename in

  try
    Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc content
    );
    Ok path
  with
  | Sys_error msg -> Error (Printf.sprintf "Cannot write %s: %s" path msg)

let generate_all ~(lockfile : Lockfile.t) ~output_dir () =
  let rec process_entries generated failed = function
    | [] -> { generated = List.rev generated; failed = List.rev failed }
    | entry :: rest ->
        match generate ~package_name:entry.Lockfile.name ~output_dir () with
        | Ok path -> process_entries (path :: generated) failed rest
        | Error msg -> process_entries generated ((entry.name, msg) :: failed) rest
  in
  process_entries [] [] lockfile.packages
