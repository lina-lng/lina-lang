(** Lina LSP server entry point.

    This is the main entry point for the linac-lsp binary, which provides
    Language Server Protocol support for Lina. *)

let run () =
  Eio_main.run @@ fun env ->
  let lsp_server = new Lina_lsp.Server.lina_server in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env (lsp_server :> Linol_eio.Jsonrpc2.server) in
  let shutdown () = lsp_server#get_status = `ReceivedExit in
  Linol_eio.Jsonrpc2.run ~shutdown server

let () =
  match run () with
  | () -> ()
  | exception exn ->
      let msg = Printexc.to_string exn in
      Printf.eprintf "linac-lsp error: %s\n%!" msg;
      exit 1
