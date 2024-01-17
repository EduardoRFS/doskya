open Eio
open Piaf

module Route = struct
  module Path = struct
    open Routes

    type path = Wiki of { page : string }

    let wiki () = s "wiki" / str /? nil
    let router = Routes.(one_of [ route (wiki ()) (fun page -> Wiki { page }) ])

    let _to_string path =
      match path with Wiki { page } -> sprintf (wiki ()) page

    let of_string string =
      match match' router ~target:string with
      | NoMatch -> None
      (* TODO: trailing? *)
      (* TODO: better than not found error message when trailing *)
      | MatchWithTrailingSlash _path -> None
      | FullMatch path -> Some path
  end
end

let not_found () =
  let body = Body.of_string "Not found" in
  Response.create ~body `Not_found

let invalid_method () =
  let body = Body.of_string "Invalid method" in
  Response.create ~body `Method_not_allowed

let server_error exn =
  let message = Format.sprintf "Server error: %s" (Printexc.to_string exn) in
  let body = Body.of_string message in
  Response.create ~body `Internal_server_error

let on_wiki ~cwd ~page =
  (* TODO: better path handling, security *)
  let markdown = Path.(load @@ (cwd / "wiki" / (page ^ ".md"))) in
  let markdown = Omd.of_string markdown in
  let body = Body.of_string @@ Omd.to_html markdown in
  Response.create ~body `OK

let main () =
  (* let () = Logs_threaded.enable () in *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  let config =
    let interface = Eio.Net.Ipaddr.V4.any in
    let port = 3000 in
    Piaf.Server.Config.create (`Tcp (interface, port))
  in
  let server =
    Server.create ~config @@ fun { ctx = _; request } ->
    try
      (* TODO: use ctx *)
      (* TODO: headers? Maybe content-type? *)
      (* TODO: body? Drain? *)
      let Request.
            { meth; target; version = _; headers = _; scheme = _; body = _ } =
        request
      in
      match meth with
      | `GET -> (
          match Route.Path.of_string target with
          | Some (Wiki { page }) -> on_wiki ~cwd ~page
          | None -> not_found ())
      | `Other _ | `PUT | `OPTIONS | `CONNECT | `TRACE | `DELETE | `HEAD | `POST
        ->
          invalid_method ()
    with exn -> server_error exn
  in
  let _command = Server.Command.start ~sw env server in
  ()

let () = main ()